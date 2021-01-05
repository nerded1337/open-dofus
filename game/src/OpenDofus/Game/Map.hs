{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Map.hs ---

-- Copyright (C) 2020 Nerd Ed

-- Author: Nerd Ed <nerded.nerded@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

module OpenDofus.Game.Map
  ( module X,
    createMapInstance,
    runMap,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi.NoBlocking.Unboxed as U
import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import Data.List
import Data.Ratio
import OpenDofus.Core.Application
import OpenDofus.Core.Data.Record
import OpenDofus.Database
import OpenDofus.Game.Map.Action
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Controller
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Interactive
import OpenDofus.Game.Map.Parser as X
import OpenDofus.Game.Map.Types
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Time hiding (threadDelay)
import OpenDofus.Prelude

createMapInstance ::
  (MonadUnliftIO m, HasConnectPool a GameDbConn, MonadReader a m) =>
  HM.HashMap InteractiveObjectGfxId InteractiveObject ->
  Map ->
  m (Either Text MapInstance)
createMapInstance ios m = case parseMap m of
  Right parsedMap -> do
    pure
      . Right
      . fmap Compose
      . getCompose
      $ ((`HM.lookup` ios) =<<)
        <$> (Compose . fmap getCompose) parsedMap
  Left err ->
    pure $
      Left $
        "Unable to load map instance: "
          <> showText (m ^. mapId)
          <> ", "
          <> showText err

createMapController ::
  MonadIO m =>
  Pool AuthDbConn ->
  Pool GameDbConn ->
  (ActorId -> GameMessage -> IO ()) ->
  MapInstance ->
  m MapController
createMapController authPool gamePool dispatch m = liftIO $ do
  actors <- H.new
  ioInstances <-
    H.fromListWithSizeHint
      (HM.size $ m ^. mapInstanceCells)
      ((ioInstanceEntry . getCompose <$> m) ^. mapInstanceCells . to HM.elems)
  pure $ MapController m actors ioInstances authPool gamePool dispatch
  where
    ioInstanceEntry c = (c ^. cellId, InteractiveObjectInstance)

runMap ::
  MonadIO m =>
  Pool AuthDbConn ->
  Pool GameDbConn ->
  (ActorId -> GameMessage -> IO ()) ->
  MapInstance ->
  m (MapController :<*>: MapEventChannels)
runMap auhPool gamePool dispatch = do
  runController <=< createMapController auhPool gamePool dispatch
  where
    runController mc = liftIO $ do
      newChans <- uncurry MapEventChannels <$> U.newChan
      void $ forkIO $ runMapWorker mc $ newChans ^. mapEventChannelsOut
      pure $ mc :<*>: newChans

runMapWorker :: MapController -> OutChan MapEvent -> IO ()
runMapWorker ctl s = do
  currentTime <- gameCurrentTime
  [stream] <- streamChan 1 s
  go currentTime stream
  where
    go :: GameTime Millisecond -> Stream MapEvent -> IO ()
    go lastUpdate eventStream = do
      beginUpdateTime <- gameCurrentTime
      events <- tryReadNext eventStream
      eventStream' :<*>: consumed <- case events of
        Next event eventStreamNext -> do
          broadcastMsgs :<*>: ToActor targetedMsgs <-
            execWriterT $
              runReaderT (applyMapEvent event) $
                MapHandlerInput
                  ctl
                  (beginUpdateTime -:- lastUpdate)
                  event
          dispatchToActors ctl broadcastMsgs
          HM.foldMapWithKey (dispatchToActor ctl) targetedMsgs
          pure $ eventStreamNext :<*>: True
        Pending -> do
          pure $ eventStream :<*>: False
      hasPlayer <-
        H.foldM
          (\x (_, a) -> pure (x || isPlayerActor a))
          False
          (ctl ^. mapControllerActors)
      endUpdateTime <- gameCurrentTime
      let updateTime = endUpdateTime -:- beginUpdateTime
          -- ticks per seconds
          maximumUpdateTime =
            ms
              ( 1000
                  % if hasPlayer
                    then 128
                    else 1
              )
      if updateTime > maximumUpdateTime
        then
          debug $
            "Update lagged: "
              <> showText (toNum @Millisecond @Natural updateTime)
              <> "ms"
        else
          unless
            consumed
            (gameDelay $ maximumUpdateTime -:- updateTime)
      go beginUpdateTime eventStream'

readMapInstance :: MapHandler MapInstance
readMapInstance =
  view $ mapHandlerInputCtl . mapControllerInstance
{-# INLINE readMapInstance #-}

readMapTemplate :: MapHandler Map
readMapTemplate =
  view mapInstanceTemplate <$> readMapInstance
{-# INLINE readMapTemplate #-}

readActor :: ActorId -> MapHandler (Maybe Actor)
readActor aid = extract =<< actors
  where
    extract = liftIO . flip H.lookup aid
    actors = view $ mapHandlerInputCtl . mapControllerActors
{-# INLINE readActor #-}

readActors :: MapHandler [Actor]
readActors = extract =<< actors
  where
    extract = liftIO . (fmap . fmap) snd . H.toList
    actors = view $ mapHandlerInputCtl . mapControllerActors
{-# INLINE readActors #-}

dispatchToActor :: MapController -> ActorId -> GameMessage -> IO ()
dispatchToActor ctl aid msg = do
  liftIO $ (ctl ^. mapControllerDispatch) aid msg
{-# INLINE dispatchToActor #-}

dispatchToActors :: MapController -> GameMessage -> IO ()
dispatchToActors ctl msg = do
  H.mapM_
    (flip (ctl ^. mapControllerDispatch) msg . fst)
    (ctl ^. mapControllerActors)
{-# INLINE dispatchToActors #-}

emitActor ::
  ActorId ->
  GameMessage ->
  MapHandler ()
emitActor a msg = do
  tell $ mempty :<*>: ToActor (HM.singleton a msg)
{-# INLINE emitActor #-}

emitBroadcast :: GameMessage -> MapHandler ()
emitBroadcast msg = do
  tell $ msg :<*>: mempty
{-# INLINE emitBroadcast #-}

applyMapEvent :: MapEvent -> MapHandler ()
applyMapEvent = go
  where
    go (MapEvent (Match (MapEventActorSpawn (actorType :<*>: aid)))) =
      onActorTypeSpawn actorType aid
    go (MapEvent (Match (MapEventActorDespawn aid))) =
      onActorDespawn aid
    go (MapEvent (Match (MapEventDispatchInformations aid))) =
      onDispatchInformations aid
    go (MapEvent (Match (MapEventActorActionStart (aid :<*>: act)))) =
      updateActor aid $ onActionStart act
    go (MapEvent (Match (MapEventActorActionAck (aid :<*>: action)))) =
      updateActor aid $ onActionAck action
    go (MapEvent (Match (MapEventActorActionAbort (aid :<*>: cell)))) =
      updateActor aid $ onActionAbort cell
    go e = do
      warn $ "Unknow map event: " <> showText e

updateActor ::
  ActorId ->
  (ActorState -> Actor -> MapHandler Actor) ->
  MapHandler ()
updateActor aid f = do
  actors <- view $ mapHandlerInputCtl . mapControllerActors
  a <- liftIO $ H.lookup actors aid
  case a of
    Just foundActor -> do
      let currentState = foundActor ^. gameActorState
      nextActor <- f currentState foundActor
      void $
        liftIO $
          H.insert
            actors
            aid
            nextActor
    Nothing -> do
      warn $ "Trying to update unknown actor: " <> showText aid

onActorTypeSpawn ::
  MapEventActorSpawnType ->
  ActorId ->
  MapHandler ()
onActorTypeSpawn MapEventActorSpawnTypePlayer aid = do
  loadedPc <- loadPlayerCharacter (coerce aid)
  case loadedPc of
    Just pc -> do
      onActorSpawn $
        Actor
          ActorIdle
          ( ActorLocation
              (pc ^. playerCharacterCharacterPosition . characterPositionMapId)
              (pc ^. playerCharacterCharacterPosition . characterPositionCellId)
          )
          SouthEast
          (ActorSpecializationPC pc)
    Nothing -> do
      warn "Could not load player"
onActorTypeSpawn _ _ = do
  pure ()

onActorSpawn :: Actor -> MapHandler ()
onActorSpawn a = do
  emitBroadcast $ MapActorSpawn $ pure a
  actors <- view $ mapHandlerInputCtl . mapControllerActors
  liftIO $ H.insert actors (a ^. to actorId) a
  m <- readMapTemplate
  traverse_
    (emitActor $ a ^. to actorId)
    [ GameCreationSuccess,
      GameDataMap
        (m ^. mapId)
        (m ^. mapCreationDate)
        (fromMaybe (error "The impossible happenned") $ m ^. mapDataKey),
      GameDataSuccess
    ]

onActorDespawn :: ActorId -> MapHandler ()
onActorDespawn aid = do
  actors <- view $ mapHandlerInputCtl . mapControllerActors
  liftIO $ H.delete actors aid
  emitBroadcast $ MapActorDespawn [aid]

onDispatchInformations :: ActorId -> MapHandler ()
onDispatchInformations aid =
  emitActor aid . MapActorSpawn =<< readActors

onActionStart ::
  MapEventActorAction ->
  ActorState ->
  Actor ->
  MapHandler Actor
onActionStart act st a = do
  case (st, act) of
    (ActorIdle, Match (MapEventActorActionStartMovement serializedPath)) -> do
      movementResult <-
        onMapMovementStart
          a
          (deserializeCompressedPath serializedPath)
      case movementResult of
        Just (path :<*>: duration) -> do
          pure $
            a & actorState
              .~ ActorDoing (ActorActionMoving path duration)
        Nothing ->
          pure a
    _ -> do
      -- TODO: allow enqueue interactive actions when movement is done
      warn "Non idle actor trying to start an action"
      pure a

onActionAck ::
  EffectId ->
  ActorState ->
  Actor ->
  MapHandler Actor
onActionAck EffectActionMapMovement (ActorDoing (ActorActionMoving path duration)) a = do
  case lastOf folded (path ^. movementPathSteps) of
    Just finalMovement -> do
      m <- readMapInstance
      let c = m ^. mapInstanceCells . at (finalMovement ^. movementStepPoint)
      debug $ showText $ getCompose <$> c
      currentTime <- gameCurrentTime
      -- TODO: properly log cheating attemp
      when
        (currentTime < duration)
        ( warn $
            "Possibly speed hacking: "
              <> showText (toNum @Millisecond @Natural $ duration -:- currentTime)
        )
      pure
        ( a
            & actorLocation . actorLocationCellId .~ finalMovement ^. movementStepPoint
            & direction .~ finalMovement ^. movementStepDirection
            & actorState .~ ActorIdle
        )
    Nothing -> do
      warn $ "Could not find the latest step in a path: " <> showText path
      pure a
onActionAck eid st a = do
  warn $
    "Unhandled action: " <> showText eid <> ", sate: " <> showText st
  pure a

onActionAbort ::
  CellId ->
  ActorState ->
  Actor ->
  MapHandler Actor
onActionAbort c (ActorDoing (ActorActionMoving path _)) a = do
  case elemIndexOf folded c path of
    Just _ -> do
      -- TODO: check path duration to stopcell in order to avoid speedhacking
      pure
        ( a & actorLocation . actorLocationCellId .~ c
            & actorState .~ ActorIdle
        )
    Nothing -> do
      warn "Actor trying to abort a movement on a cell not included in its initial path"
      pure a
onActionAbort _ _ a = do
  warn "Not moving actor trying to abort action"
  pure a

onMapMovementStart ::
  Actor ->
  CompressedMovementPath ->
  MapHandler (Maybe (MovementPath CellId :<*>: GameTime Millisecond))
onMapMovementStart a path = do
  m <- readMapTemplate
  let fullPath =
        extractFullPath
          (m ^. mapWidth)
          (a ^. actorLocation . actorLocationCellId)
          (a ^. direction)
          path
  case fullPath of
    Just validPath -> do
      let len =
            -- starting cell is part of the movement path
            -- don't count it
            length (validPath ^. movementPathSteps) - 1

          duration =
            -- TODO: handle mount/slide
            movementDuration
              ( if len >= 3
                  then MovementModifierRun
                  else MovementModifierWalk
              )
              validPath
      debug $
        "Movement len: "
          <> showText len
          <> ", duration: "
          <> showText duration
      currentTime <- gameCurrentTime
      emitBroadcast $
        GameAction
          True
          EffectActionMapMovement
          (a ^. to actorId)
          (validPath ^. movementPathCompressed)
      -- client-side is a bit imprecise, allow ~10% speedhack
      let extraDelayModifier :: Ratio Natural
          extraDelayModifier = 0.90
      pure $
        Just
          ( validPath
              :<*>: (extraDelayModifier *:* duration +:+ currentTime)
          )
    Nothing -> do
      warn "Could not decompress path"
      pure Nothing
