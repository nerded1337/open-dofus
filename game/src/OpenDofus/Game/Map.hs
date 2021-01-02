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
    runMaps,
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

type HashTable k v = H.BasicHashTable k v

createMapInstance ::
  (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m) =>
  Map ->
  m (Either Text MapInstance)
createMapInstance m = case parseMap m of
  Right parsedMap -> do
    gfxLoadedMap <-
      fmap getCompose $
        traverse
          (runVolatile @GameDbConn . getInteractiveObjectByGfxId)
          $ Compose parsedMap
    pure $
      Right $
        Compose . fmap join . getCompose
          <$> gfxLoadedMap
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

runMaps ::
  MonadIO m =>
  HashTable MapId MapEventChannels ->
  Pool AuthDbConn ->
  Pool GameDbConn ->
  (ActorId -> GameMessage -> IO ()) ->
  [MapInstance] ->
  m [MapController]
runMaps mapChannels auhPooq gamePool dispatch m = do
  liftIO $ debug $ "Number of maps to run: " <> showText (length m)
  traverse (runController <=< createMapController auhPooq gamePool dispatch) m
  where
    runController mc = liftIO $ do
      let mid = mc ^. mapControllerInstance . mapInstanceTemplate . mapId
      newChans <- uncurry MapEventChannels <$> U.newChan
      H.insert mapChannels mid newChans
      void $ forkIO $ runMapWorker mc (newChans ^. mapEventChannelsOut)
      pure mc

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
      eventStream' <- case events of
        Next event eventStreamNext -> do
          broadcastMsgs :<*>: MessageMap targetedMsgs <-
            execWriterT $
              runReaderT applyMapEvent $
                MapHandlerInput
                  ctl
                  (beginUpdateTime -:- lastUpdate)
                  event
          dispatchToActors ctl broadcastMsgs
          HM.foldMapWithKey (dispatchToActor ctl) targetedMsgs
          pure eventStreamNext
        Pending -> do
          pure eventStream
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
        else gameDelay $ maximumUpdateTime -:- updateTime
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
  tell $ mempty :<*>: MessageMap (HM.singleton a msg)
{-# INLINE emitActor #-}

emitBroadcast :: GameMessage -> MapHandler ()
emitBroadcast msg = do
  tell $ msg :<*>: mempty
{-# INLINE emitBroadcast #-}

applyMapEvent :: MapHandler ()
applyMapEvent = go =<< view mapHandlerInputEvent
  where
    go :: MapEvent -> MapHandler ()
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
      pure ()

updateActor ::
  ActorId ->
  (ActorState -> Actor -> MapHandler (ActorState :<*>: Actor)) ->
  MapHandler ()
updateActor aid f = do
  actors <- view $ mapHandlerInputCtl . mapControllerActors
  a <- liftIO $ H.lookup actors aid
  case a of
    Just foundActor -> do
      let currentState = foundActor ^. gameActorState
      nextState :<*>: nextActor <- f currentState foundActor
      void $
        liftIO $
          H.insert
            actors
            aid
            (nextActor & gameActorState .~ nextState)
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

-- liftIO $
--   H.mutateIO actors aid $ \a -> do
--     -- fire the event only if the actor is present
--     traverse_ (const $ emitBroadcast [MapActorDespawn [aid]]) a
--     pure (a, ())

onDispatchInformations :: ActorId -> MapHandler ()
onDispatchInformations aid =
  emitActor aid . MapActorSpawn =<< readActors

onActionStart ::
  MapEventActorAction ->
  ActorState ->
  Actor ->
  MapHandler (ActorState :<*>: Actor)
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
            ActorDoing (ActorActionMoving path duration)
              :<*>: a
        Nothing ->
          pure $ st :<*>: a
    _ -> do
      -- TODO: allow enqueue interactive actions when movement is done
      warn "Non idle actor trying to start an action"
      pure $ st :<*>: a

onActionAck ::
  EffectId ->
  ActorState ->
  Actor ->
  MapHandler (ActorState :<*>: Actor)
onActionAck eid st a =
  case st :<*>: eid of
    (ActorDoing (ActorActionMoving path duration) :<*>: EffectActionMapMovement) -> do
      case lastOf folded (path ^. movementPathSteps) of
        Just finalStep -> do
          currentTime <- gameCurrentTime
          -- TODO: properly log cheating attemp
          when
            (currentTime < duration)
            ( warn $
                "Possibly speed hacking: "
                  <> showText (toNum @Millisecond @Natural $ duration -:- currentTime)
            )
          pure
            ( ActorIdle
                :<*>: ( a & actorLocation . actorLocationCellId .~ (finalStep ^. movementStepPoint)
                          & direction .~ finalStep ^. movementStepDirection
                      )
            )
        Nothing -> do
          warn $ "The impossible happenned, empty movement path: " <> showText path
          pure $ st :<*>: a
    _ ->
      pure $ st :<*>: a

onActionAbort ::
  CellId ->
  ActorState ->
  Actor ->
  MapHandler (ActorState :<*>: Actor)
onActionAbort c st a = do
  case st of
    (ActorDoing (ActorActionMoving path _)) -> do
      case elemIndexOf folded c path of
        Just _ ->
          -- TODO: check path duration to stopcell in order to avoid speedhacking
          pure $
            ActorIdle
              :<*>: (a & actorLocation . actorLocationCellId .~ c)
        Nothing ->
          pure $ st :<*>: a
    _ -> do
      warn "Not moving actor trying to abort action"
      pure $ st :<*>: a

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
