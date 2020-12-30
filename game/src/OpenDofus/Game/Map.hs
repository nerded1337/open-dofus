{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Control.Concurrent.Chan.Unagi.NoBlocking as U
import Data.ByteString.Builder
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import Data.List
import Data.Ratio
import OpenDofus.Core.Application
import OpenDofus.Core.Network.Client
import OpenDofus.Database
import OpenDofus.Game.Map.Action
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Interactive
import OpenDofus.Game.Map.Parser as X
import OpenDofus.Game.Map.Types
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Time hiding (threadDelay)
import OpenDofus.Prelude

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
  (ActorId -> GameMessage -> IO ()) ->
  MapInstance ->
  m MapController
createMapController f m = liftIO $ do
  actors <- H.new
  ioInstances <-
    H.fromListWithSizeHint
      (HM.size $ m ^. mapInstanceCells)
      ((ioInstanceEntry . getCompose <$> m) ^. mapInstanceCells . to HM.elems)
  pure $ MapController m actors ioInstances f
  where
    ioInstanceEntry c = (c ^. cellId, InteractiveObjectInstance)

runMaps ::
  MonadIO m =>
  HashTable MapId MapEventChannels ->
  (ActorId -> GameMessage -> IO ()) ->
  [MapInstance] ->
  m [MapController]
runMaps mapChannels f m = do
  liftIO $ debug $ "Number of maps to run: " <> showText (length m)
  traverse (runController <=< createMapController f) m
  where
    runController mc = liftIO $ do
      let mid = mc ^. mapControllerInstance . mapInstanceTemplate . mapId
      newChans <- uncurry MapEventChannels <$> U.newChan
      H.insert mapChannels mid newChans
      void $ forkIO $ runMapWorker mc (newChans ^. mapEventChannelsOut)
      pure mc

runMapWorker :: MapController -> OutChan MapEvent -> IO ()
runMapWorker m s = do
  currentTime <- gameCurrentTime @Millisecond
  [stream] <- streamChan 1 s
  go currentTime stream
  where
    go :: GameTime Millisecond -> Stream MapEvent -> IO ()
    go lastUpdate eventStream = do
      beginUpdateTime <- gameCurrentTime @Millisecond
      events <- tryReadNext eventStream
      eventStream' <- case events of
        Next event eventStreamNext -> do
          runReaderT applyMapEvent $
            MapEventArgs
              m
              (beginUpdateTime -:- lastUpdate)
              event
          pure eventStreamNext
        Pending -> do
          pure eventStream
      hasPlayer <-
        runReaderT
          (anyOf folded isPlayerActor <$> readActors)
          (MapEventArgs m (ms 0) MapEventNoOperation)
      endUpdateTime <- gameCurrentTime @Millisecond
      let updateTime = endUpdateTime -:- beginUpdateTime
          -- ticks per seconds
          maximumUpdateTime =
            ms (1000 % if hasPlayer
                         then 128
                         else 2)
      if updateTime > maximumUpdateTime
        then
          debug $
            "Update lagged: "
              <> showText (toNum @Millisecond @Natural updateTime)
              <> "ms"
        else gameDelay $ maximumUpdateTime -:- updateTime
      go beginUpdateTime eventStream'

readMapInstance :: (MonadIO m, MapEventReader m) => m MapInstance
readMapInstance =
  view (mapEventArgsCtl . mapControllerInstance)
{-# INLINE readMapInstance #-}

readMapTemplate :: (MonadIO m, MapEventReader m) => m Map
readMapTemplate =
  view mapInstanceTemplate <$> readMapInstance
{-# INLINE readMapTemplate #-}

readActor :: (MonadIO m, MapEventReader m) => ActorId -> m (Maybe Actor)
readActor aid = extract =<< actors
  where
    extract = liftIO . flip H.lookup aid
    actors = view (mapEventArgsCtl . mapControllerActors)
{-# INLINE readActor #-}

readActors :: (MonadIO m, MapEventReader m) => m [Actor]
readActors = extract =<< actors
  where
    extract = liftIO . (fmap . fmap) snd . H.toList
    actors = view (mapEventArgsCtl . mapControllerActors)
{-# INLINE readActors #-}

dispatchToActor ::
  (MonadIO m, MapEventReader m) =>
  ActorId ->
  GameMessage ->
  m ()
dispatchToActor a msg = do
  dispatch <- view (mapEventArgsCtl . mapControllerDispatch)
  liftIO $ dispatch a msg
{-# INLINE dispatchToActor #-}

dispatchToMap :: (MonadIO m, MapEventReader m) => [GameMessage] -> m ()
dispatchToMap msgs = do
  ctl <- view mapEventArgsCtl
  let serializedMsgs =
        FullySerialized $ foldMap ((<> word8 0) . toNetwork) msgs
  liftIO $
    H.mapM_
      (flip (ctl ^. mapControllerDispatch) serializedMsgs . fst)
      (ctl ^. mapControllerActors)
{-# INLINE dispatchToMap #-}

applyMapEvent :: (MonadIO m, MapEventReader m) => m ()
applyMapEvent = go =<< view mapEventArgsEvent
  where
    go (MapEventActorSpawn a) =
      onActorSpawn a
    go (MapEventActorDespawn aid) =
      onActorDespawn aid
    go (MapEventDispatchInformations aid) =
      onDispatchInformations aid
    go (MapEventActorActionStart aid action params) =
      updateActor aid (onActionStart action params)
    go (MapEventActorActionAck aid action) =
      updateActor aid (onActionAck action)
    go (MapEventActorActionAbort aid cell) =
      updateActor aid (onActionAbort cell)
    go MapEventNoOperation =
      pure ()

updateActor ::
  (MonadIO m, MapEventReader m) =>
  ActorId ->
  (ActorState -> Actor -> m (ActorState, Actor)) ->
  m ()
updateActor aid f = do
  actors <- view (mapEventArgsCtl . mapControllerActors)
  a <- liftIO $ H.lookup actors aid
  case a of
    Just foundActor -> do
      let currentState = foundActor ^. gameActorState
      (nextState, nextActor) <- f currentState foundActor
      void $
        liftIO $
          H.insert
            actors
            aid
            (nextActor & gameActorState .~ nextState)
    Nothing -> do
      warn $ "Trying to update unknown actor: " <> showText aid

onActorSpawn :: (MonadIO m, MapEventReader m) => Actor -> m ()
onActorSpawn a = do
  dispatchToMap [MapActorSpawn $ pure a]
  actors <- view (mapEventArgsCtl . mapControllerActors)
  liftIO $ H.insert actors (a ^. to actorId) a

onActorDespawn :: (MonadIO m, MapEventReader m) => ActorId -> m ()
onActorDespawn aid = do
  actors <- view (mapEventArgsCtl . mapControllerActors)
  liftIO $ H.delete actors aid
  dispatchToMap [MapActorDespawn [aid]]

onDispatchInformations :: (MonadIO m, MapEventReader m) => ActorId -> m ()
onDispatchInformations aid =
  dispatchToActor aid . MapActorSpawn =<< readActors

onActionAck ::
  (MonadIO m, MapEventReader m) =>
  EffectId ->
  ActorState ->
  Actor ->
  m (ActorState, Actor)
onActionAck eid st a =
  case (st, eid) of
    (ActorDoing (ActorActionMoving path duration), EffectActionMapMovement) -> do
      case lastOf folded (path ^. movementPathSteps) of
        Just finalStep -> do
          currentTime <- gameCurrentTime @Millisecond
          -- TODO: properly log cheating attemp
          when
            (currentTime < duration)
            ( warn $
                "Possibly speed hacking: "
                  <> showText (toNum @Millisecond @Natural $ duration -:- currentTime)
            )
          pure
            ( ActorIdle,
              a & actorLocation . actorLocationCellId .~ (finalStep ^. movementStepPoint)
                & direction .~ finalStep ^. movementStepDirection
            )
        Nothing -> do
          warn $ "The impossible happenned, empty movement path: " <> showText path
          pure (st, a)
    _ ->
      pure (st, a)

onActionAbort ::
  (MonadIO m, MapEventReader m) =>
  CellId ->
  ActorState ->
  Actor ->
  m (ActorState, Actor)
onActionAbort c st a = do
  case st of
    (ActorDoing (ActorActionMoving path _)) -> do
      case elemIndexOf folded c path of
        Just _ ->
          -- TODO: check path duration to stopcell in order to avoid speedhacking
          pure (ActorIdle, a & actorLocation . actorLocationCellId .~ c)
        Nothing ->
          pure (st, a)
    _ -> do
      warn "Not moving actor trying to abort action"
      pure (st, a)

onActionStart ::
  (MonadIO m, MapEventReader m) =>
  EffectId ->
  EffectParams ->
  ActorState ->
  Actor ->
  m (ActorState, Actor)
onActionStart eid params st a = do
  case (st, eid) of
    (ActorIdle, EffectActionMapMovement) -> do
      movementResult <- onMapMovementStart a params
      case movementResult of
        Just (path, duration) -> do
          pure (ActorDoing $ ActorActionMoving path duration, a)
        Nothing ->
          pure (st, a)
    _ -> do
      -- TODO: allow enqueue interactive actions when movement is done
      warn "Non idle actor trying to start an action"
      pure (st, a)

onMapMovementStart ::
  (MonadIO m, MapEventReader m) =>
  Actor ->
  CompressedMovementPath ->
  m (Maybe (MovementPath CellId, GameTime Millisecond))
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
              (m ^. mapWidth)
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
      currentTime <- gameCurrentTime @Millisecond
      dispatchToMap
        [ GameAction
            True
            EffectActionMapMovement
            (a ^. to actorId)
            (validPath ^. movementPathCompressed)
        ]
          -- client-side is a bit imprecise, allow ~10% speedhack
      let extraDelayModifier :: Ratio Natural
          extraDelayModifier = 0.95
      pure $ Just (validPath, extraDelayModifier *:* duration +:+ currentTime)
    Nothing -> do
      warn "Could not decompress path"
      pure Nothing
