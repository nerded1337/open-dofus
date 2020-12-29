{-# LANGUAGE BangPatterns #-}
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
    raiseMapEvent,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi.NoBlocking as U
import Data.ByteString.Builder
import Data.Compact
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import Data.List
import Data.Ratio
import GHC.IO.Unsafe (unsafePerformIO)
import OpenDofus.Core.Application
import OpenDofus.Core.Network.Client
import OpenDofus.Database
import OpenDofus.Game.Map.Action
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Interactive
import OpenDofus.Game.Map.Movement
import OpenDofus.Game.Map.Parser as X
import OpenDofus.Game.Map.Types
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Time hiding (threadDelay)
import OpenDofus.Prelude
import RIO.List (headMaybe)

subAreaChannels ::
  HashTable MapSubAreaId (InChan MapEventDispatch, OutChan MapEventDispatch)
subAreaChannels = unsafePerformIO H.new
{-# NOINLINE subAreaChannels #-}

createMapInstance ::
  (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m) =>
  Map ->
  m (Either Text MapInstance)
createMapInstance m = case parseMap m of
  Right parsedMap -> do
    gfxLoadedMap <- do
      runVolatile @GameDbConn $
        (traverse . traverse . traverse) getInteractiveObjectByGfxId parsedMap
    pure $ Right $ fmap join <$> gfxLoadedMap
  Left err ->
    pure $
      Left $
        "Unable to load map instance: "
          <> showText (m ^. mapId)
          <> ", "
          <> showText err
{-# INLINE createMapInstance #-}

runMaps ::
  MonadIO m =>
  (ActorId -> GameMessage -> IO ()) ->
  [MapInstance] ->
  m [MapController]
runMaps f m = do
  let m' = groupBySubarea m
  liftIO $ debug $ "Number of area to run: " <> showText (length m')
  mapControllers <- (traverse . traverse) (createMapController f) m'
  traverse_ runSubAreaGroup mapControllers
  pure $ join mapControllers
  where
    runSubAreaGroup g =
      maybe
        (const $ pure ())
        (runSubArea . view (mapControllerInstance . to getCompact . mapInstanceTemplate . mapSubAreaId))
        (headMaybe g)
        g

    sameSubArea x y =
      (x ^. mapInstanceTemplate . mapSubAreaId)
        == (y ^. mapInstanceTemplate . mapSubAreaId)
    groupBySubarea = groupBy sameSubArea
{-# INLINE runMaps #-}

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
      ((ioInstanceEntry <$> m) ^. mapInstanceCells . to HM.elems)
  m' <- compact m
  pure $ MapController m' actors ioInstances f
  where
    ioInstanceEntry c = (c ^. cellId, InteractiveObjectInstance)
{-# INLINE createMapController #-}

runSubArea ::
  MonadIO m =>
  MapSubAreaId ->
  [MapController] ->
  m ()
runSubArea saId subAreaMaps = do
  subAreaDispatchTable <-
    liftIO $ H.fromList (mapToDispatchEntry <$> subAreaMaps)
  chans <- liftIO $ H.lookup subAreaChannels saId
  (_, eventChannel) <- case chans of
    Just foundChans ->
      pure foundChans
    Nothing -> do
      debug $ "Creating sub area channels: id: " <> showText saId <> ", maps: " <> showText (length subAreaMaps)
      newChans <- liftIO U.newChan
      liftIO $ H.insert subAreaChannels saId newChans
      pure newChans
  void $ liftIO $ forkIO $ runSubAreaWorker subAreaDispatchTable eventChannel
  where
    mapToDispatchEntry m =
      (m ^. mapControllerInstance . to getCompact . mapInstanceTemplate . mapId, m)
{-# INLINE runSubArea #-}

runSubAreaWorker :: HashTable MapId MapController -> OutChan MapEventDispatch -> IO ()
runSubAreaWorker ctls chan = do
  boxed <- tryReadChan chan
  currentTime <- gameCurrentTime
  updateSubAreaWorker ctls chan currentTime boxed
{-# INLINE runSubAreaWorker #-}

updateSubAreaWorker ::
  HashTable MapId MapController ->
  OutChan MapEventDispatch ->
  GameTime Millisecond ->
  Element MapEventDispatch ->
  IO ()
updateSubAreaWorker !ctls !chan !lastUpdate !boxed = do
  beginUpdateTime <- gameCurrentTime @Millisecond

  event <- tryRead boxed

  boxed' <- case event of
    Just actualEvent -> do
      m <- H.lookup ctls $ actualEvent ^. mapEventDispatchMapId
      case m of
        Just foundMap -> do
          let elapsed = beginUpdateTime -:- lastUpdate
          runReaderT applyMapEvent $
            MapEventArgs
              (actualEvent ^. mapEventDispatchEvent)
              foundMap
              elapsed
        Nothing ->
          -- should be impossible
          warn "Map event dispatch table not found"
      tryReadChan chan
    Nothing -> do
      pure boxed

  endUpdateTime <- gameCurrentTime @Millisecond

  let updateTime = endUpdateTime -:- beginUpdateTime
      maximumUpdateTime = ms (1000 % 64)

  if updateTime > maximumUpdateTime
    then
      debug $
        "Update lagged: "
          <> showText (toNum @Millisecond @Natural updateTime)
    else gameDelay $ maximumUpdateTime -:- updateTime

  updateSubAreaWorker ctls chan beginUpdateTime boxed'
{-# INLINE updateSubAreaWorker #-}

raiseMapEvent :: MonadIO m => Map -> MapEvent -> m ()
raiseMapEvent m e = do
  chans <- liftIO $ H.lookup subAreaChannels (m ^. mapSubAreaId)
  case chans of
    Just (eventChannel, _) ->
      liftIO $ U.writeChan eventChannel $ MapEventDispatch e (m ^. mapId)
    Nothing ->
      warn "Area dispatcher not found"
{-# INLINE raiseMapEvent #-}

readMapInstance :: (MonadIO m, MapEventReader m) => m MapInstance
readMapInstance =
  view (mapEventArgsCtl . mapControllerInstance . to getCompact)
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
  dispatch <- view (mapEventArgsCtl . mapControllerDispatch)
  liftIO $
    H.mapM_
      (flip dispatch (FullySerialized $ foldMap ((<> word8 0) . toNetwork) msgs) . fst)
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
    go (MapEventActorActionAck aid action) = do
      updateActor aid (onActionAck action)
    go (MapEventActorActionAbort aid action cell) = do
      updateActor aid (onActionAbort action cell)
    {-# INLINE go #-}
{-# INLINE applyMapEvent #-}

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
{-# INLINE updateActor #-}

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
    (ActorDoing (ActorActionMoving path duration), GameActionMapMovement) -> do
      mi <- readMapInstance
      let x = last $ toList (path ^. movementPathSteps)
      currentTime <- gameCurrentTime @Millisecond
      when
        (currentTime < duration)
        ( warn $
            "Possibly speed hacking: "
              <> showText (toNum @Millisecond @Natural $ duration -:- currentTime)
        )
      debug $
        "Current cell: "
          <> showText (mi ^. mapInstanceCells . at (x ^. movementStepPoint))
      pure
        ( ActorIdle,
          a & actorLocation . actorLocationCellId .~ (x ^. movementStepPoint)
            & direction .~ x ^. movementStepDirection
        )
    _ ->
      pure (st, a)

onActionAbort ::
  (MonadIO m, MapEventReader m) =>
  EffectId ->
  CellId ->
  ActorState ->
  Actor ->
  m (ActorState, Actor)
onActionAbort eid c st a = do
  case (st, eid) of
    (ActorDoing (ActorActionMoving path _), GameActionMapMovement) -> do
      case elemIndexOf folded c path of
        Just _ ->
          -- TODO: duration to this step
          pure (ActorIdle, a & actorLocation . actorLocationCellId .~ c)
        Nothing ->
          pure (st, a)
    _ -> do
      warn "Not moving actor trying to abort action"
      pure (st, a)

onActionStart ::
  (MonadIO m, MapEventReader m) =>
  EffectId ->
  ActionStartParams ->
  ActorState ->
  Actor ->
  m (ActorState, Actor)
onActionStart eid params st a = do
  case (st, eid) of
    (ActorIdle, GameActionMapMovement) -> do
      movementResult <- onMapMovementStart a params
      case movementResult of
        Just (path, duration) -> do
          pure (ActorDoing $ ActorActionMoving path duration, a)
        Nothing ->
          pure (st, a)
    _ -> do
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
            -- don't count start cell
            length (validPath ^. movementPathSteps) - 1
          duration =
            movementDuration
              (m ^. mapWidth)
              -- TODO: handle mount
              ( if len >= 3
                  then MovementModifierRun
                  else MovementModifierWalk
              )
              validPath
      debug $ "Movement: " <> showText validPath
      debug $
        "Movement len: "
          <> showText len
          <> ", duration: "
          <> showText duration
      currentTime <- gameCurrentTime
      dispatchToMap
        [ GameAction
            True
            GameActionMapMovement
            (a ^. to actorId)
            (validPath ^. movementPathCompressed)
        ]
      -- allow ~10% speedhack
      let extraDelayModifier :: Ratio Natural
          extraDelayModifier = 0.9
      pure $ Just (validPath, (extraDelayModifier *:* duration) +:+ currentTime)
    Nothing -> do
      warn "Could not decompress path"
      pure $ Nothing
