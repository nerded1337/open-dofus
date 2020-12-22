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
import qualified Data.HashTable.IO as H
import Data.List
import Data.Ratio
import GHC.IO.Unsafe (unsafePerformIO)
import OpenDofus.Core.Application
import OpenDofus.Database
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Interactive
import OpenDofus.Game.Map.Parser as X
import OpenDofus.Game.Map.Types
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Time hiding (threadDelay)
import OpenDofus.Prelude
import RIO.List (headMaybe)

subAreaChannels
  :: HashTable MapSubAreaId (InChan MapEventDispatch, OutChan MapEventDispatch)
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
        traverse (traverse getInteractiveObjectByGfxId) parsedMap
    pure $ Right $ fmap InteractiveObjectInstance . join <$> gfxLoadedMap
  Left err ->
    pure $
      Left $
        "Unable to load map instance: "
          <> showText (m ^. mapId)
          <> ", "
          <> showText err

runMaps ::
  MonadIO m =>
  (ActorId -> [GameMessage] -> IO ()) ->
  [MapInstance] ->
  m [MapController]
runMaps f m = do
  let !m' = groupBySubarea m
  liftIO $ debug $ "Number of area to run: " <> showText (length m')
  mapControllers <- (traverse . traverse) (createMapController f) m'
  traverse_ runSubAreaGroup mapControllers
  pure $ join mapControllers
  where
    runSubAreaGroup g =
      maybe
        (const $ pure ())
        (runSubArea . view (mapControllerInstance . mapInstanceTemplate . mapSubAreaId))
        (headMaybe g)
        g

    sameSubArea x y =
      (x ^. mapInstanceTemplate . mapSubAreaId)
        == (y ^. mapInstanceTemplate . mapSubAreaId)
    groupBySubarea = groupBy sameSubArea

createMapController ::
  MonadIO m =>
  (ActorId -> [GameMessage] -> IO ()) ->
  MapInstance ->
  m MapController
createMapController f m = liftIO $ do
  actors <- H.new
  pure $ MapController m actors f

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
  void $ liftIO $ forkIO $ updateSubArea subAreaDispatchTable eventChannel
  where
    mapToDispatchEntry m =
      (m ^. mapControllerInstance . mapInstanceTemplate . mapId, m)

updateSubArea :: HashTable MapId MapController -> OutChan MapEventDispatch -> IO ()
updateSubArea ctls chan = go =<< next
  where
    next :: IO (Element MapEventDispatch)
    next = tryReadChan chan

    go :: Element MapEventDispatch -> IO ()
    go box = do
      e <- tryRead box
      case e of
        Just e' -> do
          debug $
            "Processing map event: mapId="
              <> showText (e' ^. mapEventDispatchMapId)
          m <- H.lookup ctls (e' ^. mapEventDispatchMapId)
          case m of
            Just foundMap ->
              runReaderT applyMapEvent $ MapEventArgs (e' ^. mapEventDispatchEvent) foundMap
            Nothing ->
              warn "Map event dispatch table not found"
          go =<< next
        Nothing -> do
          gameDelay $ ms (1000 % 20)
          go box

raiseMapEvent :: MonadIO m => Map -> MapEvent -> m ()
raiseMapEvent m e = do
  chans <- liftIO $ H.lookup subAreaChannels (m ^. mapSubAreaId)
  case chans of
    Just (eventChannel, _) ->
      liftIO $ U.writeChan eventChannel $ MapEventDispatch e (m ^. mapId)
    Nothing ->
      warn "Area dispatcher not found"
{-# INLINE raiseMapEvent #-}

readActors ::
  (MonadIO m, MapEventReader m) =>
  m [GameActor]
readActors = extract =<< actors
  where
    extract = liftIO . (fmap . fmap) snd . H.toList
    actors = view (mapEventArgsCtl . mapControllerActors)
{-# INLINE readActors #-}

dispatchToActor ::
  (MonadIO m, MapEventReader m) =>
  ActorId ->
  [GameMessage] ->
  m ()
dispatchToActor actor msgs = do
  dispatch <- view (mapEventArgsCtl . mapControllerDispatch)
  liftIO $ dispatch actor msgs
{-# INLINE dispatchToActor #-}

dispatchToMap :: (MonadIO m, MapEventReader m) => [GameMessage] -> m ()
dispatchToMap msgs = do
  ctl <- view mapEventArgsCtl
  dispatch <- view (mapEventArgsCtl . mapControllerDispatch)
  -- TODO: serialize before dispatching
  liftIO $ H.mapM_ (flip dispatch msgs . fst) $ ctl ^. mapControllerActors
{-# INLINE dispatchToMap #-}

applyMapEvent :: (MonadIO m, MapEventReader m) => m ()
applyMapEvent = go =<< asks (view mapEventArgsEvent)
  where
    go (MapEventActorSpawn actor) = do
      dispatchToMap [MapActorSpawn $ pure actor]
      actors <- view (mapEventArgsCtl . mapControllerActors)
      liftIO $ H.insert actors (actor ^. to actorId) actor
    go (MapEventActorDespawn actor) = do
      actors <- view (mapEventArgsCtl . mapControllerActors)
      liftIO $ H.delete actors (actor ^. to actorId)
      dispatchToMap [MapActorDespawn $ pure actor]
    go (MapEventDispatchInformations actor) = do
      actors <- readActors
      dispatchToActor (actor ^. to actorId) [MapActorSpawn actors]
