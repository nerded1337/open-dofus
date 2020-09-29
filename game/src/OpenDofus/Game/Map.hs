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

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module OpenDofus.Game.Map
  ( module X
  , initializeMap
  , runMap
  , raiseMapEvent
  , dispatchToMap
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan.Unagi.NoBlocking
                                               as U
import           Data.HashMap.Strict           as HM
import           OpenDofus.Database
import           OpenDofus.Game.Map.Parser     as X
import           OpenDofus.Game.Network.Message
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

data MapEventArgs = MapEventArgs
    { _mapEventArgsEvent :: !(MapEvent GameClientController)
    , _mapEventArgsCtl   :: {-# UNPACK #-} !GameMapController
    }

makeClassy ''MapEventArgs

instance Show MapEventArgs where
  show (MapEventArgs event _) = show event

type MapEventReader m = MonadReader MapEventArgs m

{-# INLINE raiseMapEvent #-}
raiseMapEvent :: MonadIO m => MapController a b -> b -> m ()
raiseMapEvent !ctl = liftIO . U.writeChan (ctl ^. mapControllerEventIn)

{-# INLINE initializeMap #-}
initializeMap
  :: (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m)
  => Map
  -> m (Either Text (MapInstance b))
initializeMap m = case parseMap m of
  Just parsedMap -> do
    gfxLoadedMap <- runVolatile @GameDbConn
      $ traverseFirst (traverse getInteractiveObjectByGfxId) parsedMap
    actors <- newIORef mempty
    pure $ Right $ bimap (fmap InteractiveObjectInstance . join)
                         (const actors)
                         gfxLoadedMap
  Nothing -> pure $ Left $ "Probably missing data key for map: " <> showText
    (m ^. mapId)

{-# INLINE runMap #-}
runMap
  :: MonadIO m
  => MapInstance GameClientController
  -> m (GameMapController, ThreadId)
runMap m = do
  (inChan, outChan) <- liftIO U.newChan
  let ctl = MapController m inChan outChan
  tid <- runMapController ctl
  pure (ctl, tid)

{-# INLINE runMapController #-}
runMapController :: MonadIO m => GameMapController -> m ThreadId
runMapController !ctl = liftIO $ forkIO $ runOpenDofusApp True $ go =<< next
 where
  next = liftIO $ U.tryReadChan (ctl ^. mapControllerEventOut)
  go !nextElement = do
    e <- liftIO $ tryRead nextElement
    case e of
      Just mapEvent -> do
        logInfo
          $  "Processing map event: mapId="
          <> displayShow (ctl ^. mapControllerMap . mapInstanceTemplate . mapId)
          <> ", event="
          <> displayShow mapEvent
        runReaderT applyMapEvent $ MapEventArgs mapEvent ctl
        go =<< next
      Nothing -> do
        threadDelay (1000000 `quot` 128)
        go nextElement

{-# INLINE dispatchToMap #-}
dispatchToMap :: (MonadIO m, MapEventReader m, ToNetwork a) => a -> m ()
dispatchToMap !msg = do
  ctl <- asks (view mapEventArgsCtl)
  go $ ctl ^. mapControllerMap . mapInstanceActors
  where go = traverse_ (dispatchToActor msg) <=< readIORef

{-# INLINE dispatchToActor #-}
dispatchToActor
  :: (MonadIO m, MapEventReader m, ToNetwork a)
  => a
  -> GameActor GameClientController
  -> m ()
dispatchToActor !msg =
  let doSend = traverse_ (runReaderT (sendMessage msg))
      readController =
          readIORef
            . controller @(GameActor GameClientController) @GameClientController
  in  doSend <=< readController

{-# INLINE readActors #-}
readActors
  :: (MonadIO m, MapEventReader m) => m [GameActor GameClientController]
readActors = do
  ioActors <- asks
    (view $ mapEventArgsCtl . mapControllerMap . mapInstanceActors)
  HM.elems <$> readIORef ioActors

{-# INLINE applyMapEvent #-}
applyMapEvent :: (MonadIO m, MapEventReader m) => m ()
applyMapEvent = go =<< asks (view mapEventArgsEvent)
 where
  go (MapEventSpawnActor actor) = do
    actors <- asks
      (view $ mapEventArgsCtl . mapControllerMap . mapInstanceActors)
    modifyIORef' actors (HM.insert (actor ^. to actorId) actor)
    dispatchToMap (MapActorSpawn [actor])

  go (MapEventDespawnActor actor) = do
    actors <- asks
      (view $ mapEventArgsCtl . mapControllerMap . mapInstanceActors)
    modifyIORef' actors (HM.delete (actor ^. to actorId))
    dispatchToMap (MapActorDespawn [actor])

  go (MapEventDispatchInformations actor) = do
    actors <- readActors
    dispatchToActor (MapActorSpawn actors) actor
