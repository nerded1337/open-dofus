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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module OpenDofus.Game.Map
  ( module X
  , initializeMap
  , runMap
  )
where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Chan.Unagi.NoBlocking
                                               as U
import           Data.ByteString.Char8         as BS
import           OpenDofus.Database
import           OpenDofus.Game.Map.Parser     as X
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

initializeMap
  :: forall a b m
   . (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m)
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

runMap
  :: MonadIO m
  => MapInstance GameClient
  -> m (MapController GameClient GameHandlerInput, ThreadId)
runMap m = do
  (inChan, outChan) <- liftIO U.newChan
  let mapCtl = MapController m inChan outChan
  tid <- runMapController mapCtl
  pure (mapCtl, tid)

runMapController
  :: MonadIO m => MapController GameClient GameHandlerInput -> m ThreadId
runMapController mapCtl = liftIO $ forkIO $ go mapInitialHandler =<< next
 where
  next = U.tryReadChan (mapCtl ^. mapControllerEventOut)
  go !h !nextElement = do
    e <- tryRead nextElement
    case e of
      Just mapEvent -> do
        h' <- runMessageHandler h mapEvent
        go h' =<< next
      Nothing -> do
        threadDelay (1000000 `quot` 128)
        go h nextElement

mapInitialHandler :: GameClientHandler
mapInitialHandler = MessageHandlerCont go
 where
  go = do
    msg <- asks (view handlerInputMessage)
    liftIO $ BS.putStrLn $ "Map handling: " <> showByteString msg
    pure mapInitialHandler
