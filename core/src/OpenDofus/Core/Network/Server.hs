{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- Server.hs ---

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

module OpenDofus.Core.Network.Server
  ( HandlerInput (..),
    HasHandlerInput (..),
    HasServerState (..),
    HasClientType (..),
    HasMessageType (..),
    ServerState (..),
    ClientMessage (..),
    ClientPacket,
    ClientHandler,
    IsServerContext,
    IsServer,
    startServer,
    emit,
    kick,
    emitToClient,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Writer.Class (MonadWriter (tell))
import Control.Monad.Writer.Strict (execWriterT)
import qualified Data.ByteString.Lazy.Builder as LBS
import qualified Data.DList as DL
import qualified Network.Socket as SK
import OpenDofus.Core.Data.Constructible
import OpenDofus.Core.Network.Client
  ( ClientConnection (ClientConnection),
    ClientMessage (..),
    ClientPacket,
    HasClientConnection (..),
    ToNetwork,
    toNetwork,
  )
import OpenDofus.Core.Network.Server.State
  ( HasServerState (..),
    ServerState (..),
  )
import OpenDofus.Core.Network.Server.Types
  ( ClientHandler,
    HandlerInput (..),
    HasClientType (..),
    HasHandlerInput (..),
    HasMessageType (..),
  )
import OpenDofus.Core.Network.Types
  ( HasNetworkId (..),
    NetworkId (NetworkId),
  )
import OpenDofus.Prelude
import qualified StmContainers.Map as M
import qualified Streamly.FileSystem.Handle as FS
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Tuple.Strict as SS
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S
import Z.Data.CBytes (w2c)

type IsServer m s =
  ( HasServerState s m (ClientTypeOf s),
    HasMessageType s,
    ToNetwork (MessageTypeOf s),
    HasClientType s,
    HasClientConnection (ClientTypeOf s)
  )

type IsServerContext m s = (IsServer m s, MonadCatch m, S.MonadAsync m)

defaultRcvBuffSize :: Int
defaultRcvBuffSize = 256

startServer :: IsServerContext m s => s -> ClientHandler m s () -> m ()
startServer server clientHandler = do
  S.unfold (TCP.acceptOnPortWith options) (server ^. serverStatePort)
    & S.mapM createConnection
    & S.concatMapWith S.parallel (clientStream server clientHandler)
    & S.drain
  where
    options = [(SK.ReuseAddr, 1), (SK.KeepAlive, 0), (SK.NoDelay, 1)]

    createConnection sock = liftIO $ do
      -- The socket is done once we have converted it to a Handle
      peer <- SK.getPeerName sock
      ClientConnection
        <$> SK.socketToHandle sock ReadWriteMode
        <*> createNetworkId
        <*> pure peer

    createNetworkId =
      NetworkId <$> nextRandom

clientStream ::
  IsServerContext m s =>
  s ->
  ClientHandler m s () ->
  ClientConnection ->
  S.SerialT m ()
clientStream server clientHandler conn =
  S.mapM
    (clientLoop server clientHandler)
    (onConnected <> onClientSent <> onDisconnected)
  where
    mkTup = SS.Tuple' conn
    onConnected = pure $ mkTup ClientConnected
    onDisconnected = pure $ mkTup ClientDisconnected
    arrayToBS 10 xs = xs
    arrayToBS x xs = w2c x :- xs
    onClientSent =
      let sk = conn ^. clientConnectionSocket
       in S.finally (liftIO $ hClose sk) $
            S.unfold FS.readChunksWithBufferOf (defaultRcvBuffSize, sk)
              & AS.splitOn 0
              & S.handle @_ @_ @SomeException mempty
              & S.map (mkTup . ClientSent . A.foldr arrayToBS mempty)

clientLoop ::
  IsServerContext m s =>
  s ->
  ClientHandler m s () ->
  SS.Tuple' ClientConnection ClientMessage ->
  m ()
clientLoop srv clientHandler (SS.Tuple' cc msg) = do
  let netId = cc ^. networkId
      clients = srv ^. serverState . serverStateClients
  client <- fmap (fromMaybe (error "impossible")) $ case msg of
    ClientConnected -> do
      client <- (srv ^. serverStateMakeClient) cc
      liftIO $ atomically $ M.insert client netId clients
      pure $ Just client
    ClientDisconnected ->
      liftIO $
        atomically $ do
          client <- M.lookup netId clients
          M.delete netId clients
          pure client
    _ ->
      liftIO $ atomically $ M.lookup netId clients
  om <- execWriterT $ runReaderT clientHandler $ HandlerInput srv client msg
  emitToClient client om

emit :: (ToNetwork a, MonadWriter (DL.DList a) m) => a -> m ()
emit = tell . pure
{-# INLINE emit #-}

emitToClient ::
  (MonadIO m, ToNetwork a, Traversable t, HasClientConnection c) =>
  c ->
  t a ->
  m ()
emitToClient client =
  emitToSocket (client ^. clientConnection . clientConnectionSocket)
{-# INLINE emitToClient #-}

emitToSocket ::
  (MonadIO m, ToNetwork a, Traversable t) =>
  Handle ->
  t a ->
  m ()
emitToSocket h = emitSocketBuilder h . foldMap ((<> LBS.word8 0) . toNetwork)
{-# INLINE emitToSocket #-}

emitSocketBuilder :: MonadIO m => Handle -> LBS.Builder -> m ()
emitSocketBuilder h message =
  void $
    liftIO $
      try @_ @SomeException $
        LBS.hPutBuilder h message
{-# INLINE emitSocketBuilder #-}

kick :: (MonadIO m, MonadReader a m, HasClientConnection a) => m ()
kick = doKick =<< ask
  where
    doKick =
      liftIO
        . void
        . try @_ @SomeException
        . hClose
        . view (clientConnection . clientConnectionSocket)
{-# INLINE kick #-}
