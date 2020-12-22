{-# LANGUAGE AllowAmbiguousTypes #-}
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
    sendClientMessages,
  )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Writer.Class (MonadWriter (tell))
import Control.Monad.Writer.Strict (execWriterT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Builder as LBS
import qualified Data.DList as DL
import qualified Network.Socket as SK
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
import qualified Streamly.Data.Array.Storable.Foreign as A
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Tuple.Strict as SS
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Prelude as S
import Unsafe.Coerce (unsafeCoerce)

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

    createConnection sock =
      liftIO $
        ClientConnection sock
          <$> createNetworkId
          <*> SK.getPeerName sock

    createNetworkId = NetworkId <$> nextRandom

clientStream ::
  IsServerContext m s =>
  s ->
  ClientHandler m s () ->
  ClientConnection ->
  S.SerialT m ()
clientStream server clientHandler conn = S.mapM (clientLoop server clientHandler) clientMessages
  where
    clientMessages = onConnected <> onClientSent <> onDisconnected
    onConnected = pure $ SS.Tuple' conn ClientConnected
    onDisconnected = pure $ SS.Tuple' conn ClientDisconnected
    onClientSent =
      let sk = conn ^. clientConnectionSocket
       in S.finally
            (liftIO $ SK.close sk)
            $ S.unfold (UF.concat NS.readChunksWithBufferOf A.read) (defaultRcvBuffSize, sk)
              & S.filter (/= BI.c2w '\n')
              & S.splitOn (== 0) FL.toList
              & S.handle @_ @_ @SomeException (pure mempty)
              & S.map (SS.Tuple' conn . ClientSent . BS.pack . unsafeCoerce)

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
  liftIO $ sendMessages (client ^. clientConnection . clientConnectionSocket) om

emit :: (ToNetwork a, MonadWriter (DL.DList a) m) => a -> m ()
emit = tell . pure
{-# INLINE emit #-}

sendClientMessages ::
  (MonadIO m, ToNetwork a, Traversable t, HasClientConnection c) =>
  c ->
  t a ->
  m ()
sendClientMessages client =
  sendMessages (client ^. clientConnection . clientConnectionSocket)
{-# INLINE sendClientMessages #-}

sendMessages ::
  (MonadIO m, ToNetwork a, Traversable t) =>
  SK.Socket ->
  t a ->
  m ()
sendMessages socket = sendBuilder socket . foldMap ((<> LBS.word8 0) . toNetwork)
{-# INLINE sendMessages #-}

sendBuilder :: MonadIO m => SK.Socket -> LBS.Builder -> m ()
sendBuilder socket message
  | null encodedMessage =
    pure ()
  | otherwise =
    void $
      liftIO $
        try @_ @SomeException $
          S.fold
            (NS.write socket)
            (S.fromList $ unsafeCoerce encodedMessage)
  where
    encodedMessage = BS.unpack $ LBS.toStrict $ toLazyByteString message
{-# INLINE sendBuilder #-}

kick :: (MonadIO m, MonadReader a m, HasClientConnection a) => m ()
kick = doKick =<< ask
  where
    doKick =
      liftIO
        . void
        . try @_ @SomeException
        . SK.close
        . view (clientConnection . clientConnectionSocket)
{-# INLINE kick #-}
