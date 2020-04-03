-- Server.hs ---

-- Copyright (C) 2019 Nerd Ed

-- Author: Nerd Ed <nerded.nerded@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This l Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module OpenDofus.Core.Network.Server
  ( MessageHandler(..)
  , HandlerInput(..)
  , HasHandlerInput(..)
  , HasServerState(..)
  , HasClientType(..)
  , ServerState(..)
  , ClientState(..)
  , ClientMessage(..)
  , HasClientState(..)
  , IsServerContext
  , IsServer
  , IsClient
  , mkServer
  , startServer
  , send
  , sendMessage
  , sendMessages
  , disconnect
  ) where

import           Data.Bytes.Types
import qualified Data.ByteString.Lazy.Builder               as BS
import qualified Data.ByteString.Lazy.Builder.Extras        as BS
import qualified Data.ByteString.Lazy.Char8                 as BS
import qualified Data.Foldable                              as F
import qualified Data.Map.Strict                            as M
import           Data.Primitive                             (MutableByteArray, copyMutableByteArray,
                                                             getSizeofMutableByteArray,
                                                             mutableByteArrayContents,
                                                             newAlignedPinnedByteArray)
import qualified Data.Vector.Unboxed                        as VU
import           Foreign.Ptr
import           GHC.Exts                                   (RealWorld)
import qualified Net.IPv4                                   as IPv4
import qualified Socket.Stream.IPv4                         as SI
import qualified Socket.Stream.Uninterruptible.MutableBytes as SI

import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server.State
import           OpenDofus.Core.Network.Types
import           OpenDofus.Data.Constructible
import           OpenDofus.Prelude

type IsClient c s
   = (HasNetworkId c, HasClientConnection c, HasClientState c)

type IsServer s c = (HasServerState s c, IsClient c s, HasClientType s)

type IsServerContext a b m = (IsServer a b, MonadIO m)

{-# INLINE defaultClientBufferSegmentSize #-}
defaultClientBufferSegmentSize :: Int
defaultClientBufferSegmentSize = 1024

{-# INLINE defaultClientBufferSegmentOffset #-}
defaultClientBufferSegmentOffset :: Int
defaultClientBufferSegmentOffset = 0

{-# INLINE defaultAlignment #-}
defaultAlignment :: Int
defaultAlignment = 1

{-# INLINE mkServer #-}
mkServer ::
     (IsClient a b,
     MonadIO m)
  => Word16
  -> MaxClient
  -> ReceiveBufferSize
  -> ClientCtor a
  -> m (ServerState a)
mkServer !p !mc !sz !ctor = liftIO $ do
  ServerState p mc sz ctor
    <$> atomically (newTVar mempty)
    <*> atomically (newTVar $ VU.fromList $ unMaxClient <$> [0..mc - 1])
    <*> newAlignedPinnedByteArray
         (unMaxClient mc * unReceiveBufferSize sz)
         defaultAlignment

{-# INLINE startServer #-}
startServer :: IsServerContext a b m => a -> MessageHandler a b -> m (Either SI.SocketException ())
startServer st h = go (st ^. port) (onStarted st h)
  where
    go p onStartedCallback =
      liftIO $ SI.withListener (SI.Peer IPv4.loopback p) onStartedCallback

{-# INLINE onStarted #-}
onStarted :: IsServerContext a b m => a -> MessageHandler a b -> SI.Listener -> Word16 -> m ()
onStarted st h = go $ onConnected st h
  where
    go onConnectedCallback listener _ = do
      liftIO $
        forever $
        SI.forkAcceptedUnmasked
          listener
          (clientDisconnected st)
          onConnectedCallback

{-# INLINE clientDisconnected #-}
clientDisconnected ::
     IsServerContext a b m
  => a
  -> Either SI.CloseException ()
  -> (NetworkId, Maybe Int)
  -> m ()
clientDisconnected st e (netId, slot) = do
  liftIO $ BS.putStrLn $ BS.pack $ show e
  void $
    atomically $ do
      modifyTVar cs (M.delete netId)
      traverse (\s -> modifyTVar cslots (\ss -> s :- ss)) slot
  where
    cs = st ^. clients
    cslots = st ^. clientSlots

{-# INLINE onConnected #-}
onConnected ::
     IsServerContext a b m
  => a
  -> MessageHandler a b
  -> SI.Connection
  -> SI.Peer
  -> m (NetworkId, Maybe Int)
onConnected st h con peer =
  let mc = st ^. maxClient
      cs = st ^. clients
      cslots = st ^. clientSlots
      ctor = st ^. makeClient
      sz = unReceiveBufferSize $ st ^. receiveBufferSize
      buff = st ^. receiveBufferSegment
   in liftIO $ do
        netId <- NetworkId <$> nextRandom
        clientSegment <-
          atomically . newTVar =<<
          MutableBytes <$>
          liftIO
            (newAlignedPinnedByteArray
               defaultClientBufferSegmentSize
               defaultAlignment) <*>
          pure defaultClientBufferSegmentOffset <*>
          pure defaultClientBufferSegmentSize
        connected <-
          atomically $ do
            connectedClients <- readTVar cs
            if (M.size connectedClients >= unMaxClient mc)
              then pure Nothing
              else do
                client <- ctor netId clientSegment (ClientConnection con peer)
                availableSlots <- readTVar cslots
                case availableSlots of
                  (slot :- slots) -> do
                    writeTVar cs $ M.insert netId client connectedClients
                    writeTVar cslots slots
                    pure $ Just (client, slot)
                  _ -> pure Nothing
        case connected of
          Just !(client, slot) -> do
            next <- runMessageHandler h $ HandlerInput st client ClientConnected
            case next of
              h'@(MessageHandlerCont _) -> do
                next' <-
                  onReceive st client h' (MutableBytes buff (slot * sz) sz)
                case next' of
                  h''@(MessageHandlerCont _) -> do
                    void $ runMessageHandler h'' $ HandlerInput st client ClientDisconnected
                    pure (netId, Just slot)
                  _ -> do
                    pure (netId, Just slot)
              _ -> do
                pure (netId, Just slot)
          Nothing -> do
            pure (netId, Nothing)

{-# INLINE onReceive #-}
onReceive ::
     (IsClient a c, MonadIO m)
  => b
  -> a
  -> MessageHandler b a
  -> MutableBytes RealWorld
  -> m (MessageHandler b a)
onReceive !st !client !h !slice@(MutableBytes arr off _) =
  let socket = client ^. clientStateConnection . clientConnectionSocket
      onReceived (MessageHandlerCont f) (Right count) = do
        receive client (MutableBytes arr off count)
        msgs <- parse client
        h' <- handlerLoop msgs (MessageHandlerCont f)
        loop h'
      onReceived !h' _ = do
        pure h'
      loop !f = onReceived f =<< SI.receiveOnce socket slice
   in liftIO $ loop h
  where
    handlerLoop (x :- xs) (MessageHandlerCont f) = do
      nextHandler <- runReaderT f $ HandlerInput st client x
      handlerLoop xs nextHandler
    handlerLoop _ a = pure $! a

{-# INLINE sendMessage #-}
sendMessage ::
     ( IsClient c s
     , HasHandlerInput i s c
     , ToNetwork b
     , MonadIO m
     , MonadReader i m
     )
  => b
  -> m ()
sendMessage !message = sendMessages [message]

{-# INLINE sendMessages #-}
sendMessages ::
     ( IsClient c s
     , HasHandlerInput i s c
     , ToNetwork b
     , Foldable f
     , MonadIO m
     , MonadReader i m
     )
  => f b
  -> m ()
sendMessages !messages =
  sendBuilder (F.length messages) $
  foldMap ((<> BS.word8 0) . toNetwork) messages

{-# INLINE resizeMutablePinnedByteArray #-}
resizeMutablePinnedByteArray ::
     MonadIO m
  => MutableByteArray RealWorld
  -> Int
  -> m (MutableByteArray RealWorld)
resizeMutablePinnedByteArray arr n =
  liftIO $ go =<< getSizeofMutableByteArray arr
  where
    go s
      | s < n = do
        arr' <- newAlignedPinnedByteArray n 1
        copyMutableByteArray arr' 0 arr 0 s
        pure arr'
      | otherwise = pure arr

{-# INLINE sendBuilder #-}
sendBuilder ::
     (IsClient c s, MonadIO m, HasHandlerInput i s c, MonadReader i m)
  => Int
  -> BS.Builder
  -> m ()
sendBuilder !resizeFactor !message = do
    arr <- liftIO $ newAlignedPinnedByteArray initialBlockSize 1
    action <- liftIO $ BS.runBuilder message (mutableByteArrayContents arr) initialBlockSize
    (written, arr') <- liftIO $ writeToBuffer arr 0 action
    client <- asks (view handlerInputClient)
    void $ send client (MutableBytes arr' 0 written)
  where
    initialBlockSize :: Int
    initialBlockSize = 64 * resizeFactor
    writeToBuffer ::
         (MutableByteArray RealWorld)
      -> Int
      -> (Int, BS.Next)
      -> IO (Int, MutableByteArray RealWorld)
    writeToBuffer !arr _ !(written, BS.Done) = pure $ (written, arr)
    writeToBuffer !arr !off !(written, BS.More _ next) = do
      s <- getSizeofMutableByteArray arr
      arr' <- resizeMutablePinnedByteArray arr (s * 2)
      r <- next (plusPtr (mutableByteArrayContents arr') (off + written)) ((s - (off + written)) + s)
      (written', arr'') <- writeToBuffer arr' (off + written) r
      pure $! (written + written', arr'')
    writeToBuffer !arr !off !(written, BS.Chunk _ next) = do
      s <- getSizeofMutableByteArray arr
      r <- next (plusPtr (mutableByteArrayContents arr) (off + written)) s
      (written', arr') <- writeToBuffer arr (off + written) r
      pure $! (written + written', arr')

{-# INLINE send #-}
send ::
     (IsClient c s, MonadIO m)
  => c
  -> MutableBytes RealWorld
  -> m (Either (SI.SendException 'SI.Uninterruptible) ())
send !client = liftIO . SI.send (client ^. clientStateConnection . clientConnectionSocket)

{-# INLINE disconnect #-}
disconnect ::
     (IsClient c s, HasHandlerInput i s c, MonadIO m, MonadReader i m) => m ()
disconnect = go =<< asks (view handlerInputClient)
  where
    go client =
      liftIO $
      SI.disconnect_ (client ^. clientStateConnection . clientConnectionSocket)
