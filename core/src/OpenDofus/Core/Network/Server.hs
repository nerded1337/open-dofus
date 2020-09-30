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

module OpenDofus.Core.Network.Server
  ( MessageHandler(..)
  , MessageHandlerCallback(..)
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
  , mkServer
  , startServer
  , send
  , sendMessage
  , sendMessages
  , disconnect
  )
where

import           Data.Bytes.Types
import qualified Data.ByteString.Lazy.Builder  as BS
import qualified Data.ByteString.Lazy.Builder.Extras
                                               as BS
import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.ByteString.Unsafe        as BS
import qualified Data.Map.Strict               as M
import           Data.Primitive                 ( Ptr(..)
                                                , newAlignedPinnedByteArray
                                                )
import           Data.Primitive.Addr            ( Addr(..) )
import qualified Data.Vector.Unboxed           as VU
import           Foreign.Marshal                ( free
                                                , mallocBytes
                                                , reallocBytes
                                                )
import           Foreign.Ptr                    ( plusPtr )
import           GHC.Exts                       ( RealWorld )
import qualified Net.IPv4                      as IPv4
import qualified Socket.Stream.IPv4            as SI
import qualified Socket.Stream.Uninterruptible.Addr
                                               as SI
import qualified Socket.Stream.Uninterruptible.MutableBytes
                                               as SIR

import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server.State
import           OpenDofus.Core.Network.Types
import           OpenDofus.Data.Constructible
import           OpenDofus.Prelude

type IsServer s c = (HasServerState s c, HasClientState c, HasClientType s)

type IsServerContext a b m = (IsServer a b, MonadIO m)

defaultClientBufferSegmentSize :: Int
defaultClientBufferSegmentSize = 1024

defaultClientBufferSegmentOffset :: Int
defaultClientBufferSegmentOffset = 0

defaultAlignment :: Int
defaultAlignment = 1

mkServer
  :: (HasClientState a, MonadIO m)
  => Word16
  -> MaxClient
  -> ReceiveBufferSize
  -> ClientCtor a
  -> m (ServerState a)
mkServer !p !mc !sz !ctor =
  liftIO
    $   ServerState p mc sz ctor
    <$> atomically (newTVar mempty)
    <*> atomically (newTVar $ VU.fromList $ unMaxClient <$> [0 .. mc - 1])
    <*> newAlignedPinnedByteArray (unMaxClient mc * unReceiveBufferSize sz)
                                  defaultAlignment

startServer
  :: IsServerContext a b m
  => a
  -> MessageHandler IO a b
  -> m (Either SI.SocketException ())
startServer !st !h = go (st ^. port) (onStarted st h)
 where
  go !p !onStartedCallback =
    liftIO $ SI.withListener (SI.Peer IPv4.loopback p) onStartedCallback

onStarted
  :: IsServerContext a b m
  => a
  -> MessageHandler IO a b
  -> SI.Listener
  -> Word16
  -> m ()
onStarted !st !h = go $ onConnected st h
 where
  go !onConnectedCallback !listener _ =
    liftIO $ forever $ SI.forkAcceptedUnmasked listener
                                               (clientDisconnected st)
                                               onConnectedCallback

clientDisconnected
  :: IsServerContext a b m
  => a
  -> Either SI.CloseException ()
  -> (NetworkId, Maybe Int)
  -> m ()
clientDisconnected !st !e (!netId, !slot) = do
  void $ bitraverse
    ( liftIO
    . BS.putStrLn
    . BS.pack
    . ("Client disconnected with error: " <>)
    . show
    )
    pure
    e
  void $ atomically $ do
    modifyTVar cs (M.delete netId)
    traverse (\s -> modifyTVar cslots (s :-)) slot
 where
  !cs     = st ^. clients
  !cslots = st ^. clientSlots

onConnected
  :: IsServerContext a b m
  => a
  -> MessageHandler IO a b
  -> SI.Connection
  -> SI.Peer
  -> m (NetworkId, Maybe Int)
onConnected st h con peer = liftIO $ do
  !netId         <- NetworkId <$> nextRandom
  !clientSegment <-
    atomically
    .   newTVar
    =<< MutableBytes
    <$> liftIO
          (newAlignedPinnedByteArray defaultClientBufferSegmentSize
                                     defaultAlignment
          )
    <*> pure defaultClientBufferSegmentOffset
    <*> pure defaultClientBufferSegmentSize
  !connected <- atomically $ do
    !connectedClients <- readTVar cs
    if M.size connectedClients >= unMaxClient mc
      then pure Nothing
      else do
        !client         <- ctor netId clientSegment (ClientConnection con peer)
        !availableSlots <- readTVar cslots
        case availableSlots of
          (slot :- slots) -> do
            writeTVar cs $ M.insert netId client connectedClients
            writeTVar cslots slots
            pure $ Just (client, slot)
          _ -> pure Nothing
  case connected of
    Just (!client, !slot) -> do
      !next <- runMessageHandler h $ HandlerInput st client ClientConnected
      case next of
        h'@(MessageHandlerCont _) -> do
          !next' <- onReceive st client h' (MutableBytes buff (slot * sz) sz)
          case next' of
            h''@(MessageHandlerCont _) -> do
              void $ runMessageHandler h'' $ HandlerInput st
                                                          client
                                                          ClientDisconnected
              pure (netId, Just slot)
            h''@(MessageHandlerDisconnect _) -> do
              void $ runMessageHandler h'' $ HandlerInput st
                                                          client
                                                          ClientDisconnected
              pure (netId, Just slot)
            _ -> pure (netId, Just slot)
        h'@(MessageHandlerDisconnect _) -> do
          liftIO $ void $ runMessageHandler h' $ HandlerInput
            st
            client
            ClientDisconnected
          pure (netId, Just slot)
        _ -> pure (netId, Just slot)
    Nothing -> pure (netId, Nothing)
 where
  !mc     = st ^. maxClient
  !cs     = st ^. clients
  !cslots = st ^. clientSlots
  !ctor   = st ^. makeClient
  !buff   = st ^. receiveBufferSegment
  !sz     = unReceiveBufferSize $ st ^. receiveBufferSize

onReceive
  :: (HasClientState a, MonadIO m)
  => b
  -> a
  -> MessageHandler IO b a
  -> MutableBytes RealWorld
  -> m (MessageHandler IO b a)
onReceive !st !client !h slice@(MutableBytes !arr !off _) = liftIO $ loop h
 where
  !socket = client ^. clientStateConnection . clientConnectionSocket

  onReceived (MessageHandlerCont !f) (Right !count) = do
    receive client (MutableBytes arr off count)
    !msgs <- parse client
    !h'   <- handlerLoop msgs (MessageHandlerCont f)
    case h' of
      (MessageHandlerCont _) -> loop h'
      _                      -> pure h'
  onReceived !h' _ = pure h'

  loop !f = onReceived f =<< SIR.receiveOnce socket slice

  handlerLoop (x :- xs) !mh =
    handlerLoop xs =<< runMessageHandler mh (HandlerInput st client x)

  handlerLoop _ !mh = pure mh

sendMessage
  :: (HasClientState c, ToNetwork b, MonadIO m, MonadReader c m) => b -> m ()
sendMessage !message = sendMessages [message]

sendMessages
  :: (HasClientState c, ToNetwork b, Foldable f, MonadIO m, MonadReader c m)
  => f b
  -> m ()
sendMessages = sendBuilder . foldMap ((<> BS.word8 0) . toNetwork)

sendBuilder
  :: (HasClientState c, MonadIO m, MonadReader c m) => BS.Builder -> m ()
sendBuilder !message = do
  (arr@(Ptr addr), !written) <- liftIO $ do
    !arr              <- mallocBytes initialBlockSize
    !action           <- BS.runBuilder message arr initialBlockSize
    (!written, !arr') <- writeToBuffer arr 0 initialBlockSize action
    bs                <- BS.unsafePackCStringLen $ coerce (arr', written)
    liftIO
      $ traverse_ (BS.putStrLn . ("ServerSent: " <>))
      $ filter (/= "")
      $ BS.split '\0'
      $ BS.fromStrict bs
    pure (arr', written)
  !client <- ask
  void $ send client (Addr addr) written
  liftIO $ free arr
 where
  initialBlockSize :: Int
  initialBlockSize = 256

  writeToBuffer
    :: Ptr Word8 -> Int -> Int -> (Int, BS.Next) -> IO (Int, Ptr Word8)
  writeToBuffer !arr _    _  (!written, BS.Done        ) = pure (written, arr)

  writeToBuffer !arr !off !l (!written, BS.More _ !next) = do
    let !l' = l * 2
    !arr' <- reallocBytes arr l'
    let !arr'' = plusPtr arr' (off + written)
    !r                   <- next arr'' (l' - (off + written))
    (!written', !arr''') <- writeToBuffer arr' (off + written) l' r
    pure (written + written', arr''')

  writeToBuffer _ _ _ _ = error "Strict chunks not yet handled"


send
  :: (HasClientState a, MonadIO m)
  => a
  -> Addr
  -> Int
  -> m (Either (SI.SendException 'SI.Uninterruptible) ())
send !client !addr !len = liftIO $ SI.send
  (client ^. clientStateConnection . clientConnectionSocket)
  addr
  len

disconnect :: (HasClientState a, MonadIO m, MonadReader a m) => m ()
disconnect = liftIO . go =<< asks (view clientState)
 where
  go =
    void . SI.disconnect . view (clientStateConnection . clientConnectionSocket)
