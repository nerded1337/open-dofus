-- State.hs ---

-- Copyright (C) 2019 Nerd Ed

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

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module OpenDofus.Core.Network.Server.State
  ( HasServerState(..)
  , HandlerInput(..)
  , HasHandlerInput(..)
  , HasClientType(..)
  , ServerState(..)
  , MessageHandler(..)
  , ClientConnection(..)
  , HasClientConnection(..)
  , HasClientState(..)
  , ClientCtor
  , runMessageHandler
  ) where

import           Data.Kind
import qualified Data.Map.Strict               as M
import           Data.Primitive                (MutableByteArray)
import qualified Data.Vector.Unboxed           as VU
import           GHC.Exts

import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Types  (HasNetworkId, MaxClient,
                                                NetworkId, ReceiveBufferSize)
import           OpenDofus.Prelude

data HandlerInput a b =
  HandlerInput
    { _handlerInputServer  :: !a
    , _handlerInputClient  :: !b
    , _handlerInputMessage :: !ClientMessage
    }

makeClassy ''HandlerInput

runMessageHandler ::
     MonadIO m
  => MessageHandler a b
  -> HandlerInput a b
  -> m (MessageHandler a b)
runMessageHandler (MessageHandlerCont f) i = liftIO $ runReaderT f i
runMessageHandler x                      _ = pure x

data MessageHandler a b where
  MessageHandlerLeaf :: MessageHandler a b
  MessageHandlerDisconnect :: MessageHandler a b
  MessageHandlerCont
    :: ReaderT (HandlerInput a b) IO (MessageHandler a b)
    -> MessageHandler a b

instance Show (MessageHandler a b) where
  show MessageHandlerLeaf       = "MessageHandlerLeaf"
  show MessageHandlerDisconnect = "MessageHandlerDisconnect"
  show (MessageHandlerCont _)   = "MessageHandlerCont"

instance Semigroup (MessageHandler a b) where
  MessageHandlerCont f <> MessageHandlerCont g = MessageHandlerCont fg
    where
      fg = do
        f' <- f
        g' <- g
        pure $ f' <> g'
  MessageHandlerLeaf <> x = x
  x <> MessageHandlerLeaf = x
  MessageHandlerDisconnect <> (MessageHandlerCont f) = MessageHandlerCont f'
    where
      f' = do
        g <- f
        pure $ MessageHandlerCont $ f'' g
      f'' g = do
        x <- ask
        void $ runMessageHandler g x
        pure MessageHandlerLeaf
  (MessageHandlerCont f) <> MessageHandlerDisconnect = MessageHandlerCont f'
    where
      f' = do
        g <- f
        pure $ MessageHandlerCont $ f'' g
      f'' g = do
        x <- ask
        void $ runMessageHandler g x
        pure MessageHandlerLeaf
  MessageHandlerDisconnect <> MessageHandlerDisconnect = MessageHandlerDisconnect

class ( HasNetworkId (ClientTypeOf a)
      , HasClientConnection (ClientTypeOf a)
      , HasClientState (ClientTypeOf a)
      ) =>
      HasClientType a
  where
  type ClientTypeOf a :: Type

data ServerState a =
  ServerState
    { _port                 :: {-# UNPACK #-}!Word16
    , _maxClient            :: {-# UNPACK #-}!MaxClient
    , _receiveBufferSize    :: {-# UNPACK #-}!ReceiveBufferSize
    , _makeClient           :: ClientCtor a
    , _clients              :: {-# UNPACK #-}!(TVar (M.Map NetworkId a))
    , _clientSlots          :: {-# UNPACK #-}!(TVar (VU.Vector Int))
    , _receiveBufferSegment :: {-# UNPACK #-}!(MutableByteArray RealWorld)
    }

makeClassy ''ServerState
