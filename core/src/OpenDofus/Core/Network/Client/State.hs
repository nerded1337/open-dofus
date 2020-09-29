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

{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module OpenDofus.Core.Network.Client.State
  ( ClientCtor
  , ClientState(..)
  , HasClientState(..)
  , ClientConnection(..)
  , HasClientConnection(..)
  , ClientMessage(..)
  , ClientBuffer
  , HandlerInput(..)
  , HasHandlerInput(..)
  , MessageHandlerCallback(..)
  , MessageHandler(..)
  , runMessageHandler
  )
where

import           Control.Concurrent.STM
import           Data.Bytes.Types
import           GHC.Exts
import           OpenDofus.Core.Network.Client.Connection
import           OpenDofus.Core.Network.Client.Message
import           OpenDofus.Core.Network.Types   ( NetworkId )
import           OpenDofus.Prelude

type ClientCtor a = NetworkId -> ClientBuffer -> ClientConnection -> STM a

type ClientBuffer = TVar (MutableBytes RealWorld)

data ClientState = ClientState
    { _clientStateBufferSegment :: {-# UNPACK #-} !ClientBuffer
    , _clientStateConnection    :: {-# UNPACK #-} !ClientConnection
    , _clientStateNetworkId     :: {-# UNPACK #-} !NetworkId
    }

makeClassy ''ClientState

data HandlerInput a b = HandlerInput
    { _handlerInputServer  :: !a
    , _handlerInputClient  :: !b
    , _handlerInputMessage :: !ClientMessage
    }

makeClassy ''HandlerInput

instance HasClientState b => HasClientState (HandlerInput a b) where
  {-# INLINE clientState #-}
  clientState = handlerInputClient . clientState

{-# INLINE runMessageHandler #-}
runMessageHandler
  :: MonadIO m
  => MessageHandler m a b
  -> HandlerInput a b
  -> m (MessageHandler m a b)
runMessageHandler (MessageHandlerCont f) i =
  runReaderT (unMessageHandlerCallback f) i
runMessageHandler (MessageHandlerDisconnect f) i =
  runReaderT (unMessageHandlerCallback f) i
runMessageHandler x _ = pure x

newtype MessageHandlerCallback a m b =
  MessageHandlerCallback
    { unMessageHandlerCallback :: ReaderT a m b
    }
  deriving newtype (Functor, Applicative, Monad, MonadReader a)

instance MonadIO (MessageHandlerCallback a IO) where
  {-# INLINE liftIO #-}
  liftIO x = MessageHandlerCallback $ ReaderT $ const x

data MessageHandler m a b = MessageHandlerLeaf
    | MessageHandlerDisconnect !(MessageHandlerCallback
                               (HandlerInput a b)
                               m
                               (MessageHandler m a b))
    | MessageHandlerCont !(MessageHandlerCallback (HandlerInput a b) m
                         (MessageHandler m a b))

instance Show (MessageHandler m a b) where
  {-# INLINE show #-}
  show MessageHandlerLeaf           = "MessageHandlerLeaf"
  show (MessageHandlerDisconnect _) = "MessageHandlerDisconnect"
  show (MessageHandlerCont       _) = "MessageHandlerCont"

instance Monad m => Semigroup (MessageHandler m a b) where
  {-# INLINE (<>) #-}
  MessageHandlerLeaf   <> x                    = x
  x                    <> MessageHandlerLeaf   = x
  MessageHandlerCont f <> MessageHandlerCont g = MessageHandlerCont $ f <> g
  (MessageHandlerDisconnect f) <> (MessageHandlerCont g) =
    MessageHandlerDisconnect $ f <> g
  (MessageHandlerCont f) <> (MessageHandlerDisconnect g) =
    MessageHandlerDisconnect $ f <> g
  (MessageHandlerDisconnect f) <> (MessageHandlerDisconnect g) =
    MessageHandlerDisconnect $ f <> g

instance Monad m => Monoid (MessageHandler m a b) where
  {-# INLINE mempty #-}
  mempty = MessageHandlerLeaf

instance (Semigroup b, Monad m) => Semigroup (MessageHandlerCallback a m b) where
  {-# INLINE (<>) #-}
  f <> g = do
    x <- f
    y <- g
    pure $ x <> y

instance (Monoid b, Monad m) => Monoid (MessageHandlerCallback a m b) where
  {-# INLINE mempty #-}
  mempty = pure mempty
