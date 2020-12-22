{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Types.hs ---

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

module OpenDofus.Core.Network.Server.Types
  ( ClientHandler,
    HandlerInput (..),
    HasHandlerInput (..),
    HasMessageType (..),
    HasClientType (..),
  )
where

import Control.Monad.Writer.Strict (WriterT)
import qualified Data.DList as DL
import Data.Kind (Type)
import OpenDofus.Core.Network.Client
  ( ClientMessage,
    HasClientConnection (..),
    ToNetwork,
  )
import OpenDofus.Core.Network.Server.State (HasServerState (..))
import OpenDofus.Prelude

class ToNetwork (MessageTypeOf a) => HasMessageType a where
  type MessageTypeOf a :: Type

class HasClientConnection (ClientTypeOf a) => HasClientType a where
  type ClientTypeOf a :: Type

data HandlerInput s = HandlerInput
  { _handlerInputServer :: !s,
    _handlerInputClient :: !(ClientTypeOf s),
    _handlerInputMessage :: !ClientMessage
  }

makeClassy ''HandlerInput

instance HasServerState a m b => HasServerState (HandlerInput a) m b where
  serverState = handlerInputServer . serverState
  {-# INLINE serverState #-}

instance HasClientType a => HasClientConnection (HandlerInput a) where
  clientConnection = handlerInputClient . clientConnection
  {-# INLINE clientConnection #-}

type ClientHandler m s a =
  ReaderT (HandlerInput s) (WriterT (DL.DList (MessageTypeOf s)) m) a
