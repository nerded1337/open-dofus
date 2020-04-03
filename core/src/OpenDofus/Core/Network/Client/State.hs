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

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module OpenDofus.Core.Network.Client.State
  ( ClientCtor
  , ClientState(..)
  , HasClientState(..)
  , ClientConnection(..)
  , HasClientConnection(..)
  , ClientMessage(..)
  , ClientBuffer
  ) where

import           Control.Concurrent.STM
import           Data.Bytes.Types
import           GHC.Exts
import           OpenDofus.Core.Network.Client.Connection
import           OpenDofus.Core.Network.Client.Message
import           OpenDofus.Core.Network.Types             (NetworkId)
import           OpenDofus.Prelude

type ClientCtor a = NetworkId -> ClientBuffer -> ClientConnection -> STM a

type ClientBuffer = TVar (MutableBytes RealWorld)

data ClientState =
  ClientState
    { _clientStateBufferSegment :: {-# UNPACK #-}!ClientBuffer
    , _clientStateConnection    :: {-# UNPACK #-}!ClientConnection
    }

makeClassy ''ClientState
