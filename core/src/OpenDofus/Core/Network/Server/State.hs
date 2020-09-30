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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module OpenDofus.Core.Network.Server.State
  ( HasServerState(..)
  , HasClientType(..)
  , ServerState(..)
  )
where

import           Data.Kind
import qualified Data.Map.Strict               as M
import           Data.Primitive                 ( MutableByteArray )
import qualified Data.Vector.Unboxed           as VU
import           GHC.Exts

import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Types
import           OpenDofus.Prelude

class ( HasNetworkId (ClientTypeOf a)
      , HasClientConnection (ClientTypeOf a)
      , HasClientState (ClientTypeOf a)
      ) =>
      HasClientType a
  where
  type ClientTypeOf a :: Type

data ServerState a = ServerState
    { _port                 :: {-# UNPACK #-} !Word16
    , _maxClient            :: {-# UNPACK #-} !MaxClient
    , _receiveBufferSize    :: {-# UNPACK #-} !ReceiveBufferSize
    , _makeClient           :: !(ClientCtor a)
    , _clients              :: {-# UNPACK #-} !(TVar (M.Map NetworkId a))
    , _clientSlots          :: {-# UNPACK #-} !(TVar (VU.Vector Int))
    , _receiveBufferSegment :: {-# UNPACK #-} !(MutableByteArray RealWorld)
    }

makeClassy ''ServerState

instance HasServerState a b => HasServerState (HandlerInput a b) b where
  serverState = handlerInputServer . serverState
