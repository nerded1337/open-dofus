{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- Connection.hs ---

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

module OpenDofus.Core.Network.Client.Connection
  ( HasClientConnection (..),
    ClientConnection (..),
  )
where

import Network.Socket (SockAddr, Socket)
import OpenDofus.Core.Network.Types (HasNetworkId (..), NetworkId)
import OpenDofus.Prelude

data ClientConnection = ClientConnection
  { _clientConnectionSocket :: {-# UNPACK #-} !Socket,
    _clientConnectionNetworkId :: {-# UNPACK #-} !NetworkId,
    _clientConnectionIp :: !SockAddr
  }

makeClassy ''ClientConnection

instance HasNetworkId ClientConnection where
  networkId = clientConnectionNetworkId
  {-# INLINE networkId #-}

instance Show ClientConnection where
  show conn =
    "Connection { networkId = \"" <> show (conn ^. networkId) <> "\", ip = \"" <> show (conn ^. clientConnectionIp) <> "\" }"
