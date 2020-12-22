{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- State.hs ---

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

module OpenDofus.Core.Network.Server.State
  ( HasServerState (..),
    ServerState (..),
  )
where

import Network.Socket (PortNumber)
import OpenDofus.Core.Network.Client (ClientConnection)
import OpenDofus.Core.Network.Types (NetworkId)
import OpenDofus.Prelude (makeClassy)
import qualified StmContainers.Map as M

data ServerState m a = ServerState
  { _serverStatePort :: {-# UNPACK #-} !PortNumber,
    _serverStateMakeClient :: !(ClientConnection -> m a),
    _serverStateClients :: {-# UNPACK #-} !(M.Map NetworkId a)
  }

makeClassy ''ServerState
