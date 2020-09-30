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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module OpenDofus.Auth.Server where

import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server
import           OpenDofus.Core.Network.Types
import           OpenDofus.Database
import           OpenDofus.Prelude

newtype AuthClient = AuthClient
    { _authClientState :: ClientState
    }

makeClassy ''AuthClient

data AuthServer = AuthServer
    { _authServerState  :: {-# UNPACK #-} !(ServerState AuthClient)
    , _authServerDbPool :: {-# UNPACK #-} !(Pool AuthDbConn)
    }

makeClassy ''AuthServer

type AuthClientHandler = MessageHandler IO AuthServer AuthClient

instance HasConnectPool AuthServer AuthDbConn where
  getConnectionPool = view authServerDbPool

instance HasClientType AuthServer where
  type ClientTypeOf AuthServer = AuthClient

instance HasServerState AuthServer AuthClient where
  serverState = authServerState

instance Show AuthClient where
  show (AuthClient s) =
    "AuthClient { " <> show (s ^. clientStateNetworkId) <> " }"

instance HasClientConnection AuthClient where
  clientConnection = clientConnection

instance HasClientState AuthClient where
  clientState = authClientState

instance HasNetworkId AuthClient where
  networkId = authClientState . clientStateNetworkId

mkClient :: NetworkId -> ClientBuffer -> ClientConnection -> STM AuthClient
mkClient i b c = pure $ AuthClient (ClientState b c i)
