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

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module OpenDofus.Game.Server where

import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server
import           OpenDofus.Core.Network.Types
import           OpenDofus.Database
import           OpenDofus.Prelude

data GameClient = GameClient
    { _gameClientNetId :: !NetworkId
    , _gameClientState :: !ClientState
    }

makeClassy ''GameClient

data GameServer = GameServer
    { _gameServerState      :: !(ServerState GameClient)
    , _gameServerAuthDbPool :: !(Pool AuthDbConn)
    , _gameServerGameDbPool :: !(Pool GameDbConn)
    }

makeClassy ''GameServer

type GameClientHandler = MessageHandler GameServer GameClient

instance HasConnectPool GameServer GameDbConn where
  {-# INLINE getConnectionPool #-}
  getConnectionPool = view gameServerGameDbPool

instance HasConnectPool GameServer AuthDbConn where
  {-# INLINE getConnectionPool #-}
  getConnectionPool = view gameServerAuthDbPool

instance HasClientType GameServer where
  type ClientTypeOf GameServer = GameClient

instance HasServerState GameServer GameClient where
  {-# INLINE serverState #-}
  serverState = gameServerState

instance Show GameClient where
  show (GameClient netId _) =
    "GameClient { " <> show netId <> " }"

instance HasClientConnection GameClient where
  {-# INLINE clientConnection #-}
  clientConnection = clientConnection

instance HasClientState GameClient where
  {-# INLINE clientState #-}
  clientState = gameClientState

instance HasNetworkId GameClient where
  {-# INLINE networkId #-}
  networkId = gameClientNetId

{-# INLINE mkClient #-}
mkClient :: NetworkId -> ClientBuffer -> ClientConnection -> STM GameClient
mkClient i b c =
  pure $ GameClient i (ClientState b c)
