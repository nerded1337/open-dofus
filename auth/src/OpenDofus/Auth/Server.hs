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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpenDofus.Auth.Server where

import OpenDofus.Core.Network.Client
  ( ClientConnection,
    HasClientConnection (..),
  )
import OpenDofus.Core.Network.Server
  ( HasClientType (..),
    HasMessageType (..),
    HasServerState (..),
    HandlerInput(..),
    HasHandlerInput(..),
    ServerState,
  )
import OpenDofus.Core.Network.Types (HasNetworkId (..))
import OpenDofus.Database.Game
import OpenDofus.Database
import OpenDofus.Auth.Network.Message (AuthMessage, Salt)
import OpenDofus.Prelude

data AuthState
  = Greeting
  | ProtocolRequired {-# UNPACK #-} !Salt
  | LoggingIn {-# UNPACK #-} !Salt
  | LoggedIn {-# UNPACK #-} !Account ![WorldServer]
  | Kicked
  deriving (Show)

makePrisms ''AuthState

data AuthClient = AuthClient
  { _authClientState :: !(IORef AuthState),
    _authClientConnection :: {-# UNPACK #-} !ClientConnection
  }

makeClassy ''AuthClient

data AuthServer = AuthServer
  { _authServerState :: {-# UNPACK #-} !(ServerState IO AuthClient),
    _authServerDbPool :: {-# UNPACK #-} !(Pool AuthDbConn)
  }

makeClassy ''AuthServer

instance HasServerState AuthServer IO AuthClient where
  serverState = authServerState
  {-# INLINE serverState #-}

instance HasClientType AuthServer where
  type ClientTypeOf AuthServer = AuthClient

instance HasMessageType AuthServer where
  type MessageTypeOf AuthServer = AuthMessage

instance HasConnectPool AuthServer AuthDbConn where
  getConnectionPool = view authServerDbPool
  {-# INLINE getConnectionPool #-}

instance HasConnectPool (HandlerInput AuthServer) AuthDbConn where
  getConnectionPool = getConnectionPool . view handlerInputServer
  {-# INLINE getConnectionPool #-}

instance HasClientConnection AuthClient where
  clientConnection = authClientConnection
  {-# INLINE clientConnection #-}

instance HasNetworkId AuthClient where
  networkId = clientConnection . networkId
  {-# INLINE networkId #-}

instance Show AuthClient where
  show = show . view authClientConnection
