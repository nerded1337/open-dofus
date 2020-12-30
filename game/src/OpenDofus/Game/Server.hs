{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

module OpenDofus.Game.Server
  ( GameClient (..),
    HasGameClient (..),
    GameServer (..),
    HasGameServer (..),
    GameHandler,
    GameState (..),
  )
where

import Data.Compact
import qualified Data.HashTable.IO as H
import OpenDofus.Core.Network.Client
  ( ClientConnection,
    HasClientConnection (clientConnection),
  )
import OpenDofus.Core.Network.Server
  ( ClientHandler,
    HandlerInput,
    HasClientType (..),
    HasMessageType (..),
    HasServerState (..),
    ServerState,
    handlerInputServer,
  )
import OpenDofus.Core.Network.Types (HasNetworkId (..))
import OpenDofus.Database
  ( Account,
    AuthDbConn (AuthDbConn),
    GameDbConn (GameDbConn),
    HasConnectPool (..),
    MapId,
    MapSubAreaId,
    Pool,
    WorldId,
  )
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Types
  (MapEventChannels,  MapController,
  )
import OpenDofus.Game.Network.Message
import OpenDofus.Prelude
import qualified StmContainers.Map as M

type HashTable k v = H.BasicHashTable k v

data GameState
  = Greeting
  | CharacterSelection
      {-# UNPACK #-} !Account
  | GameCreation
      {-# UNPACK #-} !Account
      {-# UNPACK #-} !PlayerCharacter
  | InGame
      {-# UNPACK #-} !Account
      {-# UNPACK #-} !ActorId

makePrisms ''GameState

data GameClient = GameClient
  { _gameClientState :: {-# UNPACK #-} !(IORef GameState),
    _gameClientConnection :: {-# UNPACK #-} !ClientConnection
  }

makeClassy ''GameClient

instance Show GameClient where
  show = show . view gameClientConnection

data GameServer = GameServer
  { _gameServerState :: {-# UNPACK #-} !(ServerState IO GameClient),
    _gameServerAuthDbPool :: {-# UNPACK #-} !(Pool AuthDbConn),
    _gameServerGameDbPool :: {-# UNPACK #-} !(Pool GameDbConn),
    _gameServerWorldId :: {-# UNPACK #-} !WorldId,
    _gameServerMapControllers :: {-# UNPACK #-}!(HashTable MapId MapController),
    _gameServerMapChannels :: {-# UNPACK #-}!(HashTable MapId MapEventChannels),
    _gameServerPlayerToClient :: {-# UNPACK #-} !(M.Map ActorId GameClient),
    _gameServerPlayerToMap :: {-# UNPACK #-} !(M.Map ActorId MapId),
    _gameServerCompact :: {-# UNPACK #-} !(Compact ())
  }

makeClassy ''GameServer

type GameHandler a = ClientHandler IO GameServer a

instance HasClientType GameServer where
  type ClientTypeOf GameServer = GameClient

instance HasMessageType GameServer where
  type MessageTypeOf GameServer = GameMessage

instance HasConnectPool GameServer GameDbConn where
  getConnectionPool = view gameServerGameDbPool
  {-# INLINE getConnectionPool #-}

instance HasConnectPool GameServer AuthDbConn where
  getConnectionPool = view gameServerAuthDbPool
  {-# INLINE getConnectionPool #-}

instance HasConnectPool (HandlerInput GameServer) AuthDbConn where
  getConnectionPool = getConnectionPool . view handlerInputServer
  {-# INLINE getConnectionPool #-}

instance HasConnectPool (HandlerInput GameServer) GameDbConn where
  getConnectionPool = getConnectionPool . view handlerInputServer
  {-# INLINE getConnectionPool #-}

instance HasServerState GameServer IO GameClient where
  serverState = gameServerState
  {-# INLINE serverState #-}

instance HasClientConnection GameClient where
  clientConnection = gameClientConnection
  {-# INLINE clientConnection #-}

instance HasNetworkId GameClient where
  networkId = clientConnection . networkId
  {-# INLINE networkId #-}
