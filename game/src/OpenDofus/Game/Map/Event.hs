{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStrict #-}

-- Event.hs ---

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

module OpenDofus.Game.Map.Event
  ( MapEventChannels (..),
    HasMapEventChannels (..),
    MapEvent (..),
    MapEventVariant,
    MapEventActorAction,
    MapEventActorSpawnType,
    MapEventActorSpawn (..),
    MapEventActorDespawn (..),
    MapEventDispatchInformations (..),
    MapEventActorActionStart (..),
    MapEventActorActionStartMovement (..),
    MapEventActorActionAbort (..),
    MapEventActorActionAck (..),
    EffectParams,
    pattern MapEventActorSpawnTypePlayer,
  )
where

import Control.Concurrent.Chan.Unagi.NoBlocking.Unboxed
import Data.ByteString
import Data.Primitive (Prim)
import OpenDofus.Core.Data.PrimVector ()
import OpenDofus.Core.Data.Record
import OpenDofus.Database.Game.Effect
import OpenDofus.Database.Game.Map
import OpenDofus.Game.Map.Actor
import OpenDofus.Prelude

type EffectParams = ByteString

type MapEventVariant =
  MapEventActorSpawn
    :<+>: MapEventActorDespawn
    :<+>: MapEventDispatchInformations
    :<+>: MapEventActorActionStart
    :<+>: MapEventActorActionAbort
    :<+>: MapEventActorActionAck

newtype MapEvent = MapEvent MapEventVariant
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEvent

instance UnagiPrim MapEvent

type MapEventActorAction =
  MapEventActorActionStartMovement :<+>: Word8

newtype MapEventActorSpawnType
  = MapEventActorSpawnType Word8
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorSpawnType

newtype MapEventActorSpawn
  = MapEventActorSpawn (MapEventActorSpawnType :<*>: ActorId)
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorSpawn

newtype MapEventActorDespawn
  = MapEventActorDespawn ActorId
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorDespawn

newtype MapEventDispatchInformations
  = MapEventDispatchInformations ActorId
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventDispatchInformations

newtype MapEventActorActionStartMovement
  = MapEventActorActionStartMovement SerializedMovementPath
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorActionStartMovement

newtype MapEventActorActionStart
  = MapEventActorActionStart (ActorId :<*>: MapEventActorAction)
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorActionStart

newtype MapEventActorActionAck
  = MapEventActorActionAck (ActorId :<*>: EffectId)
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorActionAck

newtype MapEventActorActionAbort
  = MapEventActorActionAbort (ActorId :<*>: CellId)
  deriving newtype (Show, Eq, Storable)
  deriving Prim via StorablePrim MapEventActorActionAbort

pattern MapEventActorSpawnTypePlayer :: MapEventActorSpawnType
pattern MapEventActorSpawnTypePlayer <-
  MapEventActorSpawnType 0
  where
    MapEventActorSpawnTypePlayer = MapEventActorSpawnType 0

data MapEventChannels = MapEventChannels
  { _mapEventChannelsIn :: {-# UNPACK #-} !(InChan MapEvent),
    _mapEventChannelsOut :: {-# UNPACK #-} !(OutChan MapEvent)
  }

makeClassy ''MapEventChannels
