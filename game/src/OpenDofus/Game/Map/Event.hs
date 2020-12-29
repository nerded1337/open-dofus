{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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
  ( MapEvent (..),
    ActionStartParams,
  )
where

import Data.ByteString
import OpenDofus.Database.Game.Effect
import OpenDofus.Database.Game.Map
import OpenDofus.Game.Map.Actor
import OpenDofus.Prelude

type ActionStartParams = ByteString

data MapEvent
  = MapEventActorSpawn
      {-# UNPACK #-} !Actor
  | MapEventActorDespawn
      {-# UNPACK #-} !ActorId
  | MapEventDispatchInformations
      {-# UNPACK #-} !ActorId
  | MapEventActorActionStart
      {-# UNPACK #-} !ActorId
      {-# UNPACK #-} !EffectId
      {-# UNPACK #-} !ActionStartParams
  | MapEventActorActionAck
      {-# UNPACK #-} !ActorId
      {-# UNPACK #-} !EffectId
  | MapEventActorActionAbort
      {-# UNPACK #-} !ActorId
      {-# UNPACK #-} !EffectId
      {-# UNPACK #-} !CellId
  deriving stock (Show)
