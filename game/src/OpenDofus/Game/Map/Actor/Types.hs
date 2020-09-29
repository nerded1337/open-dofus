-- Types.hs ---

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

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module OpenDofus.Game.Map.Actor.Types
  ( ActorId(..)
  , Direction(..)
  , HasActorId(..)
  , HasPosition(..)
  , HasDirection(..)
  , HasController(..)
  , module X
  )
where

import           OpenDofus.Database
import           OpenDofus.Game.Map.Actor.Restriction
                                               as X
import           OpenDofus.Prelude

data Direction = East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest
    | North
    | NorthEast
    deriving (Show, Eq, Ord, Bounded, Enum)

newtype ActorId =
  ActorId
    { unActorId :: Word64
    }
  deriving newtype ( Show
                   , Eq
                   , Ord
                   , Num
                   , Real
                   , Enum
                   , Integral
                   , Hashable
                   )

class HasActorId a where
  actorId :: a -> ActorId

class HasPosition a where
  position :: a -> (MapId, CellId)

class HasDirection a where
  direction :: a -> Direction

class HasController a b where
  controller :: a -> b
