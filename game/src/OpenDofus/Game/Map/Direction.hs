{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- Direction.hs ---

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

module OpenDofus.Game.Map.Direction
  ( Direction,
    HasDirection (..),
    pattern East,
    pattern SouthEast,
    pattern South,
    pattern SouthWest,
    pattern West,
    pattern NorthWest,
    pattern North,
    pattern NorthEast,
  )
where

import OpenDofus.Prelude

newtype Direction = Direction Word8
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Hashable
    )

makeClassy ''Direction

instance Enum Direction where
  fromEnum (Direction x) = fromIntegral x
  toEnum 0 = East
  toEnum 1 = SouthEast
  toEnum 2 = South
  toEnum 3 = SouthWest
  toEnum 4 = West
  toEnum 5 = NorthWest
  toEnum 6 = North
  toEnum 7 = NorthEast
  toEnum _ = error "The impossible happenned"

pattern East :: Direction
pattern East <-
  Direction 0
  where
    East = Direction 0

pattern SouthEast :: Direction
pattern SouthEast <-
  Direction 1
  where
    SouthEast = Direction 1

pattern South :: Direction
pattern South <-
  Direction 2
  where
    South = Direction 2

pattern SouthWest :: Direction
pattern SouthWest <-
  Direction 3
  where
    SouthWest = Direction 3

pattern West :: Direction
pattern West <-
  Direction 4
  where
    West = Direction 4

pattern NorthWest :: Direction
pattern NorthWest <-
  Direction 5
  where
    NorthWest = Direction 5

pattern North :: Direction
pattern North <-
  Direction 6
  where
    North = Direction 6

pattern NorthEast :: Direction
pattern NorthEast <-
  Direction 7
  where
    NorthEast = Direction 7
