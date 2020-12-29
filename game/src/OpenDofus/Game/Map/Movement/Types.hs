{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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

module OpenDofus.Game.Map.Movement.Types
  ( MovementModifier (..),
    HasMovementModifier (..),
    MovementStep (..),
    HasMovementStep (..),
    MovementPath (..),
    HasMovementPath (..),
  )
where

import qualified Data.DList.DNonEmpty as DL
import OpenDofus.Game.Map.Direction
import OpenDofus.Prelude

data MovementModifier
  = MovementModifierSlide
  | MovementModifierWalk
  | MovementModifierRun
  | MovementModifierMount
  deriving (Show, Eq)

makeClassy ''MovementModifier

data MovementStep a = MovementStep
  { _movementStepPoint :: !a,
    _movementStepDirection :: !Direction
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

makeClassy ''MovementStep

data MovementPath a = MovementPath
  { _movementPathCompressed :: {-# UNPACK #-} !ByteString,
    _movementPathSteps :: {-# UNPACK #-} !(DL.DNonEmpty (MovementStep a))
  }
  deriving stock (Show, Eq, Functor, Foldable)

makeClassy ''MovementPath

