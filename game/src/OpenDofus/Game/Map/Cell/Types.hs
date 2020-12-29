{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

module OpenDofus.Game.Map.Cell.Types
  ( Cell,
    CellT (..),
    CellId (..),
    HasCellT (..),
    CellMovementType (..),
    pattern CellMovementTypeDoor,
    pattern CellMovementTypeTrigger,
    pattern CellMovementTypeWalkableWeak,
    pattern CellMovementTypeWalkable,
    pattern CellMovementTypePaddock,
    pattern CellMovementTypeRoad,
    pattern CellMovementTypeWalkableFast,
    cellMovementType,
  )
where

import OpenDofus.Database
import OpenDofus.Game.Map.Interactive
import OpenDofus.Prelude

type Cell a = CellT (Maybe InteractiveObjectInstance)

newtype CellMovementType = CellMovementType
  { unCellMovementType :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral
    )

pattern CellMovementTypeNonWalkable :: CellMovementType
pattern CellMovementTypeNonWalkable <-
  0
  where
    CellMovementTypeNonWalkable = 0

pattern CellMovementTypeDoor :: CellMovementType
pattern CellMovementTypeDoor <-
  1
  where
    CellMovementTypeDoor = 1

pattern CellMovementTypeTrigger :: CellMovementType
pattern CellMovementTypeTrigger <-
  2
  where
    CellMovementTypeTrigger = 2

pattern CellMovementTypeWalkableWeak :: CellMovementType
pattern CellMovementTypeWalkableWeak <-
  3
  where
    CellMovementTypeWalkableWeak = 3

pattern CellMovementTypeWalkable :: CellMovementType
pattern CellMovementTypeWalkable <-
  4
  where
    CellMovementTypeWalkable = 4

pattern CellMovementTypePaddock :: CellMovementType
pattern CellMovementTypePaddock <-
  5
  where
    CellMovementTypePaddock = 5

pattern CellMovementTypeRoad :: CellMovementType
pattern CellMovementTypeRoad <-
  6
  where
    CellMovementTypeRoad = 6

pattern CellMovementTypeWalkableFast :: CellMovementType
pattern CellMovementTypeWalkableFast <-
  7
  where
    CellMovementTypeWalkableFast = 7

cellMovementType :: Word32 -> CellMovementType
cellMovementType 0 = CellMovementTypeNonWalkable
cellMovementType 1 = CellMovementTypeDoor
cellMovementType 2 = CellMovementTypeTrigger
cellMovementType 3 = CellMovementTypeWalkableWeak
cellMovementType 4 = CellMovementTypeWalkable
cellMovementType 5 = CellMovementTypePaddock
cellMovementType 6 = CellMovementTypeRoad
cellMovementType 7 = CellMovementTypeWalkableFast
cellMovementType x = error $ "Cell walk type: " <> show x

data CellT a = Cell
  { _cellId :: {-# UNPACK #-} !CellId,
    _cellActive :: {-# UNPACK #-} !FastBool,
    _cellLineOfSight :: {-# UNPACK #-} !FastBool,
    _cellLayerGroundRot :: {-# UNPACK #-} !Word8,
    _cellGroundLevel :: {-# UNPACK #-} !Word8,
    _cellMovement :: {-# UNPACK #-} !CellMovementType,
    _cellLayerGroundNum :: {-# UNPACK #-} !Word16,
    _cellGroundSlope :: {-# UNPACK #-} !Word8,
    _cellLayerGroundFlip :: {-# UNPACK #-} !FastBool,
    _cellLayerObject1Num :: {-# UNPACK #-} !Word16,
    _cellLayerObject1Rot :: {-# UNPACK #-} !Word8,
    _cellLayerObject1Flip :: {-# UNPACK #-} !FastBool,
    _cellLayerObject2Flip :: {-# UNPACK #-} !FastBool,
    _cellLayerObject2Interactive :: {-# UNPACK #-} !FastBool,
    _cellLayerObject2Num :: {-# UNPACK #-} !Word16,
    _cellInteractiveObject :: !a
  }
  deriving stock (Show, Functor, Foldable, Traversable)

makeClassy ''CellT
