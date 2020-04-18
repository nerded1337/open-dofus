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

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module OpenDofus.Game.Map.Cell.Types
  ( Cell
  , CellT(..)
  , CellId(..)
  , HasCellT(..)
  , CellMovementType(..)
  )
where

import           Data.HashMap.Strict           as HM
import           OpenDofus.Database
import           OpenDofus.Game.Map.Actor
import           OpenDofus.Game.Map.Interactive
import           OpenDofus.Prelude

type Cell a
  = CellT (Maybe InteractiveObjectInstance) (HM.HashMap ActorId (GameActor a))

data CellMovementType = CellMovementTypeNonWalkable
    | CellMovementTypeWalkable
    | CellMovementTypeDoor
    | CellMovementTypePaddock
    | CellMovementTypePath
    | CellMovementTypeTrigger
    | CellMovementTypeUnknown6
    deriving stock (Show, Ord, Eq)

instance Enum CellMovementType where
  toEnum 0 = CellMovementTypeNonWalkable
  toEnum 1 = CellMovementTypeDoor
  toEnum 2 = CellMovementTypeTrigger
  toEnum 4 = CellMovementTypeWalkable
  toEnum 5 = CellMovementTypePaddock
  toEnum 6 = CellMovementTypeUnknown6
  toEnum 7 = CellMovementTypePath
  toEnum x = error $ "Invalid cell movement type: " <> show x
  fromEnum CellMovementTypeNonWalkable = 0
  fromEnum CellMovementTypeDoor        = 1
  fromEnum CellMovementTypeTrigger     = 2
  fromEnum CellMovementTypeWalkable    = 4
  fromEnum CellMovementTypePaddock     = 5
  fromEnum CellMovementTypeUnknown6    = 6
  fromEnum CellMovementTypePath        = 7

data CellT a b = Cell
    { _mapCellId                      :: {-# UNPACK #-} !CellId
    , _mapCellActive                  :: !Bool
    , _mapCellLineOfSight             :: !Bool
    , _mapCellLayerGroundRot          :: {-# UNPACK #-} !Word8
    , _mapCellGroundLevel             :: {-# UNPACK #-} !Word8
    , _mapCellMovement                :: !CellMovementType
    , _mapCellLayerGroundNum          :: {-# UNPACK #-} !Word16
    , _mapCellGroundSlope             :: {-# UNPACK #-} !Word8
    , _mapCellLayerGroundFlip         :: !Bool
    , _mapCellLayerObject1Num         :: {-# UNPACK #-} !Word16
    , _mapCellLayerObject1Rot         :: {-# UNPACK #-} !Word8
    , _mapCellLayerObject1Flip        :: !Bool
    , _mapCellLayerObject2Flip        :: !Bool
    , _mapCellLayerObject2Interactive :: !Bool
    , _mapCellLayerObject2Num         :: {-# UNPACK #-} !Word16
    , _mapCellInteractiveObjects      :: !a
    , _mapCellActors                  :: !b
    }
    deriving stock (Show, Functor, Foldable, Traversable)

makeClassy ''CellT

instance Bifunctor CellT where
  {-# INLINE bimap #-}
  bimap f g c = c
    { _mapCellInteractiveObjects = f $ _mapCellInteractiveObjects c
    , _mapCellActors             = g $ _mapCellActors c
    }

instance Bifoldable CellT where
  {-# INLINE bifoldMap #-}
  bifoldMap f g c =
    f (c ^. mapCellInteractiveObjects) <> g (c ^. mapCellActors)

instance Bitraversable CellT where
  {-# INLINE bitraverse #-}
  bitraverse f g c =
    let a' = f $ c ^. mapCellInteractiveObjects
        b' = g $ c ^. mapCellActors
        go x y = bimap (const x) (const y) c
    in  go <$> a' <*> b'
