{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Breed.hs ---

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

module OpenDofus.Database.Game.Breed where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import OpenDofus.Database.Game.Spell
import OpenDofus.Prelude

newtype BreedId = BreedId
  { unBreedId :: Word32
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data BreedCaracteristicCostT f = BreedCaracteristicCost
  { _breedCaracteristicCostBreed :: !(PrimaryKey BreedT f),
    _breedCaracteristicCostElement :: !(C f Word32),
    _breedCaracteristicCostFloor :: !(C f Word32),
    _breedCaracteristicCostValue :: !(C f Word32),
    _breedCaracteristicCostBonus :: !(C f Word32)
  }
  deriving (Generic, Beamable)

instance Table BreedCaracteristicCostT where
  data
    PrimaryKey
      BreedCaracteristicCostT
      f
    = BreedCaracteristicCostPK
        !(PrimaryKey BreedT f)
        !(C f Word32)
        !(C f Word32)
    deriving (Generic, Beamable)
  primaryKey =
    BreedCaracteristicCostPK
      <$> _breedCaracteristicCostBreed
      <*> _breedCaracteristicCostElement
      <*> _breedCaracteristicCostFloor
  {-# INLINE primaryKey #-}

type BreedCaracteristicCost = BreedCaracteristicCostT Identity

deriving instance Eq BreedCaracteristicCost

deriving instance Show BreedCaracteristicCost

type BreedCaracteristicCostPK = PrimaryKey BreedCaracteristicCostT Identity

deriving instance Eq BreedCaracteristicCostPK

deriving instance Show BreedCaracteristicCostPK

BreedCaracteristicCost
  (BreedPK (LensFor characteristicCostBred))
  (LensFor characteristicCostElement)
  (LensFor characteristicCostFloor)
  (LensFor characteristicCostValue)
  (LensFor characteristicCostBoost) =
    tableLenses

data BreedSpellT f = BreedSpell
  { _breedSpellBreed :: !(PrimaryKey BreedT f),
    _breedSpellSpell :: !(PrimaryKey SpellT f)
  }
  deriving (Generic, Beamable)

instance Table BreedSpellT where
  data PrimaryKey BreedSpellT f
    = BreedSpellPK
        !(PrimaryKey BreedT f)
        !(PrimaryKey SpellT f)
    deriving (Generic, Beamable)
  primaryKey =
    BreedSpellPK <$> _breedSpellBreed <*> _breedSpellSpell
  {-# INLINE primaryKey #-}

type BreedSpell = BreedSpellT Identity

deriving instance Eq BreedSpell

deriving instance Show BreedSpell

type BreedSpellPK = PrimaryKey BreedSpellT Identity

deriving instance Eq BreedSpellPK

deriving instance Show BreedSpellPK

BreedSpell
  (BreedPK (LensFor breedSpellBreed))
  (SpellPK (LensFor breedSpellSpell)) =
    tableLenses

data BreedT f = Breed
  { _breedId :: !(C f BreedId),
    _breedSmallName :: !(C f Text),
    _breedLongName :: !(C f Text),
    _breedSmallDescription :: !(C f Text),
    _breedDescription :: !(C f Text)
  }
  deriving (Generic, Beamable)

instance Table BreedT where
  data PrimaryKey BreedT f = BreedPK !(C f BreedId)
    deriving (Generic, Beamable)
  primaryKey = BreedPK . _breedId
  {-# INLINE primaryKey #-}

type Breed = BreedT Identity

deriving instance Eq Breed

deriving instance Show Breed

type BreedPK = PrimaryKey BreedT Identity

deriving instance Eq BreedPK

deriving instance Show BreedPK

Breed
  (LensFor breedId)
  (LensFor breedSmallName)
  (LensFor breedLongName)
  (LensFor breedSmallDescription)
  (LensFor breedDescription) =
    tableLenses
