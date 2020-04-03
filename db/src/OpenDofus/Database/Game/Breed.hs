-- Breed.hs ---

-- Copyright (C) 2019 Nerd Ed

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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -Wno-missing-signatures     #-}

module OpenDofus.Database.Game.Breed where

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           OpenDofus.Database.Game.Spell
import           OpenDofus.Prelude

newtype BreedId =
  BreedId
    { unBreedId :: Int
    }
  deriving newtype ( Show
                   , Ord
                   , Eq
                   , Num
                   , Real
                   , Enum
                   , Integral
                   , HasDefaultSqlDataType Postgres
                   , HasSqlValueSyntax PgValueSyntax
                   )

data BreedCharacteristicCostT f =
  BreedCharacteristicCost
    { _breedCharacteristicCostBreed   :: !(PrimaryKey BreedT f)
    , _breedCharacteristicCostElement :: !(C f Int)
    , _breedCharacteristicCostFloor   :: !(C f Int)
    , _breedCharacteristicCostValue   :: !(C f Int)
    , _breedCharacteristicCostBonus   :: !(C f Int)
    }
  deriving (Generic, Beamable)

instance Table BreedCharacteristicCostT where
  data PrimaryKey BreedCharacteristicCostT
       f = BreedCharacteristicCostId !(PrimaryKey BreedT f) !(C f Int)
                                     !(C f Int)
             deriving (Generic, Beamable)
  primaryKey =
    BreedCharacteristicCostId <$> _breedCharacteristicCostBreed <*>
    _breedCharacteristicCostElement <*>
    _breedCharacteristicCostFloor

type BreedCharacteristicCost = BreedCharacteristicCostT Identity

BreedCharacteristicCost
  (BreedPK (LensFor characteristicCostBred))
  (LensFor characteristicCostElement)
  (LensFor characteristicCostFloor)
  (LensFor characteristicCostValue)
  (LensFor characteristicCostBoost) = tableLenses

data BreedSpellT f =
  BreedSpell
    { _breedSpellBreed :: !(PrimaryKey BreedT f)
    , _breedSpellSpell :: !(PrimaryKey SpellT f)
    }
  deriving (Generic, Beamable)

instance Table BreedSpellT where
  data PrimaryKey BreedSpellT f = BreedSpellId !(PrimaryKey BreedT f)
                                             !(PrimaryKey SpellT f)
                                  deriving (Generic, Beamable)
  primaryKey = BreedSpellId <$> _breedSpellBreed <*> _breedSpellSpell

type BreedSpell = BreedSpellT Identity

BreedSpell
  (BreedPK (LensFor breedSpellBreed))
  (SpellPK (LensFor breedSpellSpell)) = tableLenses

data BreedT f =
  Breed
    { _breedId               :: !(C f BreedId)
    , _breedSmallName        :: !(C f Text)
    , _breedLongName         :: !(C f Text)
    , _breedSmallDescription :: !(C f Text)
    , _breedDescription      :: !(C f Text)
    }
  deriving (Generic, Beamable)

type Breed = BreedT Identity
deriving instance Show Breed

type BreedPK = PrimaryKey BreedT Identity
deriving instance Show BreedPK

instance Table BreedT where
  data PrimaryKey BreedT f = BreedPK !(C f BreedId)
                             deriving (Generic, Beamable)
  primaryKey = BreedPK . _breedId

Breed
  (LensFor breedId)
  (LensFor breedSmallName)
  (LensFor breedLongName)
  (LensFor breedSmallDescription)
  (LensFor breedDescription) = tableLenses
