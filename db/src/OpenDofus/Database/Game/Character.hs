{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Character.hs ---

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

module OpenDofus.Database.Game.Character where

import Data.Binary
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import OpenDofus.Database.Auth.Account
import OpenDofus.Database.Game.Breed
import OpenDofus.Database.Game.Effect
import OpenDofus.Database.Game.Map
import OpenDofus.Prelude

newtype CharacterId = CharacterId
  { unCharacterId :: Word64
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype CharacterName = CharacterName
  { unCharacterName :: Text
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      IsString,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype CharacterColor = CharacterColor
  { unCharacterColor :: Int32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype GfxId = GfxId
  { unGfxId :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype GfxSize = GfxSize
  { unGfxSize :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype CharacterSex = CharacterSex
  { unCharacterSex :: Bool
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data CharacterCaracteristicSource
  = Base
  | Gift
  | Bonus
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

newtype CharacterCaracteristicValue = CharacterCaracteristicValue
  { unCaracteristicValue :: Word32
  }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype CharacterLevel = CharacterLevel
  { unCharacterLevel :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype CharacterMaxLevel = CharacterMaxLevel
  { unCharacterMaxLevel :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype CharacterExperience = CharacterExperience
  { unCharacterExperience :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data CharacterT f = Character
  { _characterId :: !(C f CharacterId),
    _characterName :: !(C f CharacterName),
    _characterBreed :: !(PrimaryKey BreedT f),
    _characterAccountId :: !(PrimaryKey AccountT f),
    _characterLevel :: !(C f CharacterLevel),
    _characterMaxLevel :: !(C f CharacterMaxLevel),
    _characterExperience :: !(C f CharacterExperience)
  }
  deriving (Generic, Beamable)

instance Table CharacterT where
  data PrimaryKey CharacterT f = CharacterPK !(C f CharacterId)
    deriving (Generic, Beamable)
  primaryKey = CharacterPK . _characterId
  {-# INLINE primaryKey #-}

type Character = CharacterT Identity

deriving instance Eq Character

deriving instance Show Character

type CharacterPK = PrimaryKey CharacterT Identity

deriving instance Eq CharacterPK

deriving instance Show CharacterPK

Character
  (LensFor characterId)
  (LensFor characterName)
  (BreedPK (LensFor characterBreedId))
  (AccountPK (LensFor characterAccountId))
  (LensFor characterLevel)
  (LensFor characterMaxLevel)
  (LensFor characterExperience) =
    tableLenses

data CharacterPositionT f = CharacterPosition
  { _characterPositionCharacterId :: !(PrimaryKey CharacterT f),
    _characterPositionMapId :: !(PrimaryKey MapT f),
    _characterPositionCellId :: !(C f CellId)
  }
  deriving (Generic, Beamable)

instance Table CharacterPositionT where
  data
    PrimaryKey
      CharacterPositionT
      f
    = CharacterPositionPK !(PrimaryKey CharacterT f)
    deriving (Generic, Beamable)
  primaryKey = CharacterPositionPK . _characterPositionCharacterId
  {-# INLINE primaryKey #-}

type CharacterPosition = CharacterPositionT Identity

deriving instance Eq CharacterPosition

deriving instance Show CharacterPosition

type CharacterPositionPK = PrimaryKey CharacterPositionT Identity

deriving instance Eq CharacterPositionPK

deriving instance Show CharacterPositionPK

CharacterPosition
  (CharacterPK (LensFor characterPositionCharacterId))
  (MapPK (LensFor characterPositionMapId))
  (LensFor characterPositionCellId) =
    tableLenses

data CharacterLookT f = CharacterLook
  { _characterLookCharacterId :: !(PrimaryKey CharacterT f),
    _characterLookGfxId :: !(C f GfxId),
    _characterLookGfxSize :: !(C f GfxSize),
    _characterLookSex :: !(C f CharacterSex),
    _characterLookFirstColor :: !(C f CharacterColor),
    _characterLookSecondColor :: !(C f CharacterColor),
    _characterLookThirdColor :: !(C f CharacterColor)
  }
  deriving (Generic, Beamable)

instance Table CharacterLookT where
  data
    PrimaryKey
      CharacterLookT
      f
    = CharacterLookPK !(PrimaryKey CharacterT f)
    deriving (Generic, Beamable)
  primaryKey = CharacterLookPK . _characterLookCharacterId
  {-# INLINE primaryKey #-}

type CharacterLook = CharacterLookT Identity

deriving instance Eq CharacterLook

deriving instance Show CharacterLook

type CharacterLookPK = PrimaryKey CharacterLookT Identity

deriving instance Eq CharacterLookPK

deriving instance Show CharacterLookPK

CharacterLook
  (CharacterPK (LensFor characterLookCharacterId))
  (LensFor characterLookGfxId)
  (LensFor characterLookGfxSize)
  (LensFor characterLookSex)
  (LensFor characterLookFirstColor)
  (LensFor characterLookSecondColor)
  (LensFor characterLookThirdColor) =
    tableLenses

data CharacterCaracteristicT f = CharacterCaracteristic
  { _characterCaracteristicCharacterId :: !(PrimaryKey CharacterT f),
    _characterCaracteristicEffectId :: !(PrimaryKey EffectT f),
    _characterCaracteristicSource :: !(C f CharacterCaracteristicSource),
    _characterCaracteristicValue :: !(C f CharacterCaracteristicValue)
  }
  deriving (Generic, Beamable)

instance Table CharacterCaracteristicT where
  data
    PrimaryKey
      CharacterCaracteristicT
      f
    = CharacterCaracteristicPK
        !(PrimaryKey CharacterT f)
        !(PrimaryKey EffectT f)
        !(C f CharacterCaracteristicSource)
    deriving (Generic, Beamable)
  primaryKey =
    CharacterCaracteristicPK
      <$> _characterCaracteristicCharacterId
      <*> _characterCaracteristicEffectId
      <*> _characterCaracteristicSource
  {-# INLINE primaryKey #-}

type CharacterCaracteristic = CharacterCaracteristicT Identity

deriving instance Eq CharacterCaracteristic

deriving instance Show CharacterCaracteristic

type CharacterCaracteristicPK = PrimaryKey CharacterCaracteristicT Identity

deriving instance Eq CharacterCaracteristicPK

deriving instance Show CharacterCaracteristicPK

CharacterCaracteristic
  (CharacterPK (LensFor characterCaracteristicCharacterId))
  (EffectPK (LensFor characterCaracteristicEffectId))
  (LensFor characterCaracteristicSource)
  (LensFor characterCaracteristicValue) =
    tableLenses
