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
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import OpenDofus.Database.Auth.Account
import OpenDofus.Database.Game.Breed
import OpenDofus.Database.Game.Effect
import OpenDofus.Database.Game.Map
import OpenDofus.Database.Game.Spell
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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype CharacterSex = CharacterSex
  { unCharacterSex :: Bool
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

data CharacterCharacteristicSource
  = Base
  | Gift
  | Bonus
  deriving stock
    ( Show,
      Eq,
      Ord,
      Bounded,
      Enum,
      Generic
    )

newtype CharacterCharacteristicValue = CharacterCharacteristicValue
  { unCharacteristicValue :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
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

data CharacterCharacteristicT f = CharacterCharacteristic
  { _characterCharacteristicCharacterId :: !(PrimaryKey CharacterT f),
    _characterCharacteristicEffectId :: !(PrimaryKey EffectT f),
    _characterCharacteristicSource :: !(C f CharacterCharacteristicSource),
    _characterCharacteristicValue :: !(C f CharacterCharacteristicValue)
  }
  deriving (Generic, Beamable)

instance Table CharacterCharacteristicT where
  data
    PrimaryKey
      CharacterCharacteristicT
      f
    = CharacterCharacteristicPK
        !(PrimaryKey CharacterT f)
        !(PrimaryKey EffectT f)
        !(C f CharacterCharacteristicSource)
    deriving (Generic, Beamable)
  primaryKey =
    CharacterCharacteristicPK
      <$> _characterCharacteristicCharacterId
      <*> _characterCharacteristicEffectId
      <*> _characterCharacteristicSource
  {-# INLINE primaryKey #-}

type CharacterCharacteristic = CharacterCharacteristicT Identity

deriving instance Eq CharacterCharacteristic

deriving instance Show CharacterCharacteristic

type CharacterCharacteristicPK = PrimaryKey CharacterCharacteristicT Identity

deriving instance Eq CharacterCharacteristicPK

deriving instance Show CharacterCharacteristicPK

CharacterCharacteristic
  (CharacterPK (LensFor characterCaracteristicCharacterId))
  (EffectPK (LensFor characterCaracteristicEffectId))
  (LensFor characterCaracteristicSource)
  (LensFor characterCaracteristicValue) =
    tableLenses

data CharacterSpellT f = CharacterSpell
  { _characterSpellCharacterId :: !(PrimaryKey CharacterT f),
    _characterSpellSpellLevel :: !(PrimaryKey SpellLevelT f)
  }
  deriving (Generic, Beamable)

instance Table CharacterSpellT where
  data PrimaryKey CharacterSpellT f
    = CharacterSpellPK
        !(PrimaryKey CharacterT f)
        !(PrimaryKey SpellLevelT f)
    deriving (Generic, Beamable)
  primaryKey =
    CharacterSpellPK
      <$> _characterSpellCharacterId
      <*> _characterSpellSpellLevel
  {-# INLINE primaryKey #-}

CharacterSpell
  (CharacterPK (LensFor characterSpellCharacterId))
  ( SpellLevelPK
      (SpellPK (LensFor characterSpellSpellId))
      (LensFor characterSpellSpellLevel)
    ) =
    tableLenses
