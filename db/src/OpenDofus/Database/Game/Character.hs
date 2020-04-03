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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OpenDofus.Database.Game.Character where

import           Data.Binary
import           Database.Beam
import           OpenDofus.Database.Auth.Account
import           OpenDofus.Database.Game.Breed
import           OpenDofus.Database.Game.Effect
import           OpenDofus.Database.Game.Map
import           OpenDofus.Prelude

newtype CharacterId =
  CharacterId
    { unCharacterId :: Word64
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype CharacterName =
  CharacterName
    { unCharacterName :: Text
    }
  deriving newtype (Show, Eq, Ord, IsString)

newtype CharacterColor =
  CharacterColor
    { unCharacterColor :: Int
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype GfxId =
  GfxId
    { unGfxId :: Word
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype GfxSize =
  GfxSize
    { unSkinSize :: Word
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype CharacterSex =
  CharacterSex
    { unCharacterSex :: Bool
    }
  deriving newtype (Show, Eq, Ord)

data CharacterCaracteristicSource = Base
    | Gift
    | Bonus
    deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

newtype CharacterCaracteristicValue =
  CharacterCaracteristicValue
    { unCaracteristicValue :: Word
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype CharacterLevel =
  CharacterLevel
    { unCharacterLevel :: Word
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype CharacterMaxLevel =
  CharacterMaxLevel
    { unCharacterMaxLevel :: Word
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

newtype CharacterExperience =
  CharacterExperience
    { unCharacterExperience :: Word
    }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

data CharacterT f = Character
    { _characterId         :: !(C f CharacterId)
    , _characterName       :: !(C f CharacterName)
    , _characterBreed      :: !(PrimaryKey BreedT f)
    , _characterAccountId  :: !(PrimaryKey AccountT f)
    , _characterLevel      :: !(C f CharacterLevel)
    , _characterMaxLevel   :: !(C f CharacterMaxLevel)
    , _characterExperience :: !(C f CharacterExperience)
    }
    deriving (Generic, Beamable)

instance Table CharacterT where
  data PrimaryKey CharacterT f = CharacterPK !(C f CharacterId)
                               deriving (Generic, Beamable)
  primaryKey = CharacterPK . _characterId

type Character = CharacterT Identity
deriving instance Show Character

type CharacterPK = PrimaryKey CharacterT Identity
deriving instance Show CharacterPK

Character
  (LensFor characterId)
  (LensFor characterName)
  (BreedPK (LensFor characterBreedId))
  (AccountPK (LensFor characterAccountId))
  (LensFor characterLevel)
  (LensFor characterMaxLevel)
  (LensFor characterExperience)
  = tableLenses

data CharacterPositionT f = CharacterPosition
    { _characterPositionCharacterId :: !(PrimaryKey CharacterT f)
    , _characterPositionMapId       :: !(PrimaryKey MapT f)
    , _characterPositionCellId      :: !(C f CellId)
    }
    deriving (Generic, Beamable)

instance Table CharacterPositionT where
  data PrimaryKey CharacterPositionT
       f = CharacterPositionPK !(PrimaryKey CharacterT f)
             deriving (Generic, Beamable)
  primaryKey = CharacterPositionPK . _characterPositionCharacterId

type CharacterPosition = CharacterPositionT Identity
deriving instance Show CharacterPosition

type CharacterPositionPK = PrimaryKey CharacterPositionT Identity
deriving instance Show CharacterPositionPK

CharacterPosition
  (CharacterPK (LensFor characterPositionCharacterId))
  (MapPK (LensFor characterPositionMapId))
  (LensFor characterPositionCellId)
  = tableLenses

data CharacterLookT f = CharacterLook
    { _characterLookCharacterId :: !(PrimaryKey CharacterT f)
    , _characterLookGfxId       :: !(C f GfxId)
    , _characterLookGfxSize     :: !(C f GfxSize)
    , _characterLookSex         :: !(C f CharacterSex)
    , _characterLookFirstColor  :: !(C f CharacterColor)
    , _characterLookSecondColor :: !(C f CharacterColor)
    , _characterLookThirdColor  :: !(C f CharacterColor)
    }
    deriving (Generic, Beamable)

instance Table CharacterLookT where
  data PrimaryKey CharacterLookT
       f = CharacterLookPK !(PrimaryKey CharacterT f)
             deriving (Generic, Beamable)
  primaryKey = CharacterLookPK . _characterLookCharacterId

type CharacterLook = CharacterLookT Identity
deriving instance Show CharacterLook

type CharacterLookPK = PrimaryKey CharacterLookT Identity
deriving instance Show CharacterLookPK

CharacterLook
  (CharacterPK (LensFor characterLookCharacterId))
  (LensFor characterLookGfxId)
  (LensFor characterLookGfxSize)
  (LensFor characterLookSex)
  (LensFor characterFirstColor)
  (LensFor characterSecondColor)
  (LensFor characterThirdColor)
  = tableLenses

data CharacterCaracteristicT f = CharacterCaracteristic
    { _characterCaracteristicCharacterId :: !(PrimaryKey CharacterT f)
    , _characterCaracteristicEffectId    :: !(PrimaryKey EffectT f)
    , _characterCaracteristicSource      :: !(C f CharacterCaracteristicSource)
    , _characterCaracteristicValue       :: !(C f CharacterCaracteristicValue)
    }
    deriving (Generic, Beamable)

instance Table CharacterCaracteristicT where
  data PrimaryKey CharacterCaracteristicT
       f = CharacterCharacteristicPK !(PrimaryKey CharacterT f)
                                     !(PrimaryKey EffectT f)
             deriving (Generic, Beamable)
  primaryKey =
    CharacterCharacteristicPK <$> _characterCaracteristicCharacterId <*>
    _characterCaracteristicEffectId

type CharacterCaracteristic = CharacterCaracteristicT Identity
deriving instance Show CharacterCaracteristic

type CharacterCharacteristicPK = PrimaryKey CharacterCaracteristicT Identity
deriving instance Show CharacterCharacteristicPK

CharacterCaracteristic
  (CharacterPK (LensFor characterCaracteristicCharacterId))
  (EffectPK (LensFor characterCaracteristicEffectId))
  (LensFor characterCaracteristicSource)
  (LensFor characterCaracteristicValue)
  = tableLenses
