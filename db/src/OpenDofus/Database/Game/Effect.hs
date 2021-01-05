{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Effect.hs ---

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

module OpenDofus.Database.Game.Effect where

import Data.Binary
import Data.List
import Data.Vector.Primitive (Prim)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import OpenDofus.Core.Data.Constructible
import OpenDofus.Database.Types
import OpenDofus.Prelude

newtype EffectId = EffectId
  { unEffectId :: Word32
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      Num,
      Real,
      Enum,
      Integral,
      Prim,
      Storable,
      Hashable,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data CharacteristicType
  = CharacteristicTypeNone
  | CharacteristicTypeLifePoints
  | CharacteristicTypeActionPoints
  | CharacteristicTypeGold
  | CharacteristicTypeStatsPoints
  | CharacteristicTypeSpellPoints
  | CharacteristicTypeLevel
  | CharacteristicTypeStrength
  | CharacteristicTypeVitality
  | CharacteristicTypeWisdom
  | CharacteristicTypeChance
  | CharacteristicTypeAgility
  | CharacteristicTypeIntelligence
  | CharacteristicTypeDamages
  | CharacteristicTypeDamagesFactor
  | CharacteristicTypeDamagesPercent
  | CharacteristicTypeCriticalHit
  | CharacteristicTypeRange
  | CharacteristicTypeDamagesMagicalReduction
  | CharacteristicTypeDamagesPhysicalReduction
  | CharacteristicTypeExperienceBoost
  | CharacteristicTypeMovementPoints
  | CharacteristicTypeInvisibility
  | CharacteristicTypeMaxSummonedCreaturesBoost
  | CharacteristicTypeDodgePaLostProbability
  | CharacteristicTypeDodgePmLostProbability
  | CharacteristicTypeEnergyPoints
  | CharacteristicTypeAlignment
  | CharacteristicTypeWeaponDamagesPercent
  | CharacteristicTypePhysicalDamages
  | CharacteristicTypeEarthElementPercent
  | CharacteristicTypeFireElementPercent
  | CharacteristicTypeWaterElementPercent
  | CharacteristicTypeAirElementPercent
  | CharacteristicTypeNeutralElementPercent
  | CharacteristicTypeGfx
  | CharacteristicTypeCriticalMiss
  | CharacteristicTypeInitiative
  | CharacteristicTypeProspection
  | CharacteristicTypeHealing
  | CharacteristicTypeDamagesReflect
  | CharacteristicTypeLoseEnergy
  | CharacteristicTypeHonourPoints
  | CharacteristicTypeDishonourPoints
  | CharacteristicTypeEarthElement
  | CharacteristicTypeFireElement
  | CharacteristicTypeWaterElement
  | CharacteristicTypeAirElement
  | CharacteristicTypeNeutralElement
  | CharacteristicTypeEarthElementPercentPVP
  | CharacteristicTypeFireElementPercentPVP
  | CharacteristicTypeWaterElementPercentPVP
  | CharacteristicTypeAirElementPercentPVP
  | CharacteristicTypeNeutralElementPercentPVP
  | CharacteristicTypeEarthElementPVP
  | CharacteristicTypeFireElementPVP
  | CharacteristicTypeWaterElementPVP
  | CharacteristicTypeAirElementPVP
  | CharacteristicTypeNeutralElementPVP
  | CharacteristicTypeTrapDamages
  | CharacteristicTypeTrapDamagesPercent
  | CharacteristicTypeState
  | CharacteristicTypeCaptureFactor
  | CharacteristicTypeMountExperienceFactor
  | CharacteristicTypeTimeConfusion
  | CharacteristicTypeDamagesPermanentFactor
  deriving stock
    ( Show,
      Eq,
      Ord,
      Read
    )
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField CharacteristicType)

instance Enum CharacteristicType where
  toEnum (-1) = CharacteristicTypeNone
  toEnum 0 = CharacteristicTypeLifePoints
  toEnum 1 = CharacteristicTypeActionPoints
  toEnum 2 = CharacteristicTypeGold
  toEnum 3 = CharacteristicTypeStatsPoints
  toEnum 4 = CharacteristicTypeSpellPoints
  toEnum 5 = CharacteristicTypeLevel
  toEnum 10 = CharacteristicTypeStrength
  toEnum 11 = CharacteristicTypeVitality
  toEnum 12 = CharacteristicTypeWisdom
  toEnum 13 = CharacteristicTypeChance
  toEnum 14 = CharacteristicTypeAgility
  toEnum 15 = CharacteristicTypeIntelligence
  toEnum 16 = CharacteristicTypeDamages
  toEnum 17 = CharacteristicTypeDamagesFactor
  toEnum 18 = CharacteristicTypeCriticalHit
  toEnum 19 = CharacteristicTypeRange
  toEnum 20 = CharacteristicTypeDamagesMagicalReduction
  toEnum 21 = CharacteristicTypeDamagesPhysicalReduction
  toEnum 22 = CharacteristicTypeExperienceBoost
  toEnum 23 = CharacteristicTypeMovementPoints
  toEnum 24 = CharacteristicTypeInvisibility
  toEnum 25 = CharacteristicTypeDamagesPercent
  toEnum 26 = CharacteristicTypeMaxSummonedCreaturesBoost
  toEnum 27 = CharacteristicTypeDodgePaLostProbability
  toEnum 28 = CharacteristicTypeDodgePmLostProbability
  toEnum 29 = CharacteristicTypeEnergyPoints
  toEnum 30 = CharacteristicTypeAlignment
  toEnum 31 = CharacteristicTypeWeaponDamagesPercent
  toEnum 32 = CharacteristicTypePhysicalDamages
  toEnum 33 = CharacteristicTypeEarthElementPercent
  toEnum 34 = CharacteristicTypeFireElementPercent
  toEnum 35 = CharacteristicTypeWaterElementPercent
  toEnum 36 = CharacteristicTypeAirElementPercent
  toEnum 37 = CharacteristicTypeNeutralElementPercent
  toEnum 38 = CharacteristicTypeGfx
  toEnum 39 = CharacteristicTypeCriticalMiss
  toEnum 44 = CharacteristicTypeInitiative
  toEnum 48 = CharacteristicTypeProspection
  toEnum 49 = CharacteristicTypeHealing
  toEnum 50 = CharacteristicTypeDamagesReflect
  toEnum 51 = CharacteristicTypeLoseEnergy
  toEnum 52 = CharacteristicTypeHonourPoints
  toEnum 53 = CharacteristicTypeDishonourPoints
  toEnum 54 = CharacteristicTypeEarthElement
  toEnum 55 = CharacteristicTypeFireElement
  toEnum 56 = CharacteristicTypeWaterElement
  toEnum 57 = CharacteristicTypeAirElement
  toEnum 58 = CharacteristicTypeNeutralElement
  toEnum 59 = CharacteristicTypeEarthElementPercentPVP
  toEnum 60 = CharacteristicTypeFireElementPercentPVP
  toEnum 61 = CharacteristicTypeWaterElementPercentPVP
  toEnum 62 = CharacteristicTypeAirElementPercentPVP
  toEnum 63 = CharacteristicTypeNeutralElementPercentPVP
  toEnum 64 = CharacteristicTypeEarthElementPVP
  toEnum 65 = CharacteristicTypeFireElementPVP
  toEnum 66 = CharacteristicTypeWaterElementPVP
  toEnum 67 = CharacteristicTypeAirElementPVP
  toEnum 68 = CharacteristicTypeNeutralElementPVP
  toEnum 69 = CharacteristicTypeTrapDamages
  toEnum 70 = CharacteristicTypeTrapDamagesPercent
  toEnum 71 = CharacteristicTypeState
  toEnum 72 = CharacteristicTypeCaptureFactor
  toEnum 73 = CharacteristicTypeMountExperienceFactor
  toEnum 74 = CharacteristicTypeTimeConfusion
  toEnum 75 = CharacteristicTypeDamagesPermanentFactor
  toEnum x = error $ "Unknow characteristic: " <> show x

  fromEnum CharacteristicTypeNone = -1
  fromEnum CharacteristicTypeLifePoints = 0
  fromEnum CharacteristicTypeActionPoints = 1
  fromEnum CharacteristicTypeGold = 2
  fromEnum CharacteristicTypeStatsPoints = 3
  fromEnum CharacteristicTypeSpellPoints = 4
  fromEnum CharacteristicTypeLevel = 5
  fromEnum CharacteristicTypeStrength = 10
  fromEnum CharacteristicTypeVitality = 11
  fromEnum CharacteristicTypeWisdom = 12
  fromEnum CharacteristicTypeChance = 13
  fromEnum CharacteristicTypeAgility = 14
  fromEnum CharacteristicTypeIntelligence = 15
  fromEnum CharacteristicTypeDamages = 16
  fromEnum CharacteristicTypeDamagesFactor = 17
  fromEnum CharacteristicTypeDamagesPercent = 25
  fromEnum CharacteristicTypeCriticalHit = 18
  fromEnum CharacteristicTypeRange = 19
  fromEnum CharacteristicTypeDamagesMagicalReduction = 20
  fromEnum CharacteristicTypeDamagesPhysicalReduction = 21
  fromEnum CharacteristicTypeExperienceBoost = 22
  fromEnum CharacteristicTypeMovementPoints = 23
  fromEnum CharacteristicTypeInvisibility = 24
  fromEnum CharacteristicTypeMaxSummonedCreaturesBoost = 26
  fromEnum CharacteristicTypeDodgePaLostProbability = 27
  fromEnum CharacteristicTypeDodgePmLostProbability = 28
  fromEnum CharacteristicTypeEnergyPoints = 29
  fromEnum CharacteristicTypeAlignment = 30
  fromEnum CharacteristicTypeWeaponDamagesPercent = 31
  fromEnum CharacteristicTypePhysicalDamages = 32
  fromEnum CharacteristicTypeEarthElementPercent = 33
  fromEnum CharacteristicTypeFireElementPercent = 34
  fromEnum CharacteristicTypeWaterElementPercent = 35
  fromEnum CharacteristicTypeAirElementPercent = 36
  fromEnum CharacteristicTypeNeutralElementPercent = 37
  fromEnum CharacteristicTypeGfx = 38
  fromEnum CharacteristicTypeCriticalMiss = 39
  fromEnum CharacteristicTypeInitiative = 44
  fromEnum CharacteristicTypeProspection = 48
  fromEnum CharacteristicTypeHealing = 49
  fromEnum CharacteristicTypeDamagesReflect = 50
  fromEnum CharacteristicTypeLoseEnergy = 51
  fromEnum CharacteristicTypeHonourPoints = 52
  fromEnum CharacteristicTypeDishonourPoints = 53
  fromEnum CharacteristicTypeEarthElement = 54
  fromEnum CharacteristicTypeFireElement = 55
  fromEnum CharacteristicTypeWaterElement = 56
  fromEnum CharacteristicTypeAirElement = 57
  fromEnum CharacteristicTypeNeutralElement = 58
  fromEnum CharacteristicTypeEarthElementPercentPVP = 59
  fromEnum CharacteristicTypeFireElementPercentPVP = 60
  fromEnum CharacteristicTypeWaterElementPercentPVP = 61
  fromEnum CharacteristicTypeAirElementPercentPVP = 62
  fromEnum CharacteristicTypeNeutralElementPercentPVP = 63
  fromEnum CharacteristicTypeEarthElementPVP = 64
  fromEnum CharacteristicTypeFireElementPVP = 65
  fromEnum CharacteristicTypeWaterElementPVP = 66
  fromEnum CharacteristicTypeAirElementPVP = 67
  fromEnum CharacteristicTypeNeutralElementPVP = 68
  fromEnum CharacteristicTypeTrapDamages = 69
  fromEnum CharacteristicTypeTrapDamagesPercent = 70
  fromEnum CharacteristicTypeState = 71
  fromEnum CharacteristicTypeCaptureFactor = 72
  fromEnum CharacteristicTypeMountExperienceFactor = 73
  fromEnum CharacteristicTypeTimeConfusion = 74
  fromEnum CharacteristicTypeDamagesPermanentFactor = 75

data EffectOperator
  = EffectOperatorPlus
  | EffectOperatorMinus
  | EffectOperatorDivide
  deriving
    ( Show,
      Eq,
      Ord,
      Read,
      Enum,
      Bounded
    )
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField EffectOperator)

data EffectType
  = EffectTypeDamage
  | EffectTypeHeal
  | EffectTypeSpecial
  deriving stock
    ( Show,
      Eq,
      Ord,
      Read,
      Enum,
      Bounded
    )
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField EffectType)

data EffectShape
  = EffectShapeCircle
  | EffectShapeLine
  | EffectShapeTLine
  | EffectShapeCross
  | EffectShapePoint
  | EffectShapeRing
  | EffectShapeRectangle
  | EffectShapeUnknownD
  deriving stock
    ( Show,
      Eq,
      Ord,
      Read,
      Enum,
      Bounded,
      Generic
    )
  deriving anyclass (Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField EffectShape)

data EffectSize
  = EffectSizeFinite Int
  | EffectSizeInfinite
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via BinaryField EffectSize

data EffectZone
  = EffectZone EffectShape EffectSize
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via BinaryField EffectZone

data EffectT f = Effect
  { _effectId :: !(C f EffectId),
    _effectType :: !(C f EffectType),
    _effectDescription :: !(C f Text),
    _effectHasJet :: !(C f Bool),
    _effectShowInTooltip :: !(C f Bool),
    _effectOperator :: !(C f (Maybe EffectOperator)),
    _effectCharacteristic :: !(C f CharacteristicType)
  }
  deriving (Generic, Beamable)

instance Table EffectT where
  data PrimaryKey EffectT f = EffectPK !(C f EffectId)
    deriving (Generic, Beamable)
  primaryKey = EffectPK . _effectId
  {-# INLINE primaryKey #-}

type EffectPK = PrimaryKey EffectT Identity

deriving instance Eq EffectPK

deriving instance Show EffectPK

type Effect = EffectT Identity

deriving instance Eq Effect

deriving instance Show Effect

Effect
  (LensFor effectId)
  (LensFor effectType)
  (LensFor effectDescription)
  (LensFor effectHasJet)
  (LensFor effectShowInTooltip)
  (LensFor effectOperator)
  (LensFor effectCharacteristic) =
    tableLenses

zoneFromPattern :: Text -> (EffectZone, Text)
zoneFromPattern x =
  let arr = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['-', '_']
      idx i =
        fromMaybe (error $ "Invalid zone size: " <> show i) $ elemIndex i arr
      getShape 'C' = EffectShapeCircle
      getShape 'X' = EffectShapeCross
      getShape 'L' = EffectShapeLine
      getShape 'T' = EffectShapeTLine
      getShape 'P' = EffectShapePoint
      getShape 'O' = EffectShapeRing
      getShape 'R' = EffectShapeRectangle
      getShape 'D' = EffectShapeUnknownD
      getShape s = error $ "Invalid zone shape: " <> show s
   in case x of
        (shape :- ('_' :- xs)) ->
          (EffectZone (getShape shape) EffectSizeInfinite, xs)
        (shape :- (s :- xs)) ->
          (EffectZone (getShape shape) (EffectSizeFinite $ idx s), xs)
        _ ->
          error $ "Invalid Effect Zone: " <> show x
{-# INLINE zoneFromPattern #-}
