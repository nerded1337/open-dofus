{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Spell.hs ---

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

module OpenDofus.Database.Game.Spell where

import Data.Binary
import Data.Text
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import OpenDofus.Database.Game.Effect
import OpenDofus.Database.Types
import OpenDofus.Prelude

newtype SpellId = SpellId
  { unSpellId :: Word32
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      Num,
      Real,
      Enum,
      Integral,
      Hashable,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data SpellLevelEffect = SpellLevelEffect
  { _spellLevelEffectType :: {-# UNPACK #-} !Word32,
    _spellLevelEffectParam1 :: !(Maybe Int32),
    _spellLevelEffectParam2 :: !(Maybe Int32),
    _spellLevelEffectParam3 :: !(Maybe Int32),
    _spellLevelEffectRemainingTurn :: {-# UNPACK #-} !Word32,
    _spellLevelEffectProbability :: {-# UNPACK #-} !Word32,
    _spellLevelEffectParam4 :: !(Maybe Text),
    _spellLevelEffectZone :: {-# UNPACK #-} !EffectZone
  }
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving (Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      FromBackendRow Postgres
    )
    via BinaryField SpellLevelEffect

makeClassy ''SpellLevelEffect

data SpellLevelT f = SpellLevel
  { _spellLevelSpell :: !(PrimaryKey SpellT f),
    _spellLevelLevel :: !(C f Word32),
    _spellLevelNormalEffect :: !(C f (PgArray SpellLevelEffect)), -- 0
    _spellLevelCriticalEffect :: !(C f (PgArray SpellLevelEffect)), -- 1
    _spellLevelApCost :: !(C f Word32), -- 2
    _spellLevelRangeMin :: !(C f Word32), -- 3
    _spellLevelRangeMax :: !(C f Word32), -- 4
    _spellLevelCriticalHit :: !(C f Word32), -- 5
    _spellLevelCriticalFailure :: !(C f Word32), -- 6
    _spellLevelLineOnly :: !(C f Bool), -- 7
    _spellLevelLineOfSight :: !(C f Bool), -- 8
    _spellLevelFreeCell :: !(C f Bool), -- 9
    _spellLevelCanBoostRange :: !(C f Bool), -- 10
    _spellLevelClassId :: !(C f Word32), -- 11
    _spellLevelLaunchCountByTurn :: !(C f Word32), -- 12
    _spellLevelLaunchCountByPlayerTurn :: !(C f Word32), -- 13
    _spellLevelDelayBetweenLaunch :: !(C f Word32), -- 14
    -- 15: LevelZones included in the level effect
    _spellLevelRequiredStates :: !(C f (PgArray Word32)), -- 16
    _spellLevelForbiddenStates :: !(C f (PgArray Word32)), -- 17
    _spellLevelMinPlayerLevel :: !(C f Word32), -- 18
    _spellLevelCriticalFailureEndsTurn :: !(C f Bool) -- 19
  }
  deriving (Generic, Beamable)

instance Table SpellLevelT where
  data PrimaryKey SpellLevelT f
    = SpellLevelPK
        !(PrimaryKey SpellT f)
        !(C f Word32)
    deriving (Generic, Beamable)
  primaryKey = SpellLevelPK <$> _spellLevelSpell <*> _spellLevelLevel
  {-# INLINE primaryKey #-}

type SpellLevel = SpellLevelT Identity

deriving instance Eq SpellLevel

deriving instance Show SpellLevel

type SpellLevelPK = PrimaryKey SpellLevelT Identity

deriving instance Eq SpellLevelPK

deriving instance Show SpellLevelPK

SpellLevel
  (SpellPK (LensFor spellLevelSpell))
  (LensFor spellLevelLevel)
  (LensFor spellLevelNormalEffect)
  (LensFor spellLevelCriticalEffect)
  (LensFor spellLevelApCost)
  (LensFor spellLevelRangeMin)
  (LensFor spellLevelRangeMax)
  (LensFor spellLevelCriticalHit)
  (LensFor spellLevelCriticalFailure)
  (LensFor spellLevelLineOnly)
  (LensFor spellLevelLineOfSight)
  (LensFor spellLevelFreeCell)
  (LensFor spellLevelCanBoostRange)
  (LensFor spellLevelClassId)
  (LensFor spellLevelLaunchCountByTurn)
  (LensFor spellLevelLaunchCountByPlayerTurn)
  (LensFor spellLevelDelayBetweenLaunch)
  (LensFor spellLevelRequiredStates)
  (LensFor spellLevelForbiddenStates)
  (LensFor spellLevelMinPlayerLevel)
  (LensFor spellLevelCriticalFailureEndsTurn) =
    tableLenses

data SpellT f = Spell
  { _spellId :: !(C f SpellId),
    _spellName :: !(C f Text),
    _spellDescription :: !(C f Text)
  }
  deriving (Generic, Beamable)

instance Table SpellT where
  data PrimaryKey SpellT f = SpellPK !(C f SpellId)
    deriving (Generic, Beamable)
  primaryKey = SpellPK . _spellId
  {-# INLINE primaryKey #-}

type Spell = SpellT Identity

deriving instance Eq Spell

deriving instance Show Spell

type SpellPK = PrimaryKey SpellT Identity

deriving instance Eq SpellPK

deriving instance Show SpellPK

Spell
  (LensFor spellId)
  (LensFor spellName)
  (LensFor spellDescription) =
    tableLenses
