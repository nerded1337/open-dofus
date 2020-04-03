-- Spell.hs ---

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
{-# OPTIONS_GHC -Wno-missing-signatures     #-}

module OpenDofus.Database.Game.Spell where

import           Data.Binary
import           Data.Text
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

import           OpenDofus.Database.Game.Effect
import           OpenDofus.Database.Types
import           OpenDofus.Prelude

newtype SpellId =
  SpellId
    { unSpellId :: Int
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

data SpellLevelEffect =
  SpellLevelEffect
    { _spellLevelEffectType          :: {-# UNPACK #-}!Int
    , _spellLevelEffectParam1        :: !(Maybe Int)
    , _spellLevelEffectParam2        :: !(Maybe Int)
    , _spellLevelEffectParam3        :: !(Maybe Int)
    , _spellLevelEffectRemainingTurn :: {-# UNPACK #-}!Int
    , _spellLevelEffectProbability   :: {-# UNPACK #-}!Int
    , _spellLevelEffectParam4        :: !(Maybe Text)
    , _spellLevelEffectZone          :: {-# UNPACK #-}!(EffectZone)
    }
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving (Binary)
  deriving ( ToField
           , FromField
           , HasDefaultSqlDataType Postgres
           , FromBackendRow Postgres
           ) via (BinaryField SpellLevelEffect)

makeClassy ''SpellLevelEffect

data SpellLevelT f =
  SpellLevel
    { _spellLevelSpell                   :: !(PrimaryKey SpellT f)
    , _spellLevelLevel                   :: !(C f Int)
    , _spellLevelNormalEffect            :: !(C f (PgArray SpellLevelEffect)) -- 0
    , _spellLevelCriticalEffect          :: !(C f (PgArray SpellLevelEffect)) -- 1
    , _spellLevelApCost                  :: !(C f Int) -- 2
    , _spellLevelRangeMin                :: !(C f Int) -- 3
    , _spellLevelRangeMax                :: !(C f Int) -- 4
    , _spellLevelCriticalHit             :: !(C f Int) -- 5
    , _spellLevelCriticalFailure         :: !(C f Int) -- 6
    , _spellLevelLineOnly                :: !(C f Bool) -- 7
    , _spellLevelLineOfSight             :: !(C f Bool) -- 8
    , _spellLevelFreeCell                :: !(C f Bool) -- 9
    , _spellLevelCanBoostRange           :: !(C f Bool) -- 10
    , _spellLevelClassId                 :: !(C f Int) -- 11
    , _spellLevelLaunchCountByTurn       :: !(C f Int) -- 12
    , _spellLevelLaunchCountByPlayerTurn :: !(C f Int) -- 13
    , _spellLevelDelayBetweenLaunch      :: !(C f Int) -- 14
    -- 15: LevelZones included in the level effect
    , _spellLevelRequiredStates          :: !(C f (PgArray Int)) -- 16
    , _spellLevelForbiddenStates         :: !(C f (PgArray Int)) -- 17
    , _spellLevelMinPlayerLevel          :: !(C f Int) -- 18
    , _spellLevelCriticalFailureEndsTurn :: !(C f Bool) -- 19
    }
  deriving (Generic, Beamable)

instance Table SpellLevelT where
  data PrimaryKey SpellLevelT f = SpellLevelId !(PrimaryKey SpellT f)
                                             !(C f Int)
                                  deriving (Generic, Beamable)
  primaryKey = SpellLevelId <$> _spellLevelSpell <*> _spellLevelLevel

type SpellLevel = SpellLevelT Identity
deriving instance Show SpellLevel

type SpellLevelId = PrimaryKey SpellLevelT Identity
deriving instance Show SpellLevelId

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
  (LensFor spellLevelCriticalFailureEndsTurn) = tableLenses

data SpellT f =
  Spell
    { _spellId          :: !(C f SpellId)
    , _spellName        :: !(C f Text)
    , _spellDescription :: !(C f Text)
    }
  deriving (Generic, Beamable)

instance Table SpellT where
  data PrimaryKey SpellT f = SpellPK !(C f SpellId)
                             deriving (Generic, Beamable)
  primaryKey = SpellPK . _spellId

type Spell = SpellT Identity
deriving instance Show Spell

type SpellPK = PrimaryKey SpellT Identity
deriving instance Show SpellPK
deriving instance Eq SpellPK
deriving instance Ord SpellPK

Spell
  (LensFor spellId)
  (LensFor spellName)
  (LensFor spellDescription) = tableLenses
