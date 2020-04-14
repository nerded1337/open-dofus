-- Skill.hs ---

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
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OpenDofus.Database.Game.Skill where

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax

import           OpenDofus.Database.Game.InteractiveObject
import           OpenDofus.Database.Game.Item
import           OpenDofus.Database.Game.Job
import           OpenDofus.Prelude

newtype SkillId =
  SkillId
    { unSkillId :: Int
    }
  deriving newtype ( Show
                   , Ord
                   , Eq
                   , Num
                   , Real
                   , Enum
                   , Integral
                   , FromBackendRow Postgres
                   , HasSqlEqualityCheck Postgres
                   , HasDefaultSqlDataType Postgres
                   , HasSqlValueSyntax PgValueSyntax
                   )

data SkillT f = Skill
    { _skillId                  :: !(C f SkillId)
    , _skillDescription         :: !(C f Text)
    , _skillJobId               :: !(PrimaryKey JobT f)
    , _skillInteractiveObjectId :: !(PrimaryKey InteractiveObjectT f)
    , _skillItemId              :: !(PrimaryKey ItemT (Nullable f))
    , _skillCriterion           :: !(C f (Maybe Text))
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table SkillT where
  data PrimaryKey SkillT f = SkillPK !(C f SkillId)
                             deriving stock (Generic)
                             deriving anyclass (Beamable)
  primaryKey = SkillPK . _skillId

type Skill = SkillT Identity

type SkillPK = PrimaryKey SkillT Identity
deriving instance Show SkillPK

Skill (LensFor skillId) (LensFor skillDescription) (JobPK (LensFor skillJobId)) (InteractiveObjectPK (LensFor skillInteractiveObjectId)) (ItemPK (LensFor skillItemId)) (LensFor skillCriterion)
  = tableLenses

data SkillCraftT f = SkillCraft
    { _skillCraftSkillId :: !(PrimaryKey SkillT f)
    , _skillCraftItemId  :: !(PrimaryKey ItemT f)
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table SkillCraftT where
  data PrimaryKey SkillCraftT f = SkillCraftPK !(PrimaryKey SkillT f)
                                               !(PrimaryKey ItemT f)
                                  deriving stock (Generic)
                                  deriving anyclass (Beamable)
  primaryKey = SkillCraftPK <$> _skillCraftSkillId <*> _skillCraftItemId

type SkillCraft = SkillCraftT Identity
deriving instance Show SkillCraft

type SkillCraftPK = PrimaryKey SkillCraftT Identity
deriving instance Show SkillCraftPK

SkillCraft (SkillPK (LensFor skillCraftSkillId)) (ItemPK (LensFor skillCraftItemId))
  = tableLenses
