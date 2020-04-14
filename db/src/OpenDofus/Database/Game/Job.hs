-- Job.hs ---

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

module OpenDofus.Database.Game.Job where

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           OpenDofus.Prelude

newtype JobId =
  JobId
    { unJobId :: Int
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

data JobT f = Job
    { _jobId       :: !(C f JobId)
    , _jobSubJobId :: !(PrimaryKey JobT (Nullable f))
    , _jobName     :: !(C f Text)
    }
    deriving stock (Generic)
    deriving anyclass (Beamable)

instance Table JobT where
  data PrimaryKey JobT f = JobPK (C f JobId)
                           deriving stock (Generic)
                           deriving anyclass (Beamable)
  primaryKey = JobPK . _jobId

type Job = JobT Identity

type JobPK = PrimaryKey JobT Identity
deriving instance Show JobPK

Job (LensFor jobId) (JobPK (LensFor jobSubJobId)) (LensFor jobName) =
  tableLenses
