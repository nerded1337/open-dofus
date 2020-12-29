-- GameServer.hs ---
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OpenDofus.Database.Auth.WorldServer where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import OpenDofus.Database.Types
import OpenDofus.Prelude

newtype WorldId = WorldId
  { unWorldId :: Word32
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Real,
      Enum,
      Integral,
      Read,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data WorldCompletion
  = WorldCompletionLow
  | WorldCompletionMedium
  | WorldCompletionHigh
  deriving (Show, Eq, Ord, Read, Enum, Bounded)
  deriving
    ( ToField,
      FromField,
      FromBackendRow Postgres,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField WorldCompletion)

data WorldStatus
  = WorldStatusOffline
  | WorldStatusOnline
  | WorldStatusStarting
  deriving (Show, Eq, Ord, Read, Enum, Bounded)
  deriving
    ( ToField,
      FromField,
      FromBackendRow Postgres,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField WorldStatus)

data WorldServerT f = WorldServer
  { _worldServerId :: !(C f WorldId),
    _worldServerCompletion :: !(C f WorldCompletion),
    _worldServerStatus :: !(C f WorldStatus),
    _worldServerIP :: !(C f IP),
    _worldServerPort :: !(C f Port)
  }
  deriving (Generic, Beamable)

WorldServer (LensFor worldServerId) (LensFor worldServerCompletion) (LensFor worldServerStatus) (LensFor worldServerIP) (LensFor worldServerPort) =
  tableLenses

instance Table WorldServerT where
  data PrimaryKey WorldServerT f = WorldServerPK !(C f WorldId)
    deriving (Generic, Beamable)
  primaryKey = WorldServerPK . _worldServerId
  {-# INLINE primaryKey #-}

type WorldServer = WorldServerT Identity

deriving instance Show WorldServer

type WorldServerPK = PrimaryKey WorldServerT Identity

deriving instance Show WorldServerPK
