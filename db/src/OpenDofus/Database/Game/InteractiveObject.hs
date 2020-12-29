{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- InteractiveObject.hs ---

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

module OpenDofus.Database.Game.InteractiveObject where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import OpenDofus.Database.Types
import OpenDofus.Prelude

newtype InteractiveObjectId = InteractiveObjectId
  { unInteractiveObjectId :: Word32
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data InteractiveObjectType
  = InteractiveObjectTypeResource
  | InteractiveObjectTypeCraftPlan
  | InteractiveObjectTypeZaap
  | InteractiveObjectTypeFountainOfYouth
  | InteractiveObjectTypeDoor
  | InteractiveObjectTypeStorage
  | InteractiveObjectTypeCookingPot
  | InteractiveObjectTypeZaapi
  | InteractiveObjectTypeCraftmenList
  | InteractiveObjectTypePaddock
  | InteractiveObjectTypeLever
  | InteractiveObjectTypeClassStatue
  deriving stock (Show, Eq, Ord, Enum, Bounded)
  deriving
    ( ToField,
      FromField,
      FromBackendRow Postgres,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField InteractiveObjectType)

data InteractiveObjectT f = InteractiveObject
  { _interactiveObjectId :: !(C f InteractiveObjectId),
    _interactiveObjectType :: !(C f InteractiveObjectType),
    _interactiveObjectName :: !(C f Text)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table InteractiveObjectT where
  data PrimaryKey InteractiveObjectT f
    = InteractiveObjectPK (C f InteractiveObjectId)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = InteractiveObjectPK . _interactiveObjectId
  {-# INLINE primaryKey #-}

type InteractiveObject = InteractiveObjectT Identity

deriving instance Eq InteractiveObject

deriving instance Show InteractiveObject

type InteractiveObjectPK = PrimaryKey InteractiveObjectT Identity

deriving instance Eq InteractiveObjectPK

deriving instance Show InteractiveObjectPK

InteractiveObject
  (LensFor interactiveObjectId)
  (LensFor interactiveObjectType)
  (LensFor interactiveObjectName) =
    tableLenses

newtype InteractiveObjectGfxId = InteractiveObjectGfxId
  { unInteractiveObjectGfxId :: Word32
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      Num,
      Real,
      Enum,
      Integral,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data InteractiveObjectGfxT f = InteractiveObjectGfx
  { _interactiveObjectGfxId :: !(C f InteractiveObjectGfxId),
    _interactiveObjectGfxInteractiveObjectId :: !(PrimaryKey InteractiveObjectT f)
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

instance Table InteractiveObjectGfxT where
  data
    PrimaryKey
      InteractiveObjectGfxT
      f
    = InteractiveObjectGfxPK
        !(C f InteractiveObjectGfxId)
        !(PrimaryKey InteractiveObjectT f)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey =
    InteractiveObjectGfxPK
      <$> _interactiveObjectGfxId
      <*> _interactiveObjectGfxInteractiveObjectId
  {-# INLINE primaryKey #-}

type InteractiveObjectGfx = InteractiveObjectGfxT Identity

deriving instance Eq InteractiveObjectGfx

deriving instance Show InteractiveObjectGfx

type InteractiveObjectGfxPK = PrimaryKey InteractiveObjectGfxT Identity

deriving instance Eq InteractiveObjectGfxPK

deriving instance Show InteractiveObjectGfxPK

InteractiveObjectGfx
  (LensFor interactiveObjectGfxId)
  (InteractiveObjectPK (LensFor interactiveObjectGfxInteractiveObjectId)) =
    tableLenses
