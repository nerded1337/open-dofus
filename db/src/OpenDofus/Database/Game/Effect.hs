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
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

data EffectOperator
  = Plus
  | Minus
  | Divide
  deriving (Show, Eq, Ord, Read, Enum, Bounded)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField EffectOperator)

data EffectType
  = Damage
  | Heal
  | Special
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField EffectType)

data EffectShape
  = Circle
  | Line
  | TLine
  | Cross
  | Point
  | Ring
  | Rectangle
  | UnknownD
  deriving stock (Show, Eq, Ord, Read, Enum, Bounded, Generic)
  deriving anyclass (Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (EnumField EffectShape)

data EffectSize
  = Finite Int
  | Infinite
  deriving stock (Show, Eq, Ord, Read, Generic)
  deriving anyclass (Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax
    )
    via (BinaryField EffectSize)

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
    via (BinaryField EffectZone)

zoneFromPattern :: Text -> (EffectZone, Text)
zoneFromPattern x =
  let arr = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['-', '_']
      idx i =
        fromMaybe (error $ "Invalid zone size: " <> show i) $ elemIndex i arr
      getShape 'C' = Circle
      getShape 'X' = Cross
      getShape 'L' = Line
      getShape 'T' = TLine
      getShape 'P' = Point
      getShape 'O' = Ring
      getShape 'R' = Rectangle
      getShape 'D' = UnknownD
      getShape s = error $ "Invalid zone shape: " <> show s
   in case x of
        (shape :- ('_' :- xs)) ->
          (EffectZone (getShape shape) Infinite, xs)
        (shape :- (s :- xs)) ->
          (EffectZone (getShape shape) (Finite $ idx s), xs)
        _ ->
          error $ "Invalid Effect Zone: " <> show x

data EffectT f = Effect
  { _effectId :: !(C f EffectId),
    _effectType :: !(C f EffectType),
    _effectDescription :: !(C f Text),
    _effectHasJet :: !(C f Bool),
    _effectShowInTooltip :: !(C f Bool),
    _effectOperator :: !(C f (Maybe EffectOperator)),
    _effectCaracteristic :: !(C f Word32)
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
  (LensFor effectCaracteristic) =
    tableLenses
