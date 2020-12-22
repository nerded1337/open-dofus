-- Item.hs ---
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OpenDofus.Database.Game.Item where

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

newtype ItemSuperTypeId = ItemSuperTypeId
  { unItemSuperTypeId :: Word32
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

newtype ItemTypeId = ItemTypeId
  { unItemTypeId :: Word32
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

newtype ItemSlotId = ItemSlotId
  { unItemSlotId :: Word32
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

newtype ItemId = ItemId
  { unItemId :: Word32
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

data ItemSuperTypeT f = ItemSuperType
  { _itemSuperTypeId :: !(C f ItemSuperTypeId)
  }
  deriving (Generic, Beamable)

instance Table ItemSuperTypeT where
  data PrimaryKey ItemSuperTypeT f = ItemSuperTypePK !(C f ItemSuperTypeId)
    deriving (Generic, Beamable)
  primaryKey = ItemSuperTypePK . _itemSuperTypeId

type ItemSuperType = ItemSuperTypeT Identity

deriving instance Eq ItemSuperType

deriving instance Show ItemSuperType

type ItemSuperTypePK = PrimaryKey ItemSuperTypeT Identity

deriving instance Eq ItemSuperTypePK

deriving instance Show ItemSuperTypePK

ItemSuperType (LensFor superTypeId) = tableLenses

data ItemTypeT f = ItemType
  { _itemTypeId :: !(C f ItemTypeId),
    _itemSuperType :: !(PrimaryKey ItemSuperTypeT f), -- t
    _itemTypeName :: !(C f Text), -- n
    _itemTypeEffectZone :: !(C f (Maybe EffectZone)) -- z
  }
  deriving (Generic, Beamable)

instance Table ItemTypeT where
  data PrimaryKey ItemTypeT f = ItemTypePK !(C f ItemTypeId)
    deriving (Generic, Beamable)
  primaryKey = ItemTypePK . _itemTypeId

type ItemType = ItemTypeT Identity

deriving instance Eq ItemType

deriving instance Show ItemType

type ItemTypePK = PrimaryKey ItemTypeT Identity

deriving instance Eq ItemTypePK

deriving instance Show ItemTypePK

ItemType
  (LensFor itemTypeId)
  (ItemSuperTypePK (LensFor itemTypeItemSuperTypeId))
  (LensFor itemTypeName)
  (LensFor itemTypeEffectZone) =
    tableLenses

data ItemSlotT f = ItemSlot
  { _itemSlotItemType :: !(PrimaryKey ItemSuperTypeT f),
    _itemSlotId :: !(C f ItemSlotId)
  }
  deriving (Generic, Beamable)

instance Table ItemSlotT where
  data PrimaryKey ItemSlotT f
    = ItemSlotPK
        !( PrimaryKey
             ItemSuperTypeT
             f
         )
        !(C f ItemSlotId)
    deriving (Generic, Beamable)
  primaryKey = ItemSlotPK <$> _itemSlotItemType <*> _itemSlotId

type ItemSlot = ItemSlotT Identity

deriving instance Eq ItemSlot

deriving instance Show ItemSlot

type ItemSlotPK = PrimaryKey ItemSlotT Identity

deriving instance Eq ItemSlotPK

deriving instance Show ItemSlotPK

ItemSlot
  (ItemSuperTypePK (LensFor itemSlotItemTypeId))
  (LensFor itemSlotId) =
    tableLenses

data WeaponInfos = WeaponInfos
  { _weaponInfosCriticalBonus :: {-# UNPACK #-} !Word32, -- 0
    _weaponInfosApCost :: {-# UNPACK #-} !Word32, -- 1
    _weaponInfosRangeMin :: {-# UNPACK #-} !Word32, -- 2
    _weaponInfosRangeMax :: {-# UNPACK #-} !Word32, -- 3
    _weaponInfosCriticalHit :: {-# UNPACK #-} !Word32, -- 4
    _weaponInfosCriticalFailure :: {-# UNPACK #-} !Word32, -- 5
    _weaponInfosLineOnly :: !Bool, -- 6
    _weaponInfosLineOfSight :: !Bool -- 7
  }
  deriving (Show, Eq, Ord, Read)
  deriving (Generic, Binary)
  deriving
    ( ToField,
      FromField,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      FromBackendRow Postgres
    )
    via (BinaryField WeaponInfos)

makeClassy ''WeaponInfos

data ItemT f = Item
  { _itemId :: !(C f ItemId),
    _itemType :: !(PrimaryKey ItemTypeT f), -- t
    _itemName :: !(C f Text), -- n
    _itemDescription :: !(C f Text), -- d
    _itemGfx :: !(C f Word32), -- g
    _itemLevel :: !(C f Word32), -- l
    _itemWeight :: !(C f Word32), -- w
    _itemPrice :: !(C f Word32), -- p
    _itemConditions :: !(C f (Maybe Text)), -- c
    _itemIsCursed :: !(C f Bool), -- m
    _itemIsEnhanceable :: !(C f Bool), -- fm
    _itemNeedsTwoHands :: !(C f Bool), -- tw
    _itemIsEthereal :: !(C f Bool), -- et
    _itemIsHidden :: !(C f Bool), -- h
    _itemIsUsable :: !(C f Bool), -- u
    _itemIsTargetable :: !(C f Bool), -- ut
    _itemAnimation :: !(C f (Maybe Word32)), -- an
    _itemWeaponInfos :: !(C f (Maybe WeaponInfos)), -- e
    _itemStatistics :: !(C f (Maybe Text))
  }
  deriving (Generic, Beamable)

instance Table ItemT where
  data PrimaryKey ItemT f = ItemPK !(C f ItemId)
    deriving (Generic, Beamable)
  primaryKey = ItemPK . _itemId

type Item = ItemT Identity

deriving instance Eq Item

deriving instance Show Item

type ItemPK = PrimaryKey ItemT Identity

deriving instance Eq ItemPK

deriving instance Show ItemPK

Item
  (LensFor itemId)
  (ItemTypePK (LensFor itemItemTypeId))
  (LensFor itemName)
  (LensFor itemDescription)
  (LensFor itemGfx)
  (LensFor itemLevel)
  (LensFor itemWeight)
  (LensFor itemPrice)
  (LensFor itemConditions)
  (LensFor itemIsCursed)
  (LensFor itemIsEnhanceable)
  (LensFor itemNeedsTwoHands)
  (LensFor itemIsEthereal)
  (LensFor itemIsHiddden)
  (LensFor itemIsUsable)
  (LensFor itemIsTargetable)
  (LensFor itemAnimation)
  (LensFor itemWeraponInfos)
  (LensFor itemStatistics) =
    tableLenses
