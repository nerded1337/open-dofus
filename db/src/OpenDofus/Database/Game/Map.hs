{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Map.hs ---

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

module OpenDofus.Database.Game.Map where

import Data.Vector.Primitive (Prim)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import OpenDofus.Database.Types
import OpenDofus.Prelude

newtype CellId = CellId
  { unCellId :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype MapId = MapId
  { unMapId :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype MapWidth = MapWidth
  { unMapWidth :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype MapHeight = MapHeight
  { unMapHeight :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype MapCompressedData = MapCompressedData
  { unMapCompressedData :: Text
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      FromBackendRow Postgres
    )

newtype MapCreationDate = MapCreationDate
  { unMapCreationDate :: Text
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      FromBackendRow Postgres
    )

newtype MapDataKey = MapDataKey
  { unMapDataKey :: Text
  }
  deriving newtype
    ( Show,
      Ord,
      Eq,
      HasDefaultSqlDataType Postgres,
      HasSqlValueSyntax PgValueSyntax,
      FromBackendRow Postgres
    )

newtype MapSuperAreaId = MapSuperAreaId
  { unMapSuperAreaId :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype MapAreaId = MapAreaId
  { unMapAreaId :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

newtype MapSubAreaId = MapSubAreaId
  { unMapSubAreaId :: Word32
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
      HasSqlValueSyntax PgValueSyntax,
      HasSqlEqualityCheck Postgres,
      FromBackendRow Postgres
    )

data MapSuperAreaT f = MapSuperArea
  { _mapSuperAreaId :: !(C f MapSuperAreaId),
    _mapSuperAreaName :: !(C f Text)
  }
  deriving (Generic, Beamable)

instance Table MapSuperAreaT where
  data PrimaryKey MapSuperAreaT f = MapSuperAreaPK !(C f MapSuperAreaId)
    deriving (Generic, Beamable)
  primaryKey = MapSuperAreaPK . _mapSuperAreaId
  {-# INLINE primaryKey #-}

type MapSuperArea = MapSuperAreaT Identity

deriving instance Eq MapSuperArea

deriving instance Show MapSuperArea

type MapSuperAreaPK = PrimaryKey MapSuperAreaT Identity

deriving instance Eq MapSuperAreaPK

deriving instance Show MapSuperAreaPK

MapSuperArea
  (LensFor superAreaId)
  (LensFor superAreaName) = tableLenses

data MapAreaT f = MapArea
  { _mapAreaId :: !(C f MapAreaId),
    _mapAreaName :: !(C f Text),
    _mapAreaSuperArea :: !(PrimaryKey MapSuperAreaT f)
  }
  deriving (Generic, Beamable)

instance Table MapAreaT where
  data PrimaryKey MapAreaT f = MapAreaPK !(C f MapAreaId)
    deriving (Generic, Beamable)
  primaryKey = MapAreaPK . _mapAreaId
  {-# INLINE primaryKey #-}

type MapArea = MapAreaT Identity

deriving instance Eq MapArea

deriving instance Show MapArea

type MapAreaPK = PrimaryKey MapAreaT Identity

deriving instance Eq MapAreaPK

deriving instance Show MapAreaPK

MapArea (LensFor areaId) (LensFor areaName) (MapSuperAreaPK (LensFor areaSuperArea)) =
  tableLenses

data MapSubAreaT f = MapSubArea
  { _mapSubAreaId :: !(C f MapSubAreaId),
    _mapSubAreaName :: !(C f Text),
    _mapSubAreaArea :: !(PrimaryKey MapAreaT f),
    _mapSubAreaMusics :: !(C f (PgArray Int32))
  }
  deriving (Generic, Beamable)

instance Table MapSubAreaT where
  data PrimaryKey MapSubAreaT f = MapSubAreaPK !(C f MapSubAreaId)
    deriving (Generic, Beamable)
  primaryKey = MapSubAreaPK . _mapSubAreaId
  {-# INLINE primaryKey #-}

type MapSubArea = MapSubAreaT Identity

deriving instance Eq MapSubArea

deriving instance Show MapSubArea

type MapSubAreaPK = PrimaryKey MapSubAreaT Identity

deriving instance Eq MapSubAreaPK

deriving instance Show MapSubAreaPK

MapSubArea
  (LensFor subAreaId)
  (LensFor subAreaName)
  (MapAreaPK (LensFor subAreaArea))
  (LensFor subAreaMusics) =
    tableLenses

data MapSubAreaNeighbourT f = MapSubAreaNeighbour
  { _mapSubAreaNeighbourOrigin :: !(PrimaryKey MapSubAreaT f),
    _mapSubAreaNeighbourDestination :: !(PrimaryKey MapSubAreaT f)
  }
  deriving (Generic, Beamable)

instance Table MapSubAreaNeighbourT where
  data
    PrimaryKey
      MapSubAreaNeighbourT
      f
    = MapSubAreaNeighbourPK
        !(PrimaryKey MapSubAreaT f)
        !(PrimaryKey MapSubAreaT f)
    deriving (Generic, Beamable)
  primaryKey =
    MapSubAreaNeighbourPK
      <$> _mapSubAreaNeighbourOrigin
      <*> _mapSubAreaNeighbourDestination
  {-# INLINE primaryKey #-}

type MapSubAreaNeighbour = MapSubAreaNeighbourT Identity

deriving instance Eq MapSubAreaNeighbour

deriving instance Show MapSubAreaNeighbour

type MapSubAreaNeighbourPK = PrimaryKey MapSubAreaNeighbourT Identity

deriving instance Eq MapSubAreaNeighbourPK

deriving instance Show MapSubAreaNeighbourPK

MapSubAreaNeighbour
  (MapSubAreaPK (LensFor subAreaNeighbourOriginSubAreaId))
  (MapSubAreaPK (LensFor subAreaNeighbourDestinationSubAreaId)) =
    tableLenses

data MapT f = Map
  { _mapId :: !(C f MapId),
    _mapCreationDate :: !(C f MapCreationDate),
    _mapSubArea :: !(PrimaryKey MapSubAreaT f),
    _mapX :: !(C f Int32),
    _mapY :: !(C f Int32),
    _mapWidth :: !(C f MapWidth),
    _mapHeight :: !(C f MapHeight),
    _mapBackgroundNum :: !(C f Int32),
    _mapAmbianceId :: !(C f Int32),
    _mapIsOutdoor :: !(C f Bool),
    _mapCapabilities :: !(C f Word32),
    _mapCompressedData :: !(C f MapCompressedData),
    _mapDataKey :: !(C f (Maybe MapDataKey))
  }
  deriving (Generic, Beamable)

instance Table MapT where
  data PrimaryKey MapT f = MapPK !(C f MapId)
    deriving (Generic, Beamable)
  primaryKey = MapPK . _mapId
  {-# INLINE primaryKey #-}

type Map = MapT Identity

deriving instance Eq Map

deriving instance Show Map

type MapPK = PrimaryKey MapT Identity

deriving instance Eq MapPK

deriving instance Show MapPK

Map
  (LensFor mapId)
  (LensFor mapCreationDate)
  (MapSubAreaPK (LensFor mapSubAreaId))
  (LensFor mapX)
  (LensFor mapY)
  (LensFor mapWidth)
  (LensFor mapHeight)
  (LensFor mapBackgroundNum)
  (LensFor mapAmbianceId)
  (LensFor mapIsOutdoor)
  (LensFor mapCapabilities)
  (LensFor mapCompressedData)
  (LensFor mapDataKey) =
    tableLenses
