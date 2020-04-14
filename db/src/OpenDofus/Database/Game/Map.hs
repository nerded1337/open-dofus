-- Map.hs ---

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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OpenDofus.Database.Game.Map where

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           OpenDofus.Prelude       hiding ( Map )

newtype CellId =
  CellId
    { unCellId :: Word32
    }
  deriving newtype (Show, Ord, Eq, Num, Real, Enum, Integral)

newtype MapId =
  MapId
    { unMapId :: Word32
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

newtype MapSuperAreaId =
  MapSuperAreaId
    { unMapSuperAreaId :: Word32
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

newtype MapAreaId =
  MapAreaId
    { unMapAreaId :: Word32
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

newtype MapSubAreaId =
  MapSubAreaId
    { unMapSubAreaId :: Word32
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

data MapSuperAreaT f =
  MapSuperArea
    { _mapSuperAreaId   :: !(C f MapSuperAreaId)
    , _mapSuperAreaName :: !(C f Text)
    } deriving (Generic, Beamable)

instance Table MapSuperAreaT where
  data PrimaryKey MapSuperAreaT f = MapSuperAreaPK !(C f MapSuperAreaId)
                                    deriving (Generic, Beamable)
  primaryKey = MapSuperAreaPK . _mapSuperAreaId

type MapSuperArea = MapSuperAreaT Identity
deriving instance Show MapSuperArea

type MapSuperAreaPK = PrimaryKey MapSuperAreaT Identity
deriving instance Show MapSuperAreaPK

MapSuperArea (LensFor superAreaId) (LensFor superAreaName) = tableLenses

data MapAreaT f =
  MapArea
    { _mapAreaId        :: !(C f MapAreaId)
    , _mapAreaName      :: !(C f Text)
    , _mapAreaSuperArea :: !(PrimaryKey MapSuperAreaT f)
    }
  deriving (Generic, Beamable)

instance Table MapAreaT where
  data PrimaryKey MapAreaT f = MapAreaPK !(C f MapAreaId)
                                    deriving (Generic, Beamable)
  primaryKey = MapAreaPK . _mapAreaId

type MapArea = MapAreaT Identity
deriving instance Show MapAreaPK

type MapAreaPK = PrimaryKey MapAreaT Identity
deriving instance Show MapArea

MapArea (LensFor areaId) (LensFor areaName) (MapSuperAreaPK (LensFor areaSuperArea))
  = tableLenses

data MapSubAreaT f =
  MapSubArea
    { _mapSubAreaId     :: !(C f MapSubAreaId)
    , _mapSubAreaName   :: !(C f Text)
    , _mapSubAreaArea   :: !(PrimaryKey MapAreaT f)
    , _mapSubAreaMusics :: !(C f (Vector Int))
    } deriving (Generic, Beamable)

instance Table MapSubAreaT where
  data PrimaryKey MapSubAreaT f = MapSubAreaPK !(C f MapSubAreaId)
                                    deriving (Generic, Beamable)
  primaryKey = MapSubAreaPK . _mapSubAreaId

type MapSubArea = MapSubAreaT Identity
deriving instance Show MapSubArea

type MapSubAreaPK = PrimaryKey MapSubAreaT Identity
deriving instance Show MapSubAreaPK

MapSubArea (LensFor subAreaId) (LensFor subAreaName) (MapAreaPK (LensFor subAreaArea)) (LensFor subAreaMusics)
  = tableLenses

data MapSubAreaNeighbourT f =
  MapSubAreaNeighbour
    { _mapSubAreaNeighbourOrigin      :: !(PrimaryKey MapSubAreaT f)
    , _mapSubAreaNeighbourDestination :: !(PrimaryKey MapSubAreaT f)
    }
  deriving (Generic, Beamable)

instance Table MapSubAreaNeighbourT where
  data PrimaryKey MapSubAreaNeighbourT
       f = MapSubAreaNeighbourId !(PrimaryKey MapSubAreaT f)
                                 !(PrimaryKey MapSubAreaT f)
             deriving (Generic, Beamable)
  primaryKey =
    MapSubAreaNeighbourId
      <$> _mapSubAreaNeighbourOrigin
      <*> _mapSubAreaNeighbourDestination

type MapSubAreaNeighbour = MapSubAreaNeighbourT Identity
deriving instance Show MapSubAreaNeighbour

type MapSubAreaNeighbourId = PrimaryKey MapSubAreaNeighbourT Identity
deriving instance Show MapSubAreaNeighbourId

MapSubAreaNeighbour (MapSubAreaPK (LensFor subAreaNeighbourOriginSubAreaId)) (MapSubAreaPK (LensFor subAreaNeighbourDestinationSubAreaId))
  = tableLenses

data MapT f =
  Map
    { _mapId            :: !(C f MapId)
    , _mapDate          :: !(C f Text)
    , _mapSubArea       :: !(PrimaryKey MapSubAreaT f)
    , _mapX             :: !(C f Int)
    , _mapY             :: !(C f Int)
    , _mapWidth         :: !(C f Int)
    , _mapHeight        :: !(C f Int)
    , _mapBackgroundNum :: !(C f Int)
    , _mapAmbianceId    :: !(C f Int)
    , _mapIsOutdoor     :: !(C f Bool)
    , _mapCapabilities  :: !(C f Int)
    , _mapData          :: !(C f Text)
    , _mapDataKey       :: !(C f (Maybe Text))
    }
  deriving (Generic, Beamable)

instance Table MapT where
  data PrimaryKey MapT f = MapPK !(C f MapId)
                           deriving (Generic, Beamable)
  primaryKey = MapPK . _mapId

type Map = MapT Identity
deriving instance Show Map

type MapPK = PrimaryKey MapT Identity
deriving instance Show MapPK

Map (LensFor mapId) (LensFor mapDate) (MapSubAreaPK (LensFor mapSubAreaId)) (LensFor mapX) (LensFor mapY) (LensFor mapWidth) (LensFor mapHeight) (LensFor mapBackgroundNum) (LensFor mapAmbianceId) (LensFor mapIsOutdoor) (LensFor mapCapabilities) (LensFor mapData) (LensFor mapDataKey)
  = tableLenses
