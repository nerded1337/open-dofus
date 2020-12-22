{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- Types.hs ---

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

module OpenDofus.Game.Map.Types
  ( MapInstance,
    MapInstanceT (..),
    HasMapInstanceT (..),
    HashTable,
    MapEventArgs (..),
    HasMapEventArgs (..),
    MapEventReader,
    MapEventDispatch (..),
    HasMapEventDispatch (..),
    MapController,
    MapControllerT (..),
    HasMapControllerT (..),
  )
where

import Data.HashMap.Strict as HM
import qualified Data.HashTable.IO as H
import OpenDofus.Database
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Interactive
import OpenDofus.Game.Network.Message
import OpenDofus.Prelude

type HashTable k v = H.CuckooHashTable k v

type MapInstance = MapInstanceT (Maybe InteractiveObjectInstance)

data MapInstanceT a = MapInstance
  { _mapInstanceTemplate :: {-# UNPACK #-} !Map,
    _mapInstanceCells :: !(HM.HashMap CellId (CellT a))
  }
  deriving stock (Functor, Foldable, Traversable)

makeClassy ''MapInstanceT

type MapController = MapControllerT IO (Maybe InteractiveObjectInstance)

data MapControllerT m a = MapController
  { _mapControllerInstance :: {-# UNPACK #-} !(MapInstanceT a),
    _mapControllerActors :: {-# UNPACK #-} !(HashTable ActorId GameActor),
    _mapControllerDispatch :: !(ActorId -> [GameMessage] -> m ())
  }
  deriving stock (Functor, Foldable, Traversable)

makeClassy ''MapControllerT

type MapEventReader m = MonadReader MapEventArgs m

data MapEventArgs = MapEventArgs
  { _mapEventArgsEvent :: !MapEvent,
    _mapEventArgsCtl :: !MapController
  }

makeClassy ''MapEventArgs

instance Show MapEventArgs where
  show = show . view mapEventArgsEvent

data MapEventDispatch = MapEventDispatch
  { _mapEventDispatchEvent :: !MapEvent,
    _mapEventDispatchMapId :: {-# UNPACK #-} !MapId
  }

makeClassy ''MapEventDispatch
