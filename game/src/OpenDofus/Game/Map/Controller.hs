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

-- Controller.hs ---

-- Copyright (C) 2021 Nerd Ed

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

module OpenDofus.Game.Map.Controller
  ( MapController,
    MapControllerT (..),
    HasMapControllerT (..),
  )
where

import qualified Data.HashTable.IO as H
import OpenDofus.Database
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Instance
import OpenDofus.Game.Map.Interactive
import OpenDofus.Game.Network.Message
import OpenDofus.Prelude

type HashTable k v = H.BasicHashTable k v

type MapController = MapControllerT IO (Compose CellT Maybe InteractiveObject)

data MapControllerT m a = MapController
  { _mapControllerInstance :: {-# UNPACK #-} !(MapInstanceT a),
    _mapControllerActors :: {-# UNPACK #-} !(HashTable ActorId Actor),
    _mapControllerInteractiveObjectInstances :: {-# UNPACK #-} !(HashTable CellId InteractiveObjectInstance),
    _mapControllerAuthDbPool :: {-# UNPACK #-} !(Pool AuthDbConn),
    _mapControllerGameDbPool :: {-# UNPACK #-} !(Pool GameDbConn),
    _mapControllerDispatch :: !(ActorId -> GameMessage -> m ())
  }

makeClassy ''MapControllerT

instance HasConnectPool MapController AuthDbConn where
  getConnectionPool = view mapControllerAuthDbPool
  {-# INLINE getConnectionPool #-}

instance HasConnectPool MapController GameDbConn where
  getConnectionPool = view mapControllerGameDbPool
  {-# INLINE getConnectionPool #-}
