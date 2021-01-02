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

-- Instance.hs ---

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

module OpenDofus.Game.Map.Instance
  ( MapInstance,
    MapInstanceT (..),
    HasMapInstanceT (..),
  )
where

import Control.Monad.Writer
import Data.HashMap.Strict as HM
import OpenDofus.Database
import OpenDofus.Game.Map.Cell
import OpenDofus.Prelude

type MapInstance = MapInstanceT (Compose CellT Maybe InteractiveObject)

data MapInstanceT a = MapInstance
  { _mapInstanceTemplate :: {-# UNPACK #-} !Map,
    _mapInstanceCells :: !(HM.HashMap CellId a)
  }
  deriving stock (Functor, Foldable, Traversable)

makeClassy ''MapInstanceT
