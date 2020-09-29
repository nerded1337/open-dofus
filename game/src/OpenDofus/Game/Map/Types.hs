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

{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module OpenDofus.Game.Map.Types
  ( module X
  , MapInstance
  , MapInstanceT(..)
  , HasMapInstanceT(..)
  , MapController(..)
  , HasMapController(..)
  )
where

import           Control.Concurrent.Chan.Unagi.NoBlocking
                                               as U
import           Data.HashMap.Strict           as HM
import           OpenDofus.Database
import           OpenDofus.Game.Map.Actor      as X
import           OpenDofus.Game.Map.Cell       as X
import           OpenDofus.Game.Map.Event      as X
import           OpenDofus.Game.Map.Interactive
                                               as X
import           OpenDofus.Prelude

type MapInstance a
  = MapInstanceT
      (Maybe InteractiveObjectInstance)
      (IORef (HM.HashMap ActorId (GameActor a)))

data MapInstanceT a b = MapInstance
    { _mapInstanceTemplate :: {-# UNPACK #-} !Map
    , _mapInstanceCells    :: !(HM.HashMap CellId (CellT a))
    , _mapInstanceActors   :: !b
    }
    deriving stock (Show, Functor, Foldable, Traversable)

makeClassy ''MapInstanceT

instance Bifunctor MapInstanceT where
  {-# INLINE bimap #-}
  bimap f g m = m { _mapInstanceCells  = fmap f <$> _mapInstanceCells m
                  , _mapInstanceActors = g $ _mapInstanceActors m
                  }

instance Bifoldable MapInstanceT where
  {-# INLINE bifoldMap #-}
  bifoldMap f g m =
    foldMap (f . view cellInteractiveObjects) (m ^. mapInstanceCells)
      <> g (m ^. mapInstanceActors)

instance Bitraversable MapInstanceT where
  {-# INLINE bitraverse #-}
  bitraverse f g (MapInstance x y z) =
    MapInstance x <$> traverse (traverse f) y <*> g z

data MapController a b = MapController
    { _mapControllerMap      :: !(MapInstance a)
    , _mapControllerEventIn  :: {-# UNPACK #-} !(InChan b)
    , _mapControllerEventOut :: {-# UNPACK #-} !(OutChan b)
    }

makeClassy ''MapController

