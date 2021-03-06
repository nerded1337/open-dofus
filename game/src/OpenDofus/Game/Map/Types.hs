{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
    MapHandlerInput (..),
    HasMapHandlerInput (..),
    MapHandler,
    MapController,
    MapControllerT (..),
    HasMapControllerT (..),
    ToActor (..),
  )
where

import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as HM
import OpenDofus.Core.Data.Record
import OpenDofus.Database
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Controller
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Instance
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Time
import OpenDofus.Prelude

data MapHandlerInput = MapHandlerInput
  { _mapHandlerInputCtl :: {-# UNPACK #-} !MapController,
    _mapHandlerInputElapsed :: {-# UNPACK #-} !(GameTime Millisecond),
    _mapHandlerInputEvent :: !MapEvent
  }

makeClassy ''MapHandlerInput

instance HasConnectPool MapHandlerInput AuthDbConn where
  getConnectionPool = getConnectionPool . view mapHandlerInputCtl
  {-# INLINE getConnectionPool #-}

instance HasConnectPool MapHandlerInput GameDbConn where
  getConnectionPool = getConnectionPool . view mapHandlerInputCtl
  {-# INLINE getConnectionPool #-}

newtype ToActor a
  = ToActor {unToActor :: HM.HashMap ActorId a}
  deriving (Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (ToActor a) where
  ToActor x <> ToActor y =
    ToActor $ HM.unionWith (<>) x y
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (ToActor a) where
  mempty = ToActor mempty
  {-# INLINE mempty #-}

type MapHandler a =
  ReaderT
    MapHandlerInput
    ( WriterT
        ( GameMessage
            :<*>: ToActor GameMessage
        )
        IO
    )
    a
