-- Types.hs ---

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

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module OpenDofus.Core.Network.Types
  ( NetworkId(..)
  , HasNetworkId(..)
  , MaxClient(..)
  , ReceiveBufferSize(..)
  ) where

import           OpenDofus.Prelude

newtype MaxClient =
  MaxClient
    { unMaxClient :: Int
    }
  deriving newtype (Show, Eq, Ord, Num, Enum)

newtype ReceiveBufferSize =
  ReceiveBufferSize
    { unReceiveBufferSize :: Int
    }
  deriving newtype (Show, Eq, Ord, Num, Enum)

newtype NetworkId =
  NetworkId
    { unNetworkId :: UUID
    }
  deriving newtype (Show, Eq, Ord)

makeClassy ''NetworkId
