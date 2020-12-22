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

module OpenDofus.Core.Network.Client.Types
  ( FoldNetwork (..),
    ToNetwork (..),
  )
where

import qualified Data.ByteString.Lazy.Builder as LBS
import OpenDofus.Prelude

class ToNetwork a where
  toNetwork :: a -> LBS.Builder

newtype FoldNetwork f a = FoldNetwork {unFoldNetwork :: f a}

instance (Foldable f, ToNetwork a) => ToNetwork (FoldNetwork f a) where
  toNetwork = foldMap toNetwork . unFoldNetwork
  {-# INLINE toNetwork #-}
