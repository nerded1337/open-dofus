{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- Prim.hs ---

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

module OpenDofus.Core.Data.Prim
  ( internal_,
    nextPowerOfTwo,
  )
where

import Control.Monad.Primitive
import GHC.Exts

internal_ :: PrimBase m => m () -> State# (PrimState m) -> State# (PrimState m)
internal_ m s = case internal m s of
  (# s', _ #) -> s'
{-# INLINE internal_ #-}

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo (I# 0#) = I# 0#
nextPowerOfTwo (I# x#) =
  I# (1# `uncheckedIShiftL#` (32# -# word2Int# (clz32# (int2Word# (x# -# 1#)))))
{-# INLINE nextPowerOfTwo #-}
