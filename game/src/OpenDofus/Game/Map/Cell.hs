-- Cell.hs ---

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

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module OpenDofus.Game.Map.Cell
  ( Cell(..)
  , CellId(..)
  , HasCell(..)
  , parseCell
  )
where

import           Data.Bits
import           OpenDofus.Database
import           OpenDofus.Prelude       hiding ( Map )

data Cell a = Cell
    { _mapCellId                      :: CellId
    , _mapCellActive                  :: Bool
    , _mapCellLineOfSight             :: Bool
    , _mapCellLayerGroundRot          :: Word16
    , _mapCellGroundLevel             :: Word16
    , _mapCellMovement                :: Word16
    , _mapCellLayerGroundNum          :: Word16
    , _mapCellGroundSlope             :: Word16
    , _mapCellLayerGroundFlip         :: Bool
    , _mapCellLayerObject1Num         :: Word16
    , _mapCellLayerObject1Rot         :: Word16
    , _mapCellLayerObject1Flip        :: Bool
    , _mapCellLayerObject2Flip        :: Bool
    , _mapCellLayerObject2Interactive :: Bool
    , _mapCellLayerObject2Num         :: Word16
    , _mapCellInteractiveObject       :: Maybe a
    }

makeClassy ''Cell

parseCell :: CellId -> [Word8] -> Maybe (Cell InteractiveObjectId)
      --       0  1  2  3  4  5  6  7  8  9
parseCell cid [a, b, c, d, e, f, g, h, i, j] =
  let conv   = fromIntegral @Word8 @Word16
      active = conv (a .&. 32) `shiftR` 5 == 1
      los    = conv (a .&. 1) == 1
      lgr    = conv (b .&. 48) `shiftR` 4
      gl     = conv (b .&. 15)
      m      = conv (c .&. 56) `shiftR` 3
      lgn = (conv (a .&. 24) `shiftL` 6) + (conv (c .&. 7) `shiftL` 6) + conv d
      gs     = conv (e .&. 60) `shiftR` 2
      lgf    = conv (e .&. 2) `shiftR` 1 == 1
      lo1n =
          (conv (a .&. 4) `shiftL` 11)
            + (conv (e .&. 1) `shiftL` 12)
            + (conv f `shiftL` 6)
            + conv g
      lo1r = conv (h .&. 48) `shiftR` 5
      lo1f = conv (h .&. 8) `shiftR` 3 == 1
      lo2f = conv (h .&. 4) `shiftR` 2 == 1
      lo2i = conv (h .&. 2) `shiftR` 1 == 1
      lo2n =
          (conv (a .&. 2) `shiftL` 12)
            + (conv (h .&. 1) `shiftL` 12)
            + (conv i `shiftL` 6)
            + conv j
      ioId =
          (conv (a .&. 2) `shiftL` 12)
            + (conv (b .&. 1) `shiftL` 12)
            + conv (i `shiftL` 6)
            + conv j
  in  if not active
        then Nothing
        else Just $ Cell
          cid
          active
          los
          lgr
          m
          lgn
          gs
          gl
          lgf
          lo1n
          lo1r
          lo1f
          lo2f
          lo2i
          lo2n
          (if lo2i
            then Just (InteractiveObjectId $ fromIntegral ioId)
            else Nothing
          )
parseCell _ _ = Nothing
