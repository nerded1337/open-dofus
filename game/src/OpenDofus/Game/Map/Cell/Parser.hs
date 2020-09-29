-- Parser.hs ---

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

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module OpenDofus.Game.Map.Cell.Parser
  ( module X
  , parseCell
  )
where

import           Data.Bits
import           Data.ByteString               as BS
import           Data.Vector                   as V
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Game.Map.Cell.Types as X
import           OpenDofus.Prelude

{-# INLINE hashCell #-}
hashCell :: Vector Word8
hashCell = V.fromList $ BS.unpack
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

{-# INLINE prepareCellData #-}
prepareCellData :: [Word8] -> [Word8]
prepareCellData !x = go <$> x
 where
  go !e =
    toEnum . fromEnum $ fromMaybe (error "Data not indexed") $ V.elemIndex
      e
      hashCell

{-# INLINE parseCell #-}
parseCell
  :: CellId -> [Word8] -> (CellId, Maybe (CellT (Maybe InteractiveObjectGfxId)))
      --       0  1  2  3  4  5  6  7  8  9
parseCell !cid !x = go (prepareCellData x)
 where
  go (a :- (b :- (c :- (d :- (e :- (f :- (g :- (h :- (i :- (j :- _)))))))))) =
    let
      conv   = fromIntegral @Word8 @Word16
      active = ((a .&. 32) `shiftR` 5) > 0
      los    = (a .&. 1) == 1
      lgr    = (b .&. 48) `shiftR` 4
      gl     = b .&. 15
      m      = toEnum $ fromIntegral $ (c .&. 56) `shiftR` 3
      lgn = (conv (a .&. 24) `shiftL` 6) + (conv (c .&. 7) `shiftL` 6) + conv d
      gs     = (e .&. 60) `shiftR` 2
      lgf    = (conv (e .&. 2) `shiftR` 1) > 0
      lo1n =
        (conv (a .&. 4) `shiftL` 11)
          + (conv (e .&. 1) `shiftL` 12)
          + (conv f `shiftL` 6)
          + conv g
      lo1r = (h .&. 48) `shiftR` 5
      lo1f = ((h .&. 8) `shiftR` 3) > 0
      lo2f = ((h .&. 4) `shiftR` 2) > 0
      lo2i = ((h .&. 2) `shiftR` 1) > 0
      lo2n =
        (conv (a .&. 2) `shiftL` 12)
          + (conv (h .&. 1) `shiftL` 12)
          + (conv i `shiftL` 6)
          + conv j
    in
      ( cid
      , if not active
        then Nothing
        else Just $ Cell
          cid
          active
          los
          lgr
          gl
          m
          lgn
          gs
          lgf
          lo1n
          lo1r
          lo1f
          lo2f
          lo2i
          lo2n
          (if lo2i
            then Just (InteractiveObjectGfxId $ fromIntegral lo2n)
            else Nothing
          )
      )
  go _ = error "Invalid cell data"
