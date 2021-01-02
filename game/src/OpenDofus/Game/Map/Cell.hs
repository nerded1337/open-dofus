{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

module OpenDofus.Game.Map.Cell
  ( module X,
    CellPoint,
    parseCell,
    cellFromChar,
    cellToChar,
    cellToPoint,
    cellHeight,
    cellHalfHeight,
    cellWidth,
    cellHalfWidth,
    cellDiag,
  )
where

import Data.Bits
import qualified Data.ByteString as BS
import Data.Fixed
import Linear.V2
import OpenDofus.Core.Data.Constructible
import OpenDofus.Database
import OpenDofus.Game.Map.Cell.Types as X
import OpenDofus.Game.Map.Compressor
import OpenDofus.Prelude

type CellPoint = V2 Int32

cellDiag :: Double
cellDiag = 29.75
{-# INLINE cellDiag #-}

cellHeight :: Double
cellHeight = 27
{-# INLINE cellHeight #-}

cellHalfHeight :: Double
cellHalfHeight = cellHeight / 2
{-# INLINE cellHalfHeight #-}

cellWidth :: Double
cellWidth = 53
{-# INLINE cellWidth #-}

cellHalfWidth :: Double
cellHalfWidth = cellWidth / 2
{-# INLINE cellHalfWidth #-}

cellToChar :: CellId -> Maybe (Word8, Word8)
cellToChar (CellId x) =
  (,) <$!> encode64 (fromIntegral $ x `quot` 64)
    <*> encode64 (fromIntegral $ x `mod` 64)
{-# INLINE cellToChar #-}

cellFromChar :: Word8 -> Word8 -> Maybe CellId
cellFromChar c1 c2 =
  case (decode64 c1, decode64 c2) of
    (Just i1, Just i2) ->
      Just $ CellId $ fromIntegral i1 * 64 + fromIntegral i2
    _ ->
      Nothing
{-# INLINE cellFromChar #-}

prepareCellData :: BS.ByteString -> Maybe [Word8]
prepareCellData = BS.foldr' go (Just [])
  where
    go _ Nothing = Nothing
    go e (Just eis) = (: eis) <$!> decode64 e
{-# INLINE prepareCellData #-}

parseCell ::
  CellId ->
  ByteString ->
  Maybe (CellId, Compose CellT Maybe InteractiveObjectGfxId)
parseCell cid = go <=< prepareCellData
  where
    go (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- _) =
      Just
        ( cid,
          Compose $
            Cell
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
              ( if lo2i > 0
                  then Just (InteractiveObjectGfxId $ fromIntegral lo2n)
                  else Nothing
              )
        )
      where
        conv = fromIntegral @_ @Word16
        convB = fromIntegral @_ @FastBool
        active = convB $ conv ((a .&. 32) `shiftR` 5)
        los = convB $ conv (a .&. 1)
        lgr = (fromIntegral b .&. 48) `shiftR` 4
        gl = fromIntegral b .&. 15
        m = cellMovementType $ (c .&. 56) `shiftR` 3
        lgn = (conv (a .&. 24) `shiftL` 6) + (conv (c .&. 7) `shiftL` 6) + conv d
        gs = (fromIntegral e .&. 60) `shiftR` 2
        lgf = convB (conv (e .&. 2) `shiftR` 1)
        lo1n =
          (conv (a .&. 4) `shiftL` 11)
            + (conv (e .&. 1) `shiftL` 12)
            + (conv f `shiftL` 6)
            + conv g
        lo1r = (fromIntegral h .&. 48) `shiftR` 5
        lo1f = convB (conv (h .&. 8) `shiftR` 3)
        lo2f = convB (conv (h .&. 4) `shiftR` 2)
        lo2i = convB (conv (h .&. 2) `shiftR` 1)
        lo2n =
          (conv (a .&. 2) `shiftL` 12)
            + (conv (h .&. 1) `shiftL` 12)
            + (conv i `shiftL` 6)
            + conv j
    go _ = Nothing
{-# INLINE parseCell #-}

cellToPoint :: MapWidth -> CellId -> CellPoint
cellToPoint w c = V2 (round x) (round y)
  where
    c' = fromIntegral @CellId @Double c
    w' = fromIntegral @MapWidth @Double w

    w2 = w' * 2 - 1
    a = fromIntegral $ floor @Double @Int32 (c' / w2)
    b = (c' - a * w2) `mod'` w'

    y = a - b
    x = (c' - (w' - 1) * y) / w'
{-# INLINE cellToPoint #-}
