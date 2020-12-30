{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

module OpenDofus.Game.Map.Parser
  ( parseMap,
  )
where

import Data.Bits
import Data.ByteString as BS
import Data.Either
import qualified Data.HashMap.Strict as HM
import Data.Text as T
import Data.Text.Read as T
import qualified Network.URI.Encode as URI
import OpenDofus.Database
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Types
import OpenDofus.Prelude

prepareKey :: MapDataKey -> MapDataKey
prepareKey (MapDataKey key) =
  MapDataKey $
    URI.decodeText $
      snd $
        T.foldl'
          step
          (Nothing, mempty)
          key
  where
    step (Nothing, o) c = (Just c, o)
    step (Just c, o) c' = (Nothing, o <> T.singleton (toEnum hex))
      where
        hex =
          fst $
            fromRight (error "Invalid cell hex") $
              T.hexadecimal $
                T.singleton c
                  <> T.singleton c'
{-# INLINE prepareKey #-}

keyChecksum :: MapDataKey -> Int
keyChecksum (MapDataKey key) = final `mod` 16
  where
    final = T.foldl' step 0 key
    step x c = x + (fromEnum c `mod` 16)
{-# INLINE keyChecksum #-}

decompressMapData :: MapCompressedData -> MapDataKey -> ByteString
decompressMapData dat key =
  extract $
    T.foldl' step initialState (unMapCompressedData dat)
  where
    preparedKey = prepareKey key
    keyShift = keyChecksum preparedKey * 2
    keyLength = T.length (unMapDataKey preparedKey)
    initialState = (Nothing, 0, mempty)
    step (Nothing, i, o) c = (Just c, i, o)
    step (Just c, i, o) c' = (Nothing, i + 1, o <> o')
      where
        dataHex =
          fst $
            fromRight (error "Invalid cell dataHex") $
              T.hexadecimal $
                T.singleton c
                  <> T.singleton c'
        keyCode =
          T.index (unMapDataKey preparedKey) ((i + keyShift) `mod` keyLength)
        o' = T.singleton $ toEnum $ dataHex `xor` fromEnum keyCode
    extract (_, _, o) = encodeTextStrict $ URI.decodeText o
{-# INLINE decompressMapData #-}

compressedCellSize :: Int
compressedCellSize = 10

nbOfCells :: BS.ByteString -> Int
nbOfCells d = BS.length d `quot` compressedCellSize
{-# INLINE nbOfCells #-}

cellSlice :: Int -> (CellId, ByteString -> ByteString)
cellSlice cid =
  ( CellId $ fromIntegral cid,
    sliceStrictByteString
      (fromIntegral cid * compressedCellSize)
      ((fromIntegral cid * compressedCellSize) + compressedCellSize)
  )
{-# INLINE cellSlice #-}

cellGenerator :: Int -> [(CellId, ByteString -> ByteString)]
cellGenerator cells = cellSlice <$> [0 .. cells - 1]
{-# INLINE cellGenerator #-}

cellSlices :: Int -> ByteString -> [(CellId, ByteString)]
cellSlices cells =
  sequenceA $ bitraverse pure id <$> cellGenerator cells
{-# INLINE cellSlices #-}

parseCells ::
  ByteString ->
  Maybe [(CellId, Compose CellT Maybe InteractiveObjectGfxId)]
parseCells decompressed =
  traverse
    (uncurry parseCell)
    (cellSlices (nbOfCells decompressed) decompressed)
{-# INLINE parseCells #-}

parseMap ::
  Map ->
  Either String (MapInstanceT (Compose CellT Maybe InteractiveObjectGfxId))
parseMap m = parse =<< maybe (Left "Missing data key") Right (m ^. mapDataKey)
  where
    parse =
      go . decompressMapData (m ^. mapCompressedData)

    go decompressedData =
      maybe (Left "Invalid key") Right $
        MapInstance m . HM.fromList <$> parseCells decompressedData
{-# INLINE parseMap #-}
