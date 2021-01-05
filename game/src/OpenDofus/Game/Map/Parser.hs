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
import qualified Data.ByteString as BS
import Data.Either
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as T
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

decompressMapData :: MapCompressedData -> MapDataKey -> BS.ByteString
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

foldCells :: BS.ByteString -> Maybe (HM.HashMap CellId (Compose CellT Maybe InteractiveObjectGfxId))
foldCells decompressed = foldMap go [0 .. fromIntegral $ nbOfCells decompressed]
  where
    go i =
      let currentCell =
            sliceStrictByteString
              (fromIntegral i * compressedCellSize)
              (fromIntegral i * compressedCellSize + compressedCellSize)
              decompressed
       in HM.singleton i <$> (parseCell i currentCell)
{-# INLINE foldCells #-}

parseMap ::
  Map ->
  Either String (MapInstanceT (Compose CellT Maybe InteractiveObjectGfxId))
parseMap m = parse =<< maybe (Left "Missing data key") Right (m ^. mapDataKey)
  where
    parse =
      maybe (Left "Invalid key") Right
      . fmap (MapInstance m)
      . foldCells
      . decompressMapData (m ^. mapCompressedData)
