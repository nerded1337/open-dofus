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
{-# LANGUAGE OverloadedStrings  #-}

module OpenDofus.Game.Map.Parser
  ( module X
  , parseMap
  )
where

import           Data.Bits
import           Data.ByteString               as BS
import           Data.Either
import           Data.HashMap.Strict           as HM
import           Data.Text                     as T
import           Data.Text.Read                as T
import           Data.Vector                   as V
import qualified Network.URI.Encode            as URI
import           OpenDofus.Database
import           OpenDofus.Game.Map.Types      as X
import           OpenDofus.Prelude

prepareKey :: MapDataKey -> MapDataKey
prepareKey !key =
  MapDataKey
    $ URI.decodeText
    $ snd
    $ T.foldl' step (Nothing, mempty)
    $ unMapDataKey key
 where
  step (Nothing, !o) !c  = (Just c, o)
  step (Just !c, !o) !c' = (Nothing, o <> T.singleton (toEnum hex))
   where
    hex =
      fst
        $  fromRight (error "Invalid cell hex")
        $  T.hexadecimal
        $  T.singleton c
        <> T.singleton c'

keyChecksum :: MapDataKey -> Int
keyChecksum !key = final `mod` 16
 where
  final = T.foldl' step 0 $ unMapDataKey key
  step x c = x + (fromEnum c `mod` 16)

decompressMapData :: MapCompressedData -> MapDataKey -> ByteString
decompressMapData !dat !key = extract
  $ T.foldl' step initialState (unMapCompressedData dat)
 where
  preparedKey  = prepareKey key
  keyShift     = keyChecksum preparedKey * 2
  keyLength    = T.length (unMapDataKey preparedKey)
  initialState = (Nothing, 0, mempty)
  step (Nothing, !i, !o) !c = (Just c, i, o)
  step (Just !c, !i, !o) !c' =
    let dataHex =
            fst
              $  fromRight (error "Invalid cell dataHex")
              $  T.hexadecimal
              $  T.singleton c
              <> T.singleton c'
        keyCode =
            T.index (unMapDataKey preparedKey) ((i + keyShift) `mod` keyLength)
        o' = T.singleton $ toEnum $ dataHex `xor` fromEnum keyCode
    in  (Nothing, i + 1, o <> o')
  extract (_, _, o) = encodeTextStrict $ URI.decodeText o

parseMap :: Map -> Maybe (MapInstanceT (Maybe InteractiveObjectGfxId) Unit)
parseMap !m =
  go . decompressMapData (m ^. mapCompressedData) =<< m ^. mapDataKey
 where
  go !decompressedData = pure $ MapInstance
    m
    (HM.fromList . catMaybes $ V.toList
      (bitraverse pure id <$> parseCells decompressedData)
    )
    ()
   where
    compressedCellSize = 10

    nbOfCell           = BS.length decompressedData `quot` compressedCellSize

    cellSlice !cid =
      ( CellId $ fromIntegral cid
      , sliceStrictByteString
        (fromIntegral cid * compressedCellSize)
        ((fromIntegral cid * compressedCellSize) + compressedCellSize)
      )

    cellGenerator = V.generate (nbOfCell - 1) cellSlice

    cellSlices    = sequenceA $ bitraverse pure id <$> cellGenerator

    parseCells    = fmap (uncurry parseCell . second BS.unpack) . cellSlices
