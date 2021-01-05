{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- Movement.hs ---

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

module OpenDofus.Game.Map.Movement
  ( module X,
    SerializedMovementPath,
    CompressedMovementPath,
    extractFullPath,
    directionBetween,
    movementSpeed,
    movementDuration,
    maximumNumberOfSteps,
    serializeCompressedPath,
    deserializeCompressedPath,
  )
where

import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.DList as DL
import qualified Data.DList.DNonEmpty as DLN
import Data.Tuple
import qualified Data.Vector.Fixed as F
import qualified Data.Vector.Fixed.Primitive as F
import Linear.V2
import OpenDofus.Core.Data.Constructible
import OpenDofus.Database.Game
import OpenDofus.Game.Map.Cell
import OpenDofus.Game.Map.Compressor
import OpenDofus.Game.Map.Direction
import OpenDofus.Game.Map.Movement.Types as X
import OpenDofus.Game.Time
import OpenDofus.Prelude

type SerializedMovementPath = F.Vec 10 Word32

type CompressedMovementPath = BS.ByteString

toTime :: Double -> GameTime Millisecond
toTime = ms . fromIntegral . ceiling @_ @Int32
{-# INLINE toTime #-}

movementSpeed :: MovementModifier -> Direction -> GameTime Millisecond
movementSpeed MovementModifierSlide East = toTime $ cellWidth * 4 -- / 0.25
movementSpeed MovementModifierSlide West = toTime $ cellWidth * 4 -- / 0.25
movementSpeed MovementModifierSlide North = toTime $ cellHeight * 4 -- / 0.25
movementSpeed MovementModifierSlide South = toTime $ cellHeight * 4 -- / 0.25
movementSpeed MovementModifierSlide _ = toTime $ cellDiag * 4 -- / 0.25
movementSpeed MovementModifierWalk East = toTime $ cellWidth * 14.28 -- / 0.07
movementSpeed MovementModifierWalk West = toTime $ cellWidth * 14.28 -- / 0.07
movementSpeed MovementModifierWalk North = toTime $ cellHeight * 16.66 -- / 0.06
movementSpeed MovementModifierWalk South = toTime $ cellHeight * 16.66 -- / 0.06
movementSpeed MovementModifierWalk _ = toTime $ cellDiag * 16.66 -- / 0.06
movementSpeed MovementModifierRun East = toTime $ cellWidth * 5.88 -- / 0.17
movementSpeed MovementModifierRun West = toTime $ cellWidth * 5.88 -- / 0.17
movementSpeed MovementModifierRun North = toTime $ cellHeight * 6.66 -- / 0.15
movementSpeed MovementModifierRun South = toTime $ cellHeight * 6.66 -- / 0.15
movementSpeed MovementModifierRun _ = toTime $ cellDiag * 6.66 -- / 0.15
movementSpeed MovementModifierMount East = toTime $ cellWidth * 4.35 -- / 0.23
movementSpeed MovementModifierMount West = toTime $ cellWidth * 4.35 -- / 0.23
movementSpeed MovementModifierMount North = toTime $ cellHeight * 5 -- / 0.2
movementSpeed MovementModifierMount South = toTime $ cellHeight * 5 -- / 0.2
movementSpeed MovementModifierMount _ = toTime $ cellDiag * 5 -- / 0.2
{-# INLINE movementSpeed #-}

movementDuration :: MovementModifier -> MovementPath CellId -> GameTime Millisecond
movementDuration m p =
  foldl' step (ms 0) (DLN.tail $ p ^. movementPathSteps)
  where
    step t (MovementStep _ d) =
      t +:+ movementSpeed m d
{-# INLINE movementDuration #-}

directionDiscretisation :: MapWidth -> Direction -> Int32
directionDiscretisation w d =
  case d of
    East -> 1
    SouthEast -> w'
    South -> w' * 2 - 1
    SouthWest -> w' - 1
    West -> -1
    NorthWest -> - w'
    North -> - w' * 2 + 1
    NorthEast -> - w' + 1
  where
    w' = fromIntegral @MapWidth @Int32 w
{-# INLINE directionDiscretisation #-}

directionAtan :: CellPoint -> CellPoint -> Direction
directionAtan a b
  | t >= - pi8 && t < pi8 = East
  | t >= pi8 && t < pi3 = SouthEast
  | t >= pi3 && t < 2 * pi3 = South
  | t >= 2 * pi3 && t < 7 * pi8 = SouthWest
  | t >= 7 * pi8 || t < -7 * pi8 = West
  | t >= -7 * pi8 && t < -2 * pi3 = NorthWest
  | t >= -2 * pi3 && t < - pi3 = North
  | t >= - pi3 && t < - pi8 = NorthEast
  where
    d = b - a
    t = atan2 (fromIntegral $ d ^. _y) (fromIntegral $ d ^. _x)
    pi8 = pi / 8
    pi3 = pi / 3
{-# INLINE directionAtan #-}

directionFromCoord :: MapWidth -> CellId -> CellId -> Direction
directionFromCoord w a b
  | x == 0 && y > 0 = SouthWest
  | x == 0 = NorthEast
  | x > 0 = SouthEast
  | otherwise = NorthWest
  where
    begin = cellToPoint w a
    end = cellToPoint w b
    delta = end - begin
    x = delta ^. _x
    y = delta ^. _y

directionBetween :: MapWidth -> CellId -> CellId -> Direction
directionBetween w a b =
  directionAtan (cellToPoint w a) (cellToPoint w b)
{-# INLINE directionBetween #-}

compressStep :: MovementStep CellId -> Builder
compressStep s =
  word8 (forceEncode (fromIntegral (fromEnum (s ^. movementStepDirection)) .&. 7))
    <> word8 (forceEncode ((s ^. movementStepPoint . to unCellId .&. 4032) `shiftR` 6))
    <> word8 (forceEncode (s ^. movementStepPoint . to unCellId .&. 63))
  where
    forceEncode =
      fromMaybe (error "impossible")
        . encode64
        . fromIntegral

extractSteps ::
  MapWidth ->
  CellId ->
  CellId ->
  Direction ->
  DL.DList (MovementStep CellId)
extractSteps w a b d
  | len > 0 =
    flip MovementStep d
      . CellId
      . fromIntegral
      <$> [a' + gap, a' + gap + gap .. b']
  | otherwise =
    []
  where
    conv = fromIntegral @_ @Int32
    a' = conv a
    b' = conv b
    gap = directionDiscretisation w d
    len = (b' - a') `quot` gap

extractFullPath ::
  MapWidth ->
  CellId ->
  Direction ->
  CompressedMovementPath ->
  Maybe (MovementPath CellId)
extractFullPath w c d compressed =
  MovementPath fullyCompressed . fst
    <$!> extract compressed (DLN.singleton initialStep, initialStep)
  where
    initialStep =
      MovementStep c d
    fullyCompressed =
      toStrictBytes $
        toLazyByteString $
          compressStep initialStep <> byteString compressed
    app (x DLN.:| xs) ys = x DLN.:| xs <> ys
    extract (x :- y :- z :- r) (v, MovementStep k _) = do
      p@(MovementStep k' cd) <- mkStep x y z
      extract r (app v $ extractSteps w k k' cd, p)
    extract _ s =
      pure s
    mkStep x y z =
      step <$!> decode64 (toEnum $ fromEnum x)
        <*> decode64 (toEnum $ fromEnum y)
        <*> decode64 (toEnum $ fromEnum z)
    step x y z =
      MovementStep
        (CellId $ (fromIntegral (y .&. 15) `shiftL` 6) .|. fromIntegral z)
        (toEnum $ fromIntegral x)

maximumNumberOfSteps :: Word8
maximumNumberOfSteps = 15
{-# INLINE maximumNumberOfSteps #-}

{-
   Serialize a compressed path in binary format.
   A path P is composed of N steps.
   Each steps is composed of 3 char elements in range [a-zA-Z0-9\-\_].
   Each element is between 0 and 63 index.
   Encode each step in a word32, resulting in N (max of 15) word32s.
-}
serializeCompressedPath :: CompressedMovementPath -> Maybe SerializedMovementPath
serializeCompressedPath compressed
  | fromIntegral nbOfSteps > maximumNumberOfSteps = Nothing
  | otherwise = Just $ F.unfoldr serializeStep compressed
  where
    forceDecode =
      fromMaybe (error "The impossible happenned")
        . decode64
        . toEnum
        . fromEnum
    serializeStep (x :- y :- z :- r) =
      (encodeStep x y z, r)
    serializeStep r =
      -- Null step as MAX_WORD32
      (maxBound, r)
    encodeStep x y z =
      encodeStep32
        (forceDecode x)
        (forceDecode y)
        (forceDecode z)
    encodeStep32 x y z =
      conv x
        .|. ( (conv y `shiftL` 8)
                .|. conv z `shiftL` 16
            )
    conv = fromIntegral @Word8 @Word32
    len = BS.length compressed
    -- 3 chars per step
    nbOfSteps = len `quot` 3
{-# INLINE serializeCompressedPath #-}

deserializeCompressedPath :: SerializedMovementPath -> CompressedMovementPath
deserializeCompressedPath = F.foldMap decodeStep
  where
    conv = fromIntegral @Word32 @Word8
    forceEncode =
      fromMaybe (error "The impossible happenned")
        . encode64
        . conv
    decodeStep :: Word32 -> CompressedMovementPath
    decodeStep w
      | w == maxBound = mempty
      | otherwise =
        BS.singleton (forceEncode w)
          <> BS.singleton (forceEncode $ w `shiftR` 8)
          <> BS.singleton (forceEncode $ w `shiftR` 16)
{-# INLINE deserializeCompressedPath #-}
