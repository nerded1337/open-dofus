{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

module OpenDofus.Game.Map.Cell.Parser
  ( parseCell,
  )
where

import Data.Bits
import Data.ByteString as BS
import OpenDofus.Core.Data.Constructible
import OpenDofus.Database
import OpenDofus.Game.Map.Cell.Types
import OpenDofus.Prelude

hashCell :: ByteString
hashCell = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"
{-# INLINE hashCell #-}

prepareCellData :: ByteString -> Maybe [Int]
prepareCellData = BS.foldr' go (Just [])
  where
    go _ Nothing = Nothing
    go e (Just eis) =
      case BS.elemIndex e hashCell of
        Just ei ->
          Just (ei : eis)
        Nothing ->
          Nothing
{-# INLINE prepareCellData #-}

parseCell ::
  CellId ->
  ByteString ->
  Maybe (CellId, CellT (Maybe InteractiveObjectGfxId))
parseCell cid = go <=< prepareCellData
  where
    go (a :- b :- c :- d :- e :- f :- g :- h :- i :- j :- _) =
      Just
        ( cid,
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
            ( if lo2i
                then Just (InteractiveObjectGfxId $ fromIntegral lo2n)
                else Nothing
            )
        )
      where
        conv = fromIntegral @_ @Word16
        active = ((a .&. 32) `shiftR` 5) > 0
        los = (a .&. 1) == 1
        lgr = (fromIntegral b .&. 48) `shiftR` 4
        gl = fromIntegral b .&. 15
        m = toEnum $ fromIntegral $ (c .&. 56) `shiftR` 3
        lgn = (conv (a .&. 24) `shiftL` 6) + (conv (c .&. 7) `shiftL` 6) + conv d
        gs = (fromIntegral e .&. 60) `shiftR` 2
        lgf = (conv (e .&. 2) `shiftR` 1) > 0
        lo1n =
          (conv (a .&. 4) `shiftL` 11)
            + (conv (e .&. 1) `shiftL` 12)
            + (conv f `shiftL` 6)
            + conv g
        lo1r = (fromIntegral h .&. 48) `shiftR` 5
        lo1f = ((h .&. 8) `shiftR` 3) > 0
        lo2f = ((h .&. 4) `shiftR` 2) > 0
        lo2i = ((h .&. 2) `shiftR` 1) > 0
        lo2n =
          (conv (a .&. 2) `shiftL` 12)
            + (conv (h .&. 1) `shiftL` 12)
            + (conv i `shiftL` 6)
            + conv j
    go _ = Nothing
{-# INLINE parseCell #-}
