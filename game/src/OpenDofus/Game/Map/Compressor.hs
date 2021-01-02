{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- Compressor.hs ---

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

module OpenDofus.Game.Map.Compressor
  ( encode64,
    decode64,
  )
where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import OpenDofus.Prelude

decodeHash :: HM.HashMap Word8 Word8
decodeHash =
  HM.fromList $
    zip
      (BS.unpack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")
      [0 ..]
{-# INLINE decodeHash #-}

encodeHash :: HM.HashMap Word8 Word8
encodeHash =
  HM.fromList $
    zip
      [0 ..]
      (BS.unpack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")
{-# INLINE encodeHash #-}

encode64 :: Word8 -> Maybe Word8
encode64 = flip HM.lookup encodeHash
{-# INLINE encode64 #-}

decode64 :: Word8 -> Maybe Word8
decode64 = flip HM.lookup decodeHash
{-# INLINE decode64 #-}
