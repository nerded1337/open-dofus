-- Types.hs ---

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

{-# LANGUAGE TypeApplications #-}

module OpenDofus.Game.Types
  ( GameTime
  , getGameTime
  , gameTimeMilliseconds
  , millisecondsToGameTime
  )
where

import           OpenDofus.Prelude
import qualified System.Clock                  as C

newtype GameTime = GameTime C.TimeSpec

getGameTime :: MonadIO m => m GameTime
getGameTime = liftIO $ GameTime <$> C.getTime C.Realtime

_fromInteger :: Integer -> C.TimeSpec
_fromInteger i =
  let s  = fromIntegral @Integer $ i `quot` 1000000000
      ns = fromIntegral @Integer $ i `rem` 1000000000
  in  C.TimeSpec s ns

_toInteger :: C.TimeSpec -> Integer
_toInteger ts =
  let s  = fromIntegral @Int64 $ C.sec ts
      ns = fromIntegral @Int64 $ C.nsec ts
  in  s * 1000000000 + ns

gameTimeMilliseconds :: GameTime -> Int
gameTimeMilliseconds (GameTime ts) =
  fromIntegral $ _toInteger ts `quot` 1000000

millisecondsToGameTime :: Int -> C.TimeSpec
millisecondsToGameTime i = _fromInteger $ fromIntegral i * 1000000
