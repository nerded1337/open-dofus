{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- Time.hs ---

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

module OpenDofus.Game.Time
  ( module T,
    GameTime,
    gameCurrentTime,
    gameDelay
  )
where

import OpenDofus.Prelude
import Time as T
import GHC.Clock

type GameTime unit = Time unit

gameCurrentTime :: MonadIO m => m (GameTime Millisecond)
gameCurrentTime =
  toUnit . ns . fromIntegral <$> liftIO getMonotonicTimeNSec
{-# INLINE gameCurrentTime #-}
{-# SPECIALIZE INLINE gameCurrentTime :: IO (GameTime Millisecond) #-}

gameDelay :: MonadIO m => GameTime Millisecond -> m ()
gameDelay = T.threadDelay
{-# INLINE gameDelay #-}
{-# SPECIALIZE INLINE gameDelay :: GameTime Millisecond -> IO () #-}
