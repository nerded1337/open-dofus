-- Client.hs ---

-- Copyright (C) 2019 Nerd Ed

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

{-# LANGUAGE BangPatterns #-}

module OpenDofus.Core.Network.Client
  ( module X
  , receive
  , parse
  ) where

import           Data.Bytes.Types
import qualified Data.ByteString.Builder                  as BS
import           Data.Primitive
import qualified Data.Vector                              as V
import           GHC.Exts

import           OpenDofus.Core.Network.Client.Connection as X
import           OpenDofus.Core.Network.Client.Message    as X
import           OpenDofus.Core.Network.Client.State      as X
import           OpenDofus.Prelude

{-# INLINE receive #-}
receive ::
     (HasClientState a, MonadIO m) => a -> MutableBytes RealWorld -> m ()
receive !x !(MutableBytes !rarr !ro !rl) = liftIO $ do
  let buffer = x ^. clientState . clientStateBufferSegment
  (resize, barr, bo, bl') <-
    do (MutableBytes barr bo bl) <- readTVarIO buffer
       pure (bl < rl, barr, bo, rl)
  barr' <-
    if resize
      then resizeMutableByteArray barr bl'
      else pure barr
  copyMutableByteArray barr' bo rarr ro rl
  atomically $ writeTVar buffer (MutableBytes barr' bo bl')

{-# INLINE parse #-}
parse :: (HasClientState a, MonadIO m) => a -> m (V.Vector ClientMessage)
parse x = go mempty mempty 0 =<< readTVarIO (x ^. clientState . clientStateBufferSegment)
  where
    go ::
         MonadIO m
      => BS.Builder
      -> V.Vector ClientMessage
      -> Int
      -> MutableBytes RealWorld
      -> m (V.Vector ClientMessage)
    go !b !v !i !buff@(MutableBytes !arr !o !l)
      | i < l =
        liftIO $ do
          value <- readByteArray arr (o + i)
          case (value, i) of
            (0, 0) -> pure v
            (0, _) -> do
              moveByteArray arr o arr (o + i + 1) (l - i - 1)
              let newBuffer = MutableBytes arr o (l - i)
              atomically $ writeTVar (x ^. clientState . clientStateBufferSegment) newBuffer
              go
                mempty
                (V.snoc v $ ClientSent $ BS.toLazyByteString b)
                0
                newBuffer
            (10, _) -> go b v (i + 1) buff
            _ -> go (b <> BS.word8 value) v (i + 1) buff
      | otherwise = pure v
