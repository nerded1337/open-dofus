-- Text.hs ---

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

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module OpenDofus.Data.Constructible
  ( Constructible(..)
  , pattern (:-)
  , intersperseC
  ) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Sequence              as S
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as VU
import           RIO
import qualified RIO.Text                   as T

pattern (:-) :: Constructible a => Elem a -> a -> a
pattern x :- xs <- (uncons -> Just (x, xs))
  where x :- xs = cons x xs

class Constructible a where
  type Elem a :: *
  cons :: Elem a -> a -> a
  uncons :: a -> Maybe (Elem a, a)

instance Constructible [a] where
  type Elem [a] = a
  {-# INLINE cons #-}
  cons = (:)
  {-# INLINE uncons #-}
  uncons (x:xs) = Just (x, xs)
  uncons []     = Nothing

instance Constructible T.Text where
  type Elem T.Text = Char
  {-# INLINE cons #-}
  cons = T.cons
  {-# INLINE uncons #-}
  uncons = T.uncons

instance Constructible (V.Vector a) where
  type Elem (V.Vector a) = a
  {-# INLINE cons #-}
  cons = V.cons
  {-# INLINE uncons #-}
  uncons v
    | len == 0  = Nothing
    | otherwise = Just (v V.! 0, V.tail v)
    where
      len :: Int
      len = V.length v

instance VU.Unbox a => Constructible (VU.Vector a) where
  type Elem (VU.Vector a) = a
  {-# INLINE cons #-}
  cons = VU.cons
  {-# INLINE uncons #-}
  uncons v
    | len == 0  = Nothing
    | otherwise = Just (v VU.! 0, VU.tail v)
    where
      len :: Int
      len = VU.length v

instance Constructible (S.Seq a) where
  type Elem (S.Seq a) = a
  {-# INLINE cons #-}
  cons = (S.<|)
  {-# INLINE uncons #-}
  uncons s = case S.viewl s of
    (x S.:< xs) -> Just (x, xs)
    _           -> Nothing

instance Constructible BS.ByteString where
  type Elem BS.ByteString = Char
  {-# INLINE cons #-}
  cons = BS.cons
  {-# INLINE uncons #-}
  uncons = BS.uncons

instance Constructible LBS.ByteString where
  type Elem LBS.ByteString = Char
  {-# INLINE cons #-}
  cons = LBS.cons
  {-# INLINE uncons #-}
  uncons = LBS.uncons

intersperseC :: Constructible a => Elem a -> a -> a
intersperseC a (x :- xs) =
  let go !(x' :- xs') = a :- (x' :- go xs')
      go !xs'         = xs'
  in x :- go xs
intersperseC _ xs                = xs
