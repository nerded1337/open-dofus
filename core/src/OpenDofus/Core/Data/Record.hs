{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStrict #-}

-- Record.hs ---

-- Copyright (C) 2021 Nerd Ed

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

module OpenDofus.Core.Data.Record
  ( (:<*>:) (..),
    (:<+>:) (..),
    (:^:) (..),
    (:<:) (..),
    LengthOf,
    StorablePrim (..),
    pattern Match,
  )
where

import Control.Monad.Primitive
import Data.Primitive as P
import Foreign
import qualified Foreign.Storable as S
import GHC.Exts
import GHC.TypeNats
import OpenDofus.Core.Data.Prim
import OpenDofus.Prelude
import System.IO.Unsafe

data a :<*>: b = !a :<*>: !b

instance (Show a, Show b) => Show (a :<*>: b) where
  show (x :<*>: y) =
    "(" <> show x <> " :<*>: " <> show y <> ")"

instance (Eq a, Eq b) => Eq (a :<*>: b) where
  x :<*>: y == x' :<*>: y' =
    x == x' && y == y'

instance (Semigroup a, Semigroup b) => Semigroup (a :<*>: b) where
  (x :<*>: y) <> (x' :<*>: y') =
    (x <> x') :<*>: (y <> y')

instance (Monoid a, Monoid b) => Monoid (a :<*>: b) where
  mempty = mempty :<*>: mempty

infixl 9 :<*>:

data a :<+>: b = L !a | R !b

instance (Show a, Show b) => Show (a :<+>: b) where
  show (L x) = "L " <> show x
  show (R y) = "R " <> show y

instance (Eq a, Eq b) => Eq (a :<+>: b) where
  L x == L x' = x == x'
  R x == R x' = x == x'
  _ == _ = False

infixl 8 :<+>:

class f :^: g where
  frag :: g -> f

instance f :^: f where
  frag = id

instance {-# OVERLAPS #-} g :^: (f :<*>: g) where
  frag (_ :<*>: y) = y

instance (f :^: g) => f :^: (g :<*>: h) where
  frag (x :<*>: _) = frag x

class f :<: g where
  inj :: f -> g
  proj :: g -> Maybe f

instance f :<: f where
  inj = id
  proj = Just

instance {-# OVERLAPS #-} g :<: (f :<+>: g) where
  inj = R
  proj (L _) = Nothing
  proj (R y) = Just y

instance (f :<: g) => f :<: (g :<+>: h) where
  inj = L . inj
  proj (L x) = proj x
  proj (R _) = Nothing

pattern Match :: f :<: g => f -> g
pattern Match x <-
  (proj -> Just x)
  where
    Match x = inj x

type family LengthOf a :: Nat where
  LengthOf (x :<*>: y) = LengthOf x + LengthOf y
  LengthOf _ = 1

newtype StorablePrim a = StorablePrim {unStorablePrim :: a}
  deriving newtype (Show, Eq, Storable)

unsafePeekMutable :: forall a s. Storable a => Int -> MutableByteArray s -> StorablePrim a
unsafePeekMutable off =
  unsafeDupablePerformIO
    . fmap StorablePrim
    . flip S.peekByteOff off
    . castPtr
    . mutableByteArrayContents
{-# INLINE unsafePeekMutable #-}

unsafePokeMutable :: forall a s. Storable a => Int -> MutableByteArray s -> StorablePrim a -> ()
unsafePokeMutable off arr =
  unsafeDupablePerformIO
    . S.pokeByteOff (mutableByteArrayContents arr) off
    . unStorablePrim
{-# INLINE unsafePokeMutable #-}

instance Storable a => Prim (StorablePrim a) where
  sizeOf# _ =
    case S.sizeOf @a undefined of
      I# i# -> i#
  {-# INLINE sizeOf# #-}

  alignment# _ =
    case S.alignment @a undefined of
      I# i# -> i#
  {-# INLINE alignment# #-}

  readByteArray# ::
    forall s.
    MutableByteArray# s ->
    Int# ->
    State# s ->
    (# State# s, StorablePrim a #)
  readByteArray# arr# i# =
    internal @(ST s) $ do
      let i = I# i#
          arr = MutableByteArray arr#
          elemSize = S.sizeOf @a undefined
          offset = elemSize * i
      pure $! unsafePeekMutable offset arr
  {-# INLINE readByteArray# #-}

  writeByteArray# ::
    forall s.
    MutableByteArray# s ->
    Int# ->
    StorablePrim a ->
    State# s ->
    State# s
  writeByteArray# arr# i# x =
    internal_ @(ST s) $ do
      let i = I# i#
          arr = MutableByteArray arr#
          elemSize = S.sizeOf @a undefined
          offset = elemSize * i
      pure $! unsafePokeMutable offset arr x
  {-# INLINE writeByteArray# #-}

  indexByteArray# =
    error "Not implemented"

  --   let i = I# i#
  --       arr = ByteArray arr#
  --       elemSize = S.sizeOf @a undefined
  --       offset = elemSize * i
  --    in unsafePeek offset arr
  -- {-# INLINE indexByteArray# #-}

  indexOffAddr# =
    error "Not implemented"

  readOffAddr# =
    error "Not implemented"

  writeOffAddr# =
    error "Not implemented"

  setByteArray# =
    error "Not implemented"

  setOffAddr# =
    error "Not implemented"

instance (S.Storable a, S.Storable b) => Storable (a :<*>: b) where
  sizeOf _ =
    S.sizeOf @a undefined
      + S.sizeOf @b undefined
  {-# INLINE sizeOf #-}

  alignment _ =
    max
      (S.alignment @a undefined)
      (S.alignment @b undefined)
  {-# INLINE alignment #-}

  peek p = do
    x <- peekByteOff p 0
    y <- peekByteOff p $ S.sizeOf @a undefined
    pure $ x :<*>: y
  {-# INLINE peek #-}

  poke p (x :<*>: y) = do
    pokeByteOff p 0 x
    pokeByteOff p (S.sizeOf @a undefined) y
  {-# INLINE poke #-}

instance (S.Storable a, S.Storable b) => Storable (a :<+>: b) where
  sizeOf _ =
    S.sizeOf @Word8 undefined
      + S.sizeOf @a undefined
      + S.sizeOf @b undefined
  {-# INLINE sizeOf #-}

  alignment _ =
    nextPowerOfTwo $
      S.sizeOf @Word8 undefined
        + max
          (S.alignment @a undefined)
          (S.alignment @b undefined)
  {-# INLINE alignment #-}

  peek p = do
    tag <- peekByteOff @Word8 p 0
    case tag of
      0 -> L <$> peekByteOff p 1
      1 -> R <$> peekByteOff p 1
      _ -> error "The impossible happenned"
  {-# INLINE peek #-}

  poke p xy = do
    case xy of
      L x -> do
        pokeByteOff @Word8 p 0 0
        pokeByteOff p 1 x
      R y -> do
        pokeByteOff @Word8 p 0 1
        pokeByteOff p 1 y
  {-# INLINE poke #-}
