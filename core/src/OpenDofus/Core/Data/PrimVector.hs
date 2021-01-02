{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE BangPatterns #-}

-- PrimVector.hs ---

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

module OpenDofus.Core.Data.PrimVector where

import Control.Monad.Primitive
import Data.Primitive
import Data.Proxy
import qualified Data.Vector.Fixed as F
import qualified Data.Vector.Fixed.Primitive as F
import GHC.Base
import GHC.TypeLits
import OpenDofus.Core.Data.Prim
import OpenDofus.Prelude

instance (F.Arity n, KnownNat n, Prim a) => Prim (F.Vec n a) where
  sizeOf# _ =
    case fromIntegral (natVal (Proxy @n)) of
      I# i# -> i# *# sizeOf# @a undefined
  {-# INLINE sizeOf# #-}

  alignment# _ =
    alignment# @a undefined
  {-# INLINE alignment# #-}

  indexByteArray# arr# i# =
    let i = I# i#
        arr = ByteArray arr#
        elemSize = fromIntegral $ natVal (Proxy @n)
        offset = elemSize * i
     in F.generate (indexByteArray @a arr . (offset +))
  {-# INLINE indexByteArray# #-}

  readByteArray# ::
    forall s.
    MutableByteArray# s ->
    Int# ->
    State# s ->
    (# State# s, F.Vec n a #)
  readByteArray# arr# i# =
    internal $ do
      let i = I# i#
          arr = MutableByteArray arr#
          elemSize = fromIntegral $ natVal (Proxy @n)
          offset = elemSize * i
          go k = readByteArray @a @(ST s) arr (offset + k)
      F.generateM go
  {-# INLINE readByteArray# #-}

  writeByteArray# ::
    forall s.
    MutableByteArray# s ->
    Int# ->
    F.Vec n a ->
    State# s ->
    State# (PrimState (ST s))
  writeByteArray# arr# i# v =
    internal_ $ do
      let i = I# i#
          arr = MutableByteArray arr#
          elemSize = fromIntegral $ natVal (Proxy @n)
          offset = elemSize * i
          go k = writeByteArray @a @(ST s) arr (offset + k)
      F.imapM_ go v
  {-# INLINE writeByteArray# #-}

  indexOffAddr# addr# i# =
    let !(I# elemSize#) = fromIntegral $ natVal (Proxy @n)
        offset# = elemSize# *# i#
        go (I# k#) =
          indexOffAddr# @a addr# (offset# +# k#)
     in F.generate go
  {-# INLINE indexOffAddr# #-}

  readOffAddr# ::
    forall s.
    Addr# ->
    Int# ->
    State# s ->
    (# State# s, F.Vec n a #)
  readOffAddr# addr# i# = internal $ do
    let !(I# elemSize#) = fromIntegral $ natVal (Proxy @n)
        offset# = elemSize# *# i#
        go (I# k#) =
          readOffAddr# @a addr# (offset# +# k#)
    F.generateM (primitive @(ST s) . go)
  {-# INLINE readOffAddr# #-}

  writeOffAddr# ::
    forall s.
    Addr# ->
    Int# ->
    F.Vec n a ->
    State# s ->
    State# (PrimState (ST s))
  writeOffAddr# addr# i# v = internal_ $ do
    let !(I# elemSize#) = fromIntegral $ natVal (Proxy @n)
        offset# = elemSize# *# i#
        go (I# k#) =
          writeOffAddr# @a addr# (offset# +# k#)
    F.imapM_ (\i x -> primitive_ @(ST s) $ go i x) v
  {-# INLINE writeOffAddr# #-}

  setByteArray# = defaultSetByteArray#
  {-# INLINE setByteArray# #-}

  setOffAddr# = defaultSetOffAddr#
  {-# INLINE setOffAddr# #-}
