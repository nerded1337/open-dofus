-- Prelude.hs ---

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

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module OpenDofus.Prelude
  ( module X
  , IP(..)
  , Port(..)
  , Unit
  , LazyByteString
  , Verbose
  , OpenDofusApp(..)
  , HasOpenDofusApp(..)
  , runOpenDofusApp
  , encodeTextLazy
  , encodeTextStrict
  , decodeLazyByteString
  , decodeStrictByteString
  , traverseCollapse
  , sliceLazyByteString
  , sliceStrictByteString
  , showText
  , showByteString
  , bind2
  , traverseFirst
  , traverseSecond
  )
where

import           Control.Applicative           as X
import           Control.Lens                  as X
import           Data.Bifoldable               as X
import           Data.Bifunctor                as X
import           Data.Bitraversable            as X
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import qualified Data.ByteString.Lazy          as LBS
import           Data.Coerce                   as X
import           Data.Foldable                 as X
import           Data.Int                      as X
import           Data.Maybe                    as X
import           Data.Semigroup                as X
import           Data.Text                     as T
import           Data.Text.Encoding
import           Data.UUID                     as X
                                         hiding ( fromString
                                                , null
                                                )
import           Data.UUID.V4                  as X
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax
import           RIO                           as X
                                         hiding ( ASetter
                                                , ASetter'
                                                , Getting
                                                , Lens
                                                , Lens'
                                                , Map
                                                , first
                                                , lens
                                                , map
                                                , over
                                                , second
                                                , set
                                                , sets
                                                , to
                                                , view
                                                , preview
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , (.~)
                                                , (%~)
                                                )
import           RIO.Partial                   as X
                                                ( toEnum )
import           RIO.Process                   as X
import           RIO.Time                      as X

newtype IP =
  IP
    { unIP :: Text
    }
  deriving newtype ( Show
                   , Eq
                   , Ord
                   , IsString
                   , FromBackendRow Postgres
                   , HasSqlEqualityCheck Postgres
                   , HasSqlValueSyntax PgValueSyntax
                   )

newtype Port =
  Port
    { unPort :: Word16
    }
  deriving newtype ( Show
                   , Eq
                   , Ord
                   , Num
                   , Real
                   , Enum
                   , Integral
                   , FromBackendRow Postgres
                   , HasSqlEqualityCheck Postgres
                   , HasSqlValueSyntax PgValueSyntax
                   )

type Unit = ()

type LazyByteString = LBS.ByteString

type Verbose = Bool

data OpenDofusApp = OpenDofusApp
    { _openDofusAppLogFunc        :: {-# UNPACK #-} !LogFunc
    , _openDofusAppProcessContext :: {-# UNPACK #-} !ProcessContext
    }

makeClassy ''OpenDofusApp

instance HasLogFunc OpenDofusApp where
  logFuncL = lens _openDofusAppLogFunc (\x y -> x { _openDofusAppLogFunc = y })

instance HasProcessContext OpenDofusApp where
  processContextL = lens _openDofusAppProcessContext
                         (\x y -> x { _openDofusAppProcessContext = y })

{-# INLINE runOpenDofusApp #-}
runOpenDofusApp :: MonadIO m => Verbose -> RIO OpenDofusApp a -> m a
runOpenDofusApp !verbose !f = liftIO $ do
  !lo <- logOptionsHandle stderr verbose
  !pc <- mkDefaultProcessContext
  withLogFunc lo
    $ \lf -> let !simpleApp = OpenDofusApp lf pc in runRIO simpleApp f

{-# INLINE encodeTextLazy #-}
encodeTextLazy :: Text -> LazyByteString
encodeTextLazy = LBS.fromStrict . encodeUtf8

{-# INLINE encodeTextStrict #-}
encodeTextStrict :: Text -> ByteString
encodeTextStrict = encodeUtf8

{-# INLINE decodeLazyByteString #-}
decodeLazyByteString :: LazyByteString -> Text
decodeLazyByteString = decodeUtf8 . LBS.toStrict

{-# INLINE decodeStrictByteString #-}
decodeStrictByteString :: ByteString -> Text
decodeStrictByteString = decodeUtf8

{-# INLINE sliceLazyByteString #-}
sliceLazyByteString :: Int64 -> Int64 -> LazyByteString -> LazyByteString
sliceLazyByteString start end = LBS.take (end - start) . LBS.drop start

{-# INLINE sliceStrictByteString #-}
sliceStrictByteString :: Int -> Int -> ByteString -> ByteString
sliceStrictByteString start end = BS.take (end - start) . BS.drop start

{-# INLINE bind2 #-}
bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 !f !x !y = join $ f <$> x <*> y

{-# INLINE traverseCollapse #-}
traverseCollapse
  :: (Traversable f, Monad f, Monad m) => (a -> m (f b)) -> f a -> m (f b)
traverseCollapse !f !x = join <$> traverse f x

{-# INLINE showText #-}
showText :: Show a => a -> Text
showText = T.pack . show

{-# INLINE showByteString #-}
showByteString :: Show a => a -> ByteString
showByteString = BSC.pack . show

{-# INLINE traverseFirst #-}
traverseFirst
  :: (Bitraversable f, Applicative g) => (a -> g b) -> f a c -> g (f b c)
traverseFirst f = bitraverse f pure

{-# INLINE traverseSecond #-}
traverseSecond
  :: (Bitraversable f, Applicative g) => (b -> g c) -> f a b -> g (f a c)
traverseSecond = bitraverse pure
