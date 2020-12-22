{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

module OpenDofus.Database.Types where

import Data.Binary as B (Binary, Word, decodeOrFail, encode)
import Data.ByteString.Builder (byteString, char8)
import qualified Data.ByteString.Lazy as BS
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Pool (Pool)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector as V
import Database.Beam (DataType (..), FromBackendRow, Typeable)
import Database.Beam.Backend
  ( HasSqlValueSyntax (..),
    IsSql92DataTypeSyntax (intType, timestampType, varCharType),
  )
import Database.Beam.Migrate.Generics
  ( HasDefaultSqlDataType (defaultSqlDataType),
  )
import Database.Beam.Postgres
  ( Connection,
    Pg,
    PgSyntax,
    Postgres,
    ResultError (ConversionFailed),
  )
import Database.Beam.Postgres.Syntax
  ( PgDataTypeSyntax (PgDataTypeSyntax),
    PgValueSyntax,
    defaultPgValueSyntax,
    pgByteaType,
    pgRenderSyntaxScript,
    pgUnboundedArrayType,
  )
import Database.PostgreSQL.Simple.FromField
  ( FromField (..),
    returnError,
  )
import Database.PostgreSQL.Simple.ToField
  ( Action (Many, Plain),
    ToField (..),
  )
import qualified Database.PostgreSQL.Simple.Types as T
import OpenDofus.Core.Data.Constructible (intersperseC)
import OpenDofus.Prelude
  ( Applicative (pure),
    Bool (False, True),
    Bounded,
    Either (Left, Right),
    Enum (..),
    Eq,
    Functor (fmap),
    Maybe (Nothing),
    Ord ((>)),
    Read,
    Semigroup ((<>)),
    Show,
    UTCTime,
    id,
    ($),
    (.),
    (<$>),
  )

class (forall a. Coercible (q a) (Pg a), forall a. Coercible (Pg a) (q a)) => IsPg q where
  toPg :: q a -> Pg a
  toPg = coerce
  {-# INLINE toPg #-}

  fromPg :: Pg a -> q a
  fromPg = coerce
  {-# INLINE fromPg #-}

instance IsPg Pg where
  toPg = id
  {-# INLINE toPg #-}

  fromPg = id
  {-# INLINE fromPg #-}

class IsPg (QueryTypeOf a) => HasQueryType a where
  type QueryTypeOf a :: Type -> Type

class (HasQueryType b, Coercible b Connection) => HasConnectPool a b where
  getConnectionPool :: a -> Pool b

instance (HasQueryType a, Coercible a Connection) => HasConnectPool (Pool a) a where
  getConnectionPool = id
  {-# INLINE getConnectionPool #-}

newtype BinaryField a = BinaryField
  { unBinaryField :: a
  }

instance (FromField a, Binary a, Typeable a) => FromBackendRow Postgres (BinaryField a)

instance Binary a => ToField (BinaryField a) where
  toField = toField . T.Binary . B.encode . unBinaryField

instance (Binary a, Typeable a) => FromField (BinaryField a) where
  fromField u v = do
    x <- fromField u v
    case decodeOrFail $ T.fromBinary x of
      Right (_, _, s) -> pure $ BinaryField s
      Left _ -> returnError ConversionFailed u "Unknown BinaryField type"

instance Binary a => HasDefaultSqlDataType Postgres (BinaryField a) where
  defaultSqlDataType _ _ _ = pgByteaType

instance
  (HasDefaultSqlDataType Postgres a, Binary a) =>
  HasSqlValueSyntax PgValueSyntax (BinaryField a)
  where
  sqlValueSyntax = defaultPgValueSyntax

newtype EnumField a = EnumField
  { unEnumField :: a
  }

instance (FromField a, Enum a, Bounded a, Typeable a) => FromBackendRow Postgres (EnumField a)

instance (Enum a, Bounded a) => ToField (EnumField a) where
  toField = toField . fromEnum . unEnumField

instance (Enum a, Bounded a, Typeable a) => FromField (EnumField a) where
  fromField u v = EnumField . toEnum <$> fromField u v

instance (Enum a, Bounded a) => HasDefaultSqlDataType Postgres (EnumField a) where
  defaultSqlDataType _ _ _ = pgByteaType

instance (HasDefaultSqlDataType Postgres a, Enum a, Bounded a) => HasSqlValueSyntax PgValueSyntax (EnumField a) where
  sqlValueSyntax = defaultPgValueSyntax

newtype PgArray a = PgArray
  { unPgArray :: V.Vector a
  }
  deriving newtype (Show, Eq)

instance (FromField a, FromBackendRow Postgres a, Typeable a) => FromBackendRow Postgres (PgArray a)

instance (FromField a, Typeable a) => FromField (PgArray a) where
  fromField u v = PgArray <$> fromField u v

instance (ToField a, HasDefaultSqlDataType Postgres a) => ToField (PgArray a) where
  toField (PgArray arr) =
    if V.length arr > 0
      then
        Many $
          Plain (byteString "ARRAY[") :
          V.toList (intersperseC (Plain (char8 ',')) . fmap toField $ arr)
            <> [ Plain
                   ( byteString "]::"
                       <> byteString (BS.toStrict $ pgRenderSyntaxScript getSyntax)
                       <> byteString "[]"
                   )
               ]
      else Plain (byteString "'{}'")
    where
      getSyntax :: PgSyntax
      getSyntax =
        let (PgDataTypeSyntax _ syntax _) =
              defaultSqlDataType (Proxy @a) (Proxy @Postgres) False
         in syntax

instance (HasDefaultSqlDataType Postgres a, ToField a) => HasSqlValueSyntax PgValueSyntax (PgArray a) where
  sqlValueSyntax = defaultPgValueSyntax

enumType :: (Enum a, Bounded a) => DataType Postgres a
enumType = DataType intType

textShowType :: (Show a, Read a) => Maybe Word -> DataType Postgres a
textShowType size = DataType (varCharType size Nothing)

vectorType :: Typeable a => DataType Postgres a -> DataType Postgres (PgArray a)
vectorType (DataType elemTy) = DataType $ pgUnboundedArrayType elemTy

coerceType :: Coercible a b => DataType Postgres a -> DataType Postgres b
coerceType = coerce

utctime :: Coercible UTCTime a => DataType Postgres a
utctime = DataType (timestampType Nothing True)

binaryFieldType :: Binary a => DataType Postgres a
binaryFieldType = DataType pgByteaType
