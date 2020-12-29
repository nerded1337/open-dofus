-- Account.hs ---
-- Copyright (C) 2020 Nerd ed
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module OpenDofus.Database.Auth.Account where

import Database.Beam
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import OpenDofus.Prelude

newtype AccountId = AccountId
  { unAccountId :: UUID
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountName = AccountName
  { unAccountName :: Text
  }
  deriving newtype
    ( Show,
      IsString,
      Ord,
      Eq,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountNickName = AccountNickName
  { unAccountNickName :: Text
  }
  deriving newtype
    ( Show,
      IsString,
      Ord,
      Eq,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountPassword = AccountPassword
  { unAccountPassword :: Text
  }
  deriving newtype
    ( Show,
      IsString,
      Ord,
      Eq,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountIsOnline = AccountIsOnline
  { unAccountIsOnline :: Bool
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountCreationDate = AccountCreationDate
  { unAccountCreationDate :: UTCTime
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountLastConnectionDate = AccountLastConnectionDate
  { unAccountLastConnectionDate :: UTCTime
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountLastConnectionIP = AccountLastConnectionIP
  { unAccountLastConnectionIP :: Text
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountIsBanned = AccountIsBanned
  { unAccountIsBanned :: Bool
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountTicketId = AccountTicketId
  { unAccountTicketId :: UUID
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountTicketCreationDate = AccountTicketCreationDate
  { unAccountTicketCreationDate :: UTCTime
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountIsAdmin = AccountIsAdmin
  { unAccountIsAdmin :: Bool
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountSubscriptionExpirationDate = AccountSubscriptionExpirationDate
  { unAccountSubscriptionExpirationDate :: UTCTime
  }
  deriving newtype
    ( Show,
      Eq,
      Ord,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlValueSyntax PgValueSyntax
    )

newtype AccountRemainingSubscriptionInMilliseconds = AccountRemainingSubscriptionInMilliseconds
  { unAccountRemainingSubscriptionInMilliseconds :: Word32
  }
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral)

data AccountT f = Account
  { _accountId :: !(C f AccountId),
    _accountName :: !(C f AccountName),
    _accountNickName :: !(C f AccountNickName),
    _accountPassword :: !(C f AccountPassword),
    _accountCreationDate :: !(C f AccountCreationDate),
    _accountLastConnectionDate :: !(C f AccountLastConnectionDate),
    _accountLastConnectionIP :: !(C f AccountLastConnectionIP),
    _accountIsOnline :: !(C f AccountIsOnline),
    _accountIsBanned :: !(C f AccountIsBanned),
    _accountIsAdmin :: !(C f AccountIsAdmin),
    _accountRemainingSubscription :: !(C f AccountSubscriptionExpirationDate)
  }
  deriving (Generic, Beamable)

instance Table AccountT where
  data PrimaryKey AccountT f = AccountPK !(C f AccountId)
    deriving (Generic, Beamable)
  primaryKey = AccountPK . _accountId
  {-# INLINE primaryKey #-}

type Account = AccountT Identity

deriving instance Eq Account

deriving instance Show Account

type AccountPK = PrimaryKey AccountT Identity

deriving instance Eq AccountPK

deriving instance Show AccountPK

Account
  (LensFor accountId)
  (LensFor accountName)
  (LensFor accountNickName)
  (LensFor accountPassword)
  (LensFor accountCreationDate)
  (LensFor accountLastConnectionDate)
  (LensFor accountLastConnectionIP)
  (LensFor accountIsOnline)
  (LensFor accountIsBanned)
  (LensFor accountIsAdmin)
  (LensFor accountSubscriptionExpirationDate) =
    tableLenses

data AccountTicketT f = AccountTicket
  { _accountTicketId :: !(C f AccountTicketId),
    _accountTicketAccountId :: !(PrimaryKey AccountT f),
    _accountTicketCreationDate :: !(C f AccountTicketCreationDate)
  }
  deriving (Generic, Beamable)

instance Table AccountTicketT where
  data PrimaryKey AccountTicketT f
    = AccountTicketPK
        !(C f AccountTicketId)
    deriving (Generic, Beamable)
  primaryKey = AccountTicketPK . _accountTicketId
  {-# INLINE primaryKey #-}

type AccountTicket = AccountTicketT Identity

deriving instance Eq AccountTicket

deriving instance Show AccountTicket

type AccountTicketPK = PrimaryKey AccountTicketT Identity

deriving instance Eq AccountTicketPK

deriving instance Show AccountTicketPK

AccountTicket
  (LensFor accountTicketId)
  (AccountPK (LensFor accountTicketAccountId))
  (LensFor accountTicketCreationDate) =
    tableLenses
