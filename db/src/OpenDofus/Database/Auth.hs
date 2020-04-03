-- Auth.hs ---

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

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-missing-signatures     #-}

module OpenDofus.Database.Auth
  ( module X
  , AuthDb(..)
  , AuthQuery(..)
  , AuthDbConn(..)
  , account
  , accountTicket
  , worldServer
  , authDb
  , authDbMigrations
  , getWorldServers
  , getAccountByName
  , setAccountIsOnline
  , generateAccountTicket
  , getAccountRemainingSubscriptionInMilliseconds
  , getAccountTicket
  ) where

import           Data.Functor
import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Postgres
import           OpenDofus.Database.Auth.Account     as X
import           OpenDofus.Database.Auth.WorldServer as X
import           OpenDofus.Database.Types            as X
import           OpenDofus.Prelude

newtype AuthQuery a =
  AuthQuery
    { unAuthQuery :: Pg a
    }
  deriving newtype (Functor, Applicative, Monad, IsPg)

newtype AuthDbConn =
  AuthDbConn
    { unAuthDbConn :: Connection
    }

instance HasQueryType AuthDbConn where
  type QueryTypeOf AuthDbConn = AuthQuery

data AuthDb f = AuthDb
    { _dbAccount       :: !(f (TableEntity AccountT))
    , _dbAccountTicket :: !(f (TableEntity AccountTicketT))
    , _dbWorldServer   :: !(f (TableEntity WorldServerT))
    }
    deriving stock (Generic)
    deriving anyclass (Database Postgres)

AuthDb
  (TableLens account)
  (TableLens accountTicket)
  (TableLens worldServer)
  = dbLenses

authDb :: DatabaseSettings Postgres AuthDb
authDb = unCheckDatabase $ evaluateDatabase authDbMigrations

authDbMigrations ::
     MigrationSteps Postgres () (CheckedDatabaseSettings Postgres AuthDb)
authDbMigrations = migrationStep "Initial commit" initialMigration

initialMigration :: () -> Migration Postgres (CheckedDatabaseSettings Postgres AuthDb)
initialMigration _ =
  AuthDb <$>
  createTable
    "account"
    (Account
       (field "id" (coerceType uuid) notNull unique)
       (field "name" (coerceType (varchar (Just 20))) notNull unique)
       (field "nickname" (coerceType (varchar (Just 20))) notNull unique)
       (field "password" (coerceType (varchar (Just 20))) notNull)
       (field "creation_date" utctime notNull)
       (field "last_connection_date" utctime notNull)
       (field "last_connection_ip" (coerceType $ varchar (Just 45)) notNull)
       (field "is_online" (coerceType boolean) notNull)
       (field "is_banned" (coerceType boolean) notNull)
       (field "is_admin" (coerceType boolean) notNull)
       (field "subscription_expiration_date" utctime notNull)) <*>
  createTable
    "account_ticket"
    (AccountTicket
       (field "id" (coerceType uuid) notNull unique)
       (AccountPK $ field "account_id" (coerceType uuid) notNull)
       (field "creation_date" utctime notNull)) <*>
  createTable
    "world_server"
    (WorldServer
       (field "id" int notNull unique)
       (field "completion" enumType notNull)
       (field "status" enumType notNull)
       (field "ip" (coerceType (varchar (Just 45))) notNull)
       (field "port" int notNull))

getWorldServers :: AuthQuery [WorldServer]
getWorldServers =
  AuthQuery $ runSelectReturningList $ select $ all_ (authDb ^. worldServer)

getAccountByName :: AccountName -> AuthQuery (Maybe (AccountT Identity))
getAccountByName accName =
  AuthQuery $
  runSelectReturningOne $
  select $ do
    acc <- all_ (authDb ^. account)
    guard_ (acc ^. accountName ==. val_ accName)
    pure acc

setAccountIsOnline :: AccountId -> AccountIsOnline -> AuthQuery ()
setAccountIsOnline accId isOnline =
  AuthQuery $ runUpdate $
  update
    (authDb ^. account)
    (\acc -> (acc ^. accountIsOnline) <-. val_ isOnline)
    (\acc -> (acc ^. accountId) ==. val_ accId)

generateAccountTicket :: AccountId -> AuthQuery AccountTicket
generateAccountTicket accId = AuthQuery $ do
  ticketId <- liftIO $ AccountTicketId <$> nextRandom
  currentTime <- liftIO $ AccountTicketCreationDate <$> getCurrentTime
  let ticket = (AccountTicket ticketId (AccountPK accId) currentTime)
  runInsert $
    insert (authDb ^. accountTicket) $
    insertValues [ticket]
  pure ticket

getAccountRemainingSubscriptionInMilliseconds ::
     MonadIO m
  => AccountSubscriptionExpirationDate
  -> m AccountRemainingSubscriptionInMilliseconds
getAccountRemainingSubscriptionInMilliseconds expiration = do
  now <- getCurrentTime
  let remainingSeconds =
        diffUTCTime (unAccountSubscriptionExpirationDate $ expiration) now
  pure $
    AccountRemainingSubscriptionInMilliseconds $ round $ remainingSeconds * 1000

getAccountTicket :: AccountTicketId -> AuthQuery (Maybe AccountTicket)
getAccountTicket i =
  AuthQuery $
  runSelectReturningOne $
  select $ do
    tick <- all_ (authDb ^. accountTicket)
    guard_ (tick ^. accountTicketId ==. val_ i)
    pure tick
