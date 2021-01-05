{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- Database.hs ---

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

module OpenDofus.Database
  ( module X,
    Pool,
    populateGameDb,
    bringGameDbUpToDate,
    bringAuthDbUpToDate,
    checkDbSchemas,
    createConnPool,
    runVolatile,
    runSerializable,
    getWorldServers,
    getAccountByName,
    getAccountById,
    setAccountIsOnline,
    generateAccountTicket,
    getAccountRemainingSubscriptionInMilliseconds,
    getAccountByTicket,
    getBreedById,
    getCharacterByName,
    createNewCharacter,
    getMaps,
    getMapById,
    getInteractiveObjectGfxIds,
    getInteractiveObjectByGfxId,
    isAccountCharacter,
    getCharacterPosition,
  )
where

import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.Pool
import qualified Data.HashMap.Strict as HM
import Database.Beam as X
import Database.Beam.Migrate as X
import Database.Beam.Migrate.Simple as X
import Database.Beam.Postgres as X
import Database.Beam.Postgres.Migrate as PGM
import Database.PostgreSQL.Simple.Transaction as X
import OpenDofus.Database.Auth as X
import OpenDofus.Database.Game as X
import OpenDofus.Database.SWF.Breed
import OpenDofus.Database.SWF.Effect
import OpenDofus.Database.SWF.InteractiveObject
import OpenDofus.Database.SWF.Item
import OpenDofus.Database.SWF.Job
import OpenDofus.Database.SWF.Map
import OpenDofus.Database.SWF.Skill
import OpenDofus.Database.SWF.Spell
import OpenDofus.Prelude

populateGameDb ::
  forall a m.
  (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m) =>
  FilePath ->
  m ()
populateGameDb path = do
  let path' = path <> "/lang/swf"
      version = "1015"
      mkPath x = path' <> x <> "_" <> version <> ".swf"
      runQ = runSerializable @GameDbConn . fromPg
  ss <- liftIO $ loadSpells $ mkPath "/spells_fr"
  efs <- liftIO $ loadEffects $ mkPath "/effects_fr"
  bds <- liftIO $ loadBreeds $ mkPath "/classes_fr"
  js <- liftIO $ loadJobs $ mkPath "/jobs_fr"
  sks <- liftIO $ loadSkills $ mkPath "/skills_fr"
  (ios, iogfxs) <-
    liftIO $
      loadInteractiveObjects $
        mkPath
          "/interactiveobjects_fr"
  (ist, is, it, its) <-
    liftIO $
      loadItems (mkPath "/items_fr") (mkPath "/itemstats_fr")
  (ms, msupas, mas, msubas) <-
    liftIO $
      loadMaps (path <> "/maps") (mkPath "/maps_fr")
  runQ $
    traverse_
      ( \(sk, skc) -> do
          runInsert $ insert (gameDb ^. skill) $ insertValues [sk]
          runInsert $ insert (gameDb ^. skillCraft) $ insertValues $ toList skc
      )
      sks
  runQ $ runInsert $ insert (gameDb ^. job) $ insertValues js
  runQ $ runInsert $ insert (gameDb ^. interactiveObject) $ insertValues ios
  runQ $
    runInsert $
      insert (gameDb ^. interactiveObjectGfx) $
        insertValues
          iogfxs
  runQ $ runInsert $ insert (gameDb ^. effect) $ insertValues efs
  runQ $
    traverse_
      ( \(s, lvls) -> do
          runInsert $ insert (gameDb ^. spell) $ insertValues [s]
          runInsert $ insert (gameDb ^. spellLevel) $ insertValues lvls
      )
      ss
  runQ $
    traverse_
      ( \(b, bc, bs) -> do
          runInsert $ insert (gameDb ^. breed) $ insertValues [b]
          runInsert $ insert (gameDb ^. breedCharacteristicCost) $ insertValues $ toList bc
          runInsert $ insert (gameDb ^. breedSpell) $ insertValues $ toList bs
      )
      bds
  runQ $
    runInsert $
      insert (gameDb ^. itemSuperType) $
        insertValues $
          toList
            ist
  runQ $ runInsert $ insert (gameDb ^. itemSlot) $ insertValues $ toList is
  runQ $ runInsert $ insert (gameDb ^. itemType) $ insertValues $ toList it
  runQ $ runInsert $ insert (gameDb ^. item) $ insertValues $ toList its
  runQ $
    runInsert $
      insert (gameDb ^. mapSuperArea) $
        insertValues $
          toList
            msupas
  runQ $ runInsert $ insert (gameDb ^. mapArea) $ insertValues $ toList mas
  runQ $
    traverse_
      ( \(msuba, msubaNeighs) -> do
          runInsert $
            insert (gameDb ^. mapSubAreaNeighbour) $
              insertValues $
                toList
                  msubaNeighs
          runInsert $ insert (gameDb ^. mapSubArea) $ insertValues [msuba]
      )
      msubas
  runQ $ runInsert $ insert (gameDb ^. map) $ insertValues $ toList ms

checkDbSchemas ::
  forall db env m f.
  ( Database Postgres db,
    HasConnType (db f),
    HasConnectPool env (ConnTypeOf (db f)),
    MonadReader env m,
    MonadIO m
  ) =>
  CheckedDatabaseSettings Postgres db ->
  m VerificationResult
checkDbSchemas db = do
  pool <- view $ to $ getConnectionPool @env @(ConnTypeOf (db f))
  liftIO $ withResource pool (go . coerce)
  where
    go connection =
      liftIO $
        withTransactionSerializable connection $
          runBeamPostgres
            connection
            $ verifySchema PGM.migrationBackend db

bringDbUpToDate ::
  forall db env m f.
  ( Database Postgres db,
    HasConnType (db f),
    HasConnectPool env (ConnTypeOf (db f)),
    MonadReader env m,
    MonadIO m
  ) =>
  MigrationSteps Postgres () (CheckedDatabaseSettings Postgres db) ->
  m (Maybe (CheckedDatabaseSettings Postgres db))
bringDbUpToDate m = do
  pool <- view $ to $ getConnectionPool @env @(ConnTypeOf (db f))
  liftIO $ withResource pool (go . coerce)
  where
    allowDestructive =
      defaultUpToDateHooks {runIrreversibleHook = pure True}
    go connection =
      liftIO $
        withTransactionSerializable connection $
          runBeamPostgres
            connection
            $ bringUpToDateWithHooks allowDestructive PGM.migrationBackend m

createConnPool ::
  ( Coercible a Connection,
    Coercible Connection a,
    MonadIO m
  ) =>
  Int ->
  ConnectInfo ->
  m (Pool a)
createConnPool maxConn connInfo =
  liftIO $
    createPool
      (coerce <$> connect connInfo)
      (close . coerce)
      stripes
      aliveSeconds
      maximumConnPerStripe
  where
    stripes = 4
    aliveSeconds = 4 * 60
    maximumConnPerStripe = maxConn `quot` stripes

bringGameDbUpToDate ::
  forall env m.
  ( HasConnectPool env GameDbConn,
    MonadReader env m,
    MonadIO m
  ) =>
  m (Maybe (CheckedDatabaseSettings Postgres GameDb))
bringGameDbUpToDate =
  bringDbUpToDate gameDbMigrations

bringAuthDbUpToDate ::
  ( HasConnectPool env AuthDbConn,
    MonadReader env m,
    MonadIO m
  ) =>
  m (Maybe (CheckedDatabaseSettings Postgres AuthDb))
bringAuthDbUpToDate =
  bringDbUpToDate authDbMigrations

runVolatile ::
  forall conn env m a.
  ( HasConnectPool env conn,
    MonadReader env m,
    MonadIO m
  ) =>
  QueryTypeOf conn a ->
  m a
runVolatile query = do
  pool <- view $ to $ getConnectionPool @env @conn
  liftIO $ withResource pool (go . coerce)
  where
    go connection =
      withTransactionLevel
        RepeatableRead
        connection
        (runQuery connection query)
{-# INLINE runVolatile #-}

runSerializable ::
  forall conn env m a.
  ( HasConnectPool env conn,
    MonadReader env m,
    MonadIO m
  ) =>
  QueryTypeOf conn a ->
  m a
runSerializable query = do
  pool <- view $ to $ getConnectionPool @env @conn
  liftIO $ withResource pool (go . coerce)
  where
    go connection =
      withTransactionSerializable connection $ runQuery connection query
{-# INLINE runSerializable #-}

runQuery :: (MonadIO m, IsPg q) => Connection -> q a -> m a
runQuery connection =
  liftIO
    . runBeamPostgres connection
    . toPg
{-# INLINE runQuery #-}

getWorldServers :: AuthQuery [WorldServer]
getWorldServers = AuthQuery query
  where
    query =
      runSelectReturningList $
        select $
          all_ $
            authDb ^. worldServer

getAccountByName :: AccountName -> AuthQuery (Maybe Account)
getAccountByName accName = AuthQuery query
  where
    query = runSelectReturningOne $
      select $ do
        acc <- all_ $ authDb ^. account
        guard_ $ acc ^. accountName ==. val_ accName
        pure acc

getAccountById :: AccountId -> AuthQuery (Maybe Account)
getAccountById accId = AuthQuery query
  where
    query =
      runSelectReturningOne $
        lookup_
          (authDb ^. account)
          (AccountPK accId)

setAccountIsOnline :: AccountId -> AccountIsOnline -> AuthQuery ()
setAccountIsOnline accId isOnline = AuthQuery query
  where
    query =
      runUpdate $
        update
          (authDb ^. account)
          (\acc -> (acc ^. accountIsOnline) <-. val_ isOnline)
          (\acc -> (acc ^. accountId) ==. val_ accId)

generateAccountTicket :: AccountId -> AuthQuery AccountTicket
generateAccountTicket accId = AuthQuery query
  where
    query = do
      ticketId <- liftIO $ AccountTicketId <$> nextRandom
      currentTime <- liftIO $ AccountTicketCreationDate <$> getCurrentTime
      let ticket = AccountTicket ticketId (AccountPK accId) currentTime
      runInsert $
        insert
          (authDb ^. accountTicket)
          (insertValues [ticket])
      pure ticket

getAccountRemainingSubscriptionInMilliseconds ::
  MonadIO m =>
  AccountSubscriptionExpirationDate ->
  m AccountRemainingSubscriptionInMilliseconds
getAccountRemainingSubscriptionInMilliseconds expiration = do
  now <- getCurrentTime
  let remainingSeconds =
        diffUTCTime (unAccountSubscriptionExpirationDate expiration) now
  pure $
    AccountRemainingSubscriptionInMilliseconds $
      round $
        remainingSeconds * 1000

getAccountByTicket ::
  AccountTicketId -> AuthQuery (Maybe (AccountTicket, Account))
getAccountByTicket i = AuthQuery query
  where
    query = runSelectReturningOne $
      select $ do
        tick <- all_ $ authDb ^. accountTicket
        acc <-
          join_
            (authDb ^. account)
            (\acc -> _accountTicketAccountId tick ==. primaryKey acc)
        guard_ $ tick ^. accountTicketId ==. val_ i
        pure (tick, acc)

getBreedById :: BreedId -> GameQuery (Maybe Breed)
getBreedById bid = GameQuery query
  where
    query =
      runSelectReturningOne $
        lookup_
          (gameDb ^. breed)
          (BreedPK bid)

getCharacterByName :: CharacterName -> GameQuery (Maybe Character)
getCharacterByName cn = GameQuery query
  where
    query = runSelectReturningOne $
      select $ do
        c <- all_ $ gameDb ^. character
        guard_ $ c ^. characterName ==. val_ cn
        pure c

getCharacterPosition :: CharacterId -> GameQuery (Maybe CharacterPosition)
getCharacterPosition cid = GameQuery query
  where
    query = runSelectReturningOne $
      select $ do
        c <- all_ $ gameDb ^. characterPosition
        guard_ $ c ^. characterPositionCharacterId ==. val_ cid
        pure c

isAccountCharacter :: AccountId -> CharacterId -> GameQuery Bool
isAccountCharacter aid cid = GameQuery query
  where
    query = fmap isJust $
      runSelectReturningOne $
        select $ do
          c <- all_ $ gameDb ^. character
          guard_ $
            c ^. characterId ==. val_ cid
              &&. c ^. characterAccountId ==. val_ aid
          pure c

createNewCharacter ::
  AccountId ->
  CharacterId ->
  CharacterName ->
  Breed ->
  CharacterPosition ->
  CharacterLook ->
  GameQuery (Character, CharacterPosition, CharacterLook)
createNewCharacter ai cid cn b cp cl = GameQuery query
  where
    query = do
      let newCharacter =
            Character
              cid
              cn
              (BreedPK $ b ^. breedId)
              (AccountPK ai)
              1
              0
              0
      runInsert $
        insert (gameDb ^. character) $
          insertValues [newCharacter]
      runInsert $
        insert (gameDb ^. characterLook) $
          insertValues
            [cl]
      runInsert $
        insert (gameDb ^. characterPosition) $
          insertValues
            [cp]
      pure (newCharacter, cp, cl)

getMaps :: GameQuery [Map]
getMaps = GameQuery query
  where
    query = runSelectReturningList $
      select $
        fmap fst $
          orderBy_ (asc_ . view subAreaId . snd) $ do
            m <- all_ $ gameDb ^. map
            sa <- related_ (gameDb ^. mapSubArea) (_mapSubArea m)
            pure (m, sa)

getMapById :: MapId -> GameQuery (Maybe Map)
getMapById mid = GameQuery query
  where
    query = runSelectReturningOne $ lookup_ (gameDb ^. map) (MapPK mid)

getInteractiveObjectGfxIds :: GameQuery (HM.HashMap InteractiveObjectGfxId InteractiveObject)
getInteractiveObjectGfxIds = GameQuery query
  where
    query = fmap HM.fromList $
      runSelectReturningList $
      select $ do
        ioGfx <- all_ $ gameDb ^. interactiveObjectGfx
        io <- related_ (gameDb ^. interactiveObject) (_interactiveObjectGfxInteractiveObjectId ioGfx)
        pure (ioGfx ^. interactiveObjectGfxId, io)

getInteractiveObjectByGfxId ::
  InteractiveObjectGfxId -> GameQuery (Maybe InteractiveObject)
getInteractiveObjectByGfxId iogfx = GameQuery query
  where
    query = runSelectReturningOne $
      select $ do
        io <- all_ $ gameDb ^. interactiveObject
        gfx <-
          oneToMany_
            (gameDb ^. interactiveObjectGfx)
            _interactiveObjectGfxInteractiveObjectId
            io
        guard_ (gfx ^. interactiveObjectGfxId ==. val_ iogfx)
        pure io
