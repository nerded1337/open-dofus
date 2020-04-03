-- Database.hs ---
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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-missing-signatures     #-}

module OpenDofus.Database
  ( module X
  , Pool
  , populateGameDb
  , createGameDb
  , createAuthDb
  , createConnPool
  , runVolatile
  , runSerializable
  ) where

import           Data.Coerce
import           Data.Foldable
import           Data.Functor
import           Data.Pool
import           Database.Beam                          as X
import           Database.Beam.Migrate                  as X
import           Database.Beam.Postgres                 as X
import           Database.PostgreSQL.Simple.Transaction as X
import           OpenDofus.Database.Auth                as X
import           OpenDofus.Database.Game                as X
import           OpenDofus.Database.SWF.Breed
import           OpenDofus.Database.SWF.Effect
import           OpenDofus.Database.SWF.Item
import           OpenDofus.Database.SWF.Map
import           OpenDofus.Database.SWF.Spell
import           OpenDofus.Prelude                      hiding (map)

{-# INLINE populateGameDb #-}
populateGameDb ::
     forall a m. (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m)
  => FilePath
  -> m ()
populateGameDb path = do
  let path' = path <> "/lang/swf"
      runQ = runSerializable @GameDbConn @m . fromPg
  ss <- liftIO $ loadSpells $ path' <> "/spells_fr_350.swf"
  efs <- liftIO $ loadEffects $ path' <> "/effects_fr_266.swf"
  bds <- liftIO $ loadBreeds $ path' <> "/classes_fr_180.swf"
  (ist, is, it, its) <-
    liftIO $
    loadItems (path' <> "/items_fr_449.swf") (path' <> "/itemstats_fr_3.swf")
  (ms, msupas, mas, msubas) <-
    liftIO $ loadMaps (path <> "/maps") (path' <> "/maps_fr_366.swf")
  runQ $ runInsert $ insert (gameDb ^. effect) $ insertValues $ efs
  runQ $
    traverse_
      (\(s, lvls) -> do
         runInsert $ insert (gameDb ^. spell) $ insertValues [s]
         runInsert $ insert (gameDb ^. spellLevel) $ insertValues lvls)
      ss
  runQ $
    traverse_
      (\(b, bc, bs) -> do
         runInsert $ insert (gameDb ^. breed) $ insertValues [b]
         runInsert $ insert (gameDb ^. breedCost) $ insertValues $ toList bc
         runInsert $ insert (gameDb ^. breedSpell) $ insertValues $ toList bs)
      bds
  runQ $
    runInsert $ insert (gameDb ^. itemSuperType) $ insertValues $ toList ist
  runQ $ runInsert $ insert (gameDb ^. itemSlot) $ insertValues $ toList is
  runQ $ runInsert $ insert (gameDb ^. itemType) $ insertValues $ toList it
  runQ $ runInsert $ insert (gameDb ^. item) $ insertValues $ toList its
  runQ $
    runInsert $ insert (gameDb ^. mapSuperArea) $ insertValues $ toList msupas
  runQ $ runInsert $ insert (gameDb ^. mapArea) $ insertValues $ toList mas
  runQ $
    traverse_
      (\(msuba, msubaNeighs) -> do
         runInsert $
           insert (gameDb ^. mapSubAreaNeighbour) $
           insertValues $ toList msubaNeighs
         runInsert $ insert (gameDb ^. mapSubArea) $ insertValues $ [msuba])
      msubas
  runQ $ runInsert $ insert (gameDb ^. map) $ insertValues $ toList ms

{-# INLINE createDb #-}
createDb ::
     forall a b x m. (MonadIO m, HasConnectPool x b, MonadReader x m)
  => MigrationSteps Postgres () (CheckedDatabaseSettings Postgres a)
  -> m ()
createDb m = do
  pool <- asks (getConnectionPool @x @b)
  liftIO $ withResource pool (go . coerce)
  where
    go connection =
      liftIO $
      withTransactionSerializable connection $
      runBeamPostgres
        connection
        (void $
         runMigrationSteps 0 Nothing m (\_ _ -> executeMigration runNoReturn))

createConnPool ::
     (MonadIO m, Coercible a Connection, Coercible Connection a) => ConnectInfo -> m (Pool a)
createConnPool connInfo =
  liftIO $ createPool (coerce <$> connect connInfo) (close . coerce) 2 60 100

{-# INLINE createGameDb #-}
createGameDb ::
     forall x m. (MonadIO m, HasConnectPool x GameDbConn, MonadReader x m) => m ()
createGameDb = createDb @GameDb @GameDbConn gameDbMigrations

{-# INLINE createAuthDb #-}
createAuthDb ::
     (MonadIO m, HasConnectPool x AuthDbConn, MonadReader x m) => m ()
createAuthDb = createDb @AuthDb @AuthDbConn authDbMigrations

{-# INLINE runVolatile #-}
runVolatile ::
     forall b m x a. (MonadIO m, HasConnectPool x b, MonadReader x m)
  => QueryTypeOf b a
  -> m a
runVolatile query = do
  pool <- asks (getConnectionPool @x @b)
  liftIO $ withResource pool (go . coerce)
  where
    go connection = runQuery connection query

{-# INLINE runSerializable #-}
runSerializable ::
     forall b m x a. (MonadIO m, HasConnectPool x b, MonadReader x m)
  => QueryTypeOf b a
  -> m a
runSerializable query = do
  pool <- asks (getConnectionPool @x @b)
  liftIO $ withResource pool (go . coerce)
  where
    go connection =
      withTransactionSerializable connection $ runQuery connection query

{-# INLINE runQuery #-}
runQuery :: (MonadIO m, IsPg q) => Connection -> q a -> m a
runQuery connection = liftIO . runBeamPostgres connection . toPg
