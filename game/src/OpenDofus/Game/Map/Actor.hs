-- Object.hs ---

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

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module OpenDofus.Game.Map.Actor
  ( module X
  , ActorId(..)
  , loadPlayerCharacter
  )
where

import           OpenDofus.Database
import           OpenDofus.Game.Map.Actor.Types
                                               as X
import           OpenDofus.Prelude

newtype ActorId =
  ActorId
    { unActorId :: Word64
    }
  deriving newtype ( Show
                   , Eq
                   , Ord
                   , Num
                   , Real
                   , Enum
                   , Integral
                   , Hashable
                   )

loadPlayerCharacter
  :: forall a b m
   . (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m)
  => Maybe b
  -> CharacterId
  -> m (Maybe (PlayerCharacter b))
loadPlayerCharacter gc cid = do
  r     <- runSerializable @GameDbConn query
  vargc <- liftIO $ newTVarIO gc
  pure $ build <$> r <*> pure vargc
 where
  query = GameQuery $ runSelectReturningOne $ select $ do
    c  <- all_ (gameDb ^. character)
    cp <- oneToOne_ (gameDb ^. characterPosition)
                    _characterPositionCharacterId
                    c
    cl <- oneToOne_ (gameDb ^. characterLook) _characterLookCharacterId c
    guard_ (c ^. characterId ==. val_ cid)
    pure (c, cp, cl)
  build (c, cp, cl) = PlayerCharacter c cp cl


