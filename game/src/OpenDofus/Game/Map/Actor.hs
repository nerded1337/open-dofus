{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- Actor.hs ---

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

module OpenDofus.Game.Map.Actor
  ( module X,
    GameActor (..),
    loadPlayerCharacter,
  )
where

import OpenDofus.Database
import OpenDofus.Game.Map.Actor.PlayerCharacter as X
import OpenDofus.Game.Map.Actor.Restriction as X
import OpenDofus.Game.Map.Actor.Types as X
import OpenDofus.Prelude

newtype GameActor
  = GameActorPC PlayerCharacter
  deriving newtype (Show)

instance HasActorId GameActor where
  actorId (GameActorPC pc) = pc ^. to actorId

instance HasActorLocation GameActor where
  actorLocation f (GameActorPC pc) =
    GameActorPC <$> actorLocation f pc

instance HasActorDirection GameActor where
  actorDirection f (GameActorPC pc) =
    GameActorPC <$> actorDirection f pc

loadPlayerCharacter ::
  (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m) =>
  CharacterId ->
  m (Maybe PlayerCharacter)
loadPlayerCharacter cid = do
  r <- runSerializable @GameDbConn query
  pure $ build <$> r <*> pure SouthEast <*> pure defaultRestrictions
  where
    query = GameQuery $
      runSelectReturningOne $
        select $ do
          c <- all_ (gameDb ^. character)
          cp <-
            oneToOne_
              (gameDb ^. characterPosition)
              _characterPositionCharacterId
              c
          cl <- oneToOne_ (gameDb ^. characterLook) _characterLookCharacterId c
          guard_ (c ^. characterId ==. val_ cid)
          pure (c, cp, cl)
    build (c, cp, cl) =
      PlayerCharacter
        c
        ( ActorLocation
            (cp ^. characterPositionMapId)
            (cp ^. characterPositionCellId)
        )
        cl
