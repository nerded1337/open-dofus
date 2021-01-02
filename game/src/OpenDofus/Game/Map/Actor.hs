{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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
    Actor (..),
    HasActor (..),
    ActorSpecialization (..),
    HasActorSpecialization (..),
    ActorState (..),
    HasActorState (..),
    ActorAction (..),
    HasActorAction (..),
    isPlayerActor,
    loadPlayerCharacter,
  )
where

import OpenDofus.Database
import OpenDofus.Game.Map.Actor.PlayerCharacter as X
import OpenDofus.Game.Map.Actor.Restriction as X
import OpenDofus.Game.Map.Actor.Types as X
import OpenDofus.Game.Map.Direction as X
import OpenDofus.Game.Map.Movement as X
import OpenDofus.Game.Time
import OpenDofus.Prelude

data ActorAction
  = ActorActionMoving
      {-# UNPACK #-} !(MovementPath CellId)
      {-# UNPACK #-} !(GameTime Millisecond)
  deriving stock (Show, Eq)

makeClassy ''ActorAction

data ActorState
  = ActorIdle
  | ActorDoing !ActorAction
  deriving stock (Show, Eq)

makeClassy ''ActorState

newtype ActorSpecialization
  = ActorSpecializationPC PlayerCharacter
  deriving newtype (Show, Eq)

makeClassy ''ActorSpecialization

instance HasActorId ActorSpecialization where
  actorId (ActorSpecializationPC pc) = pc ^. to actorId
  {-# INLINE actorId #-}

data Actor = Actor
  { _gameActorState :: !ActorState,
    _gameActorLocation :: {-# UNPACK #-} !ActorLocation,
    _gameActorDirection :: !Direction,
    _gameActorSpecialization :: !ActorSpecialization
  }
  deriving stock (Eq)

makeClassy ''Actor

instance Show Actor where
  show = show . view gameActorSpecialization

instance HasActorId Actor where
  actorId = actorId . view gameActorSpecialization
  {-# INLINE actorId #-}

instance HasActorLocation Actor where
  actorLocation = gameActorLocation
  {-# INLINE actorLocation #-}

instance HasDirection Actor where
  direction = gameActorDirection
  {-# INLINE direction #-}

isPlayerActor :: Actor -> Bool
isPlayerActor a =
  case a ^. gameActorSpecialization of
    ActorSpecializationPC _ -> True
    _ -> False
{-# INLINE isPlayerActor #-}

loadPlayerCharacter ::
  (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m) =>
  CharacterId ->
  m (Maybe PlayerCharacter)
loadPlayerCharacter cid =
  getCompose $ fmap build $ Compose $ runVolatile @GameDbConn query
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
          cl <-
            oneToOne_
              (gameDb ^. characterLook)
              _characterLookCharacterId
              c
          guard_ (c ^. characterId ==. val_ cid)
          pure (c, cl, cp)
    build (c, cl, cp) =
      PlayerCharacter c cl cp defaultRestrictions
