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
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

module OpenDofus.Game.Map.Actor
  ( module X
  , GameActor(..)
  , loadPlayerCharacter
  )
where

import           OpenDofus.Database
import           OpenDofus.Game.Map.Actor.PlayerCharacter
                                               as X
import           OpenDofus.Game.Map.Actor.Types
                                               as X
import           OpenDofus.Prelude

data GameActor a =
  GameActorPC (PlayerCharacter a)
  deriving (Show)

instance HasActorId (GameActor a) where
  actorId (GameActorPC pc) = pc ^. to actorId

instance HasPosition (GameActor a) where
  position (GameActorPC pc) = pc ^. to position

instance HasDirection (GameActor a) where
  direction (GameActorPC pc) = pc ^. to direction

instance HasController (GameActor a) a where
  controller (GameActorPC pc) = pc ^. to controller

loadPlayerCharacter
  :: (MonadIO m, HasConnectPool a GameDbConn, MonadReader a m)
  => controller
  -> CharacterId
  -> m (Maybe (PlayerCharacter controller))
loadPlayerCharacter gc cid = do
  r <- runSerializable @GameDbConn query
  pure $ build <$> r <*> pure SouthEast <*> pure defaultRestrictions <*> pure gc
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
