{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- PlayerCharacter.hs ---

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

module OpenDofus.Game.Map.Actor.PlayerCharacter
  ( PlayerCharacter (..),
    HasPlayerCharacter (..),
  )
where

import OpenDofus.Database
import OpenDofus.Game.Map.Actor.Restriction
import OpenDofus.Game.Map.Actor.Types
import OpenDofus.Prelude

data PlayerCharacter = PlayerCharacter
  { _playerCharacterBaseCharacter :: {-# UNPACK #-} !Character,
    _playerCharacterCharacterLocation :: {-# UNPACK #-} !ActorLocation,
    _playerCharacterCharacterLook :: {-# UNPACK #-} !CharacterLook,
    _playerCharacterCharacterDirection :: !ActorDirection,
    _playerCharacterRestrictions :: {-# UNPACK #-} !ActorRestrictionSet
  }
  deriving stock (Show, Eq)

makeClassy ''PlayerCharacter

instance HasActorId PlayerCharacter where
  actorId =
    ActorId
      . unCharacterId
      . view (playerCharacterBaseCharacter . characterId)

instance HasActorLocation PlayerCharacter where
  actorLocation = playerCharacterCharacterLocation

instance HasActorDirection PlayerCharacter where
  actorDirection = playerCharacterCharacterDirection
