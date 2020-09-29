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

{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module OpenDofus.Game.Map.Actor.PlayerCharacter
  ( PlayerCharacter(..)
  , HasPlayerCharacter(..)
  )
where

import           OpenDofus.Database
import           OpenDofus.Prelude

data PlayerCharacter a = PlayerCharacter
    { _playerCharacterBaseCharacter :: {-# UNPACK #-} !Character
    , _playerCharacterPosition      :: {-# UNPACK #-} !CharacterPosition
    , _playerCharacterCharacterLook :: {-# UNPACK #-} !CharacterLook
    , _playerCharacterController    :: !(TVar (Maybe a))
    }
    deriving stock (Generic)

makeClassy ''PlayerCharacter