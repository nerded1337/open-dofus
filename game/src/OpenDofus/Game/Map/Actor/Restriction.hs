-- Restriction.hs ---

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

{-# LANGUAGE TypeFamilies #-}

module OpenDofus.Game.Map.Actor.Restriction
  ( ActorRestriction(..)
  , ActorRestrictionSet
  , defaultRestrictions
  , addRestriction
  , removeRestriction
  , hasRestriction
  , decodeRestrictions
  , encodeRestrictions
  )
where

import           Data.Enum.Set                 as E
import           OpenDofus.Prelude

type ActorRestrictionSet = EnumSet ActorRestriction

data ActorRestriction = ActorRestrictionCantAssault
    | ActorRestrictionCantChallenge
    | ActorRestrictionCantExchange
    | ActorRestrictionCanAttack
    | ActorRestrictionCantChatToAll
    | ActorRestrictionCantBeMerchant
    | ActorRestrictionCantUseObject
    | ActorRestrictionCantInteractWithTaxCollector
    | ActorRestrictionCantUseInteractiveObject
    | ActorRestrictionCantSpeakToNpc
    | ActorRestrictionUnknow1024
    | ActorRestrictionUnknow2048
    | ActorRestrictionCanAttackDungeonMonstersWhenMutant
    | ActorRestrictionCanMoveInAllDirections
    | ActorRestrictionCanAttackMonstersAnywhereWhenMutant
    | ActorRestrictionCantInteractWithPrism
    deriving (Bounded, Enum, Eq, Ord, Show)

instance AsEnumSet ActorRestriction where
  type EnumSetRep ActorRestriction = Word16

{-# INLINE defaultRestrictions #-}
defaultRestrictions :: ActorRestrictionSet
defaultRestrictions = E.singleton ActorRestrictionCanMoveInAllDirections

{-# INLINE addRestriction #-}
addRestriction :: ActorRestriction -> ActorRestrictionSet -> ActorRestrictionSet
addRestriction = E.insert

{-# INLINE removeRestriction #-}
removeRestriction
  :: ActorRestriction -> ActorRestrictionSet -> ActorRestrictionSet
removeRestriction = E.delete

{-# INLINE hasRestriction #-}
hasRestriction :: ActorRestriction -> ActorRestrictionSet -> Bool
hasRestriction = E.member

{-# INLINE decodeRestrictions #-}
decodeRestrictions :: EnumSetRep ActorRestriction -> ActorRestrictionSet
decodeRestrictions = fromRaw

{-# INLINE encodeRestrictions #-}
encodeRestrictions :: ActorRestrictionSet -> EnumSetRep ActorRestriction
encodeRestrictions = toRaw
