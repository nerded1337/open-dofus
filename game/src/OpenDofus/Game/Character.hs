-- Character.hs ---

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

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Game.Character
  ( CharacterSelectionInfo(..)
  , getCharacterList
  ) where

import           Data.ByteString.Lazy.Builder  as BS
import           OpenDofus.Core.Network.Client
import           OpenDofus.Database
import           OpenDofus.Prelude

data CharacterSelectionInfo = CharacterSelectionInfo
    { _characterSelectionInfoId          :: CharacterId
    , _characterSelectionInfoName        :: CharacterName
    , _characterSelectionInfoLevel       :: CharacterLevel
    , _characterSelectionInfoGfxId       :: GfxId
    , _characterSelectionInfoFirstColor  :: CharacterColor
    , _characterSelectionInfoSecondColor :: CharacterColor
    , _characterSelectionInfoThirdColor  :: CharacterColor
    , _characterSelectionMaxLevel        :: CharacterMaxLevel
    , _characterWorldId                  :: WorldId
    }

instance ToNetwork CharacterSelectionInfo where
  {-# INLINE toNetwork #-}
  toNetwork (CharacterSelectionInfo i n l gi c1 c2 c3 ml wid) =
    "|" <>
    word64Dec (unCharacterId i) <>
    ";" <>
    lazyByteString (encodeText $ unCharacterName n) <>
    ";" <>
    word32Dec (unCharacterLevel l) <>
    ";" <>
    word32Dec (unGfxId gi) <>
    ";" <>
    intDec (unCharacterColor c1) <>
    ";" <>
    intDec (unCharacterColor c2) <>
    ";" <>
    intDec (unCharacterColor c3) <>
    ";" <>
    ",,,,," <> -- TODO: equipment
    ";" <>
    "0" <> -- TODO: is merchant
    ";" <>
    intDec (unWorldId wid) <> -- TODO: game id
    ";" <>
    "0" <> -- TODO: is dead
    ";" <>
    "0" <> -- TODO: death count
    ";" <> word32Dec (unCharacterMaxLevel ml)

getCharacterList :: WorldId -> AccountId -> GameQuery [CharacterSelectionInfo]
getCharacterList wid accId =
  fmap go <$>
  GameQuery
    (runSelectReturningList $
     select $ do
       c <- all_ (gameDb ^. character)
       cl <- all_ (gameDb ^. characterLook)
       guard_
         (c ^. characterAccountId ==. val_ accId &&. cl ^.
          characterLookCharacterId ==.
          (c ^. characterId))
       pure
         ( c ^. characterId
         , c ^. characterName
         , c ^. characterLevel
         , cl ^. characterLookGfxId
         , cl ^. characterLookFirstColor
         , cl ^. characterLookSecondColor
         , cl ^. characterLookThirdColor
         , c ^. characterMaxLevel))
  where
    go (i, n, l, gi, c1, c2, c3, ml) =
      CharacterSelectionInfo i n l gi c1 c2 c3 ml wid
