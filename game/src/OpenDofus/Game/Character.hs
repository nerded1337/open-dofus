{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module OpenDofus.Game.Character
  ( CharacterListInfo (..),
    getCharacterList,
  )
where

import Data.ByteString.Lazy.Builder as BS
import OpenDofus.Core.Network.Client
import OpenDofus.Database
import OpenDofus.Prelude

data CharacterListInfo = CharacterListInfo
  { _characterListInfoCharacterId :: {-# UNPACK #-} !CharacterId,
    _characterListInfoCharacterName :: {-# UNPACK #-} !CharacterName,
    _characterListInfoCharacterLevel :: {-# UNPACK #-} !CharacterLevel,
    _characterListInfoCharacterGfxId :: {-# UNPACK #-} !GfxId,
    _characterListInfoCharacterFirstColor :: {-# UNPACK #-} !CharacterColor,
    _characterListInfoCharacterSecondColor :: {-# UNPACK #-} !CharacterColor,
    _characterListInfoCharacterThirdColor :: {-# UNPACK #-} !CharacterColor,
    _characterListInfoCharacterMaxLevel :: {-# UNPACK #-} !CharacterMaxLevel,
    _characterListInfoCharacterWorldId :: {-# UNPACK #-} !WorldId
  }

makeClassy ''CharacterListInfo

instance ToNetwork CharacterListInfo where
  toNetwork (CharacterListInfo i n l gi c1 c2 c3 ml wid) =
    "|"
      <> word64Dec (unCharacterId i)
      <> ";"
      <> byteString (encodeTextStrict $ unCharacterName n)
      <> ";"
      <> word32Dec (unCharacterLevel l)
      <> ";"
      <> word32Dec (unGfxId gi)
      <> ";"
      <> int32Dec (unCharacterColor c1)
      <> ";"
      <> int32Dec (unCharacterColor c2)
      <> ";"
      <> int32Dec (unCharacterColor c3)
      <> ";"
      <> ",,,,,"
      <> ";" -- TODO: equipment
      <> "0"
      <> ";" -- TODO: is merchant
      <> word32Dec (unWorldId wid)
      <> ";" -- TODO: game id
      <> "0"
      <> ";" -- TODO: is dead
      <> "0"
      <> ";" -- TODO: death count
      <> word32Dec (unCharacterMaxLevel ml)
  {-# INLINE toNetwork #-}

getCharacterList ::
  WorldId -> AccountId -> GameQuery [CharacterListInfo]
getCharacterList wid accId =
  fmap go
    <$> GameQuery
      ( runSelectReturningList $
          select $ do
            c <- all_ (gameDb ^. character)
            cl <- all_ (gameDb ^. characterLook)
            guard_
              ( c
                  ^. characterAccountId
                  ==. val_ accId
                  &&. cl
                  ^. characterLookCharacterId
                  ==. (c ^. characterId)
              )
            pure
              $! ( c ^. characterId,
                   c ^. characterName,
                   c ^. characterLevel,
                   cl ^. characterLookGfxId,
                   cl ^. characterLookFirstColor,
                   cl ^. characterLookSecondColor,
                   cl ^. characterLookThirdColor,
                   c ^. characterMaxLevel
                 )
      )
  where
    go (i, n, l, gi, c1, c2, c3, ml) = CharacterListInfo i n l gi c1 c2 c3 ml wid
