-- Message.hs ---

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

{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Game.Network.Message where

import           Data.ByteString.Lazy.Builder
import           OpenDofus.Core.Network.Client
import           OpenDofus.Game.Server
import           OpenDofus.Database
import           OpenDofus.Game.Character
import           OpenDofus.Game.Map.Actor
import           OpenDofus.Prelude

data CharacterCreationFailureReason
  = CharacterCreationFailureReasonFull
  | CharacterCreationFailureReasonNameAlreadyExists
  | CharacterCreationFailureReasonBadName
  | CharacterCreationFailureReasonSubscriptionIsOver
  | CharacterCreationFailureReasonInvalidBreed
  | CharacterCreationFailureReasonInvalidInfos

data GameMessage
  = HelloGame
  | AccountTicketIsInvalid
  | AccountTicketIsValid
  | AccountRegionalVersion
  | CharacterList
      AccountRemainingSubscriptionInMilliseconds
      [CharacterListInfo]
  | CharacterCreationSuccess
  | CharacterCreationFailure CharacterCreationFailureReason
  | CharacterSelectionSuccess (PlayerCharacter GameClient)
  | GameCreationSuccess
  | GameDataMap MapId MapCreationDate MapDataKey

instance ToNetwork GameMessage where
  {-# INLINE toNetwork #-}
  toNetwork HelloGame              = "HG"
  toNetwork AccountTicketIsInvalid = "ATE"
  toNetwork AccountTicketIsValid   = "ATK0"
  toNetwork AccountRegionalVersion = "AVen"
  toNetwork (CharacterList subscription characters) =
    "ALK"
      <> word32Dec (unAccountRemainingSubscriptionInMilliseconds subscription)
      <> "|"
      <> intDec (length characters)
      <> toNetwork (FoldNetwork characters)
  toNetwork (CharacterCreationFailure reason) = "AAE" <> go reason
   where
    go CharacterCreationFailureReasonFull = "f"
    go CharacterCreationFailureReasonNameAlreadyExists = "a"
    go CharacterCreationFailureReasonBadName = "n"
    go CharacterCreationFailureReasonSubscriptionIsOver = "s"
    go _ = ""
  toNetwork CharacterCreationSuccess = "AAK"
  toNetwork (CharacterSelectionSuccess pc) =
    let base = pc ^. playerCharacterBaseCharacter
        look = pc ^. playerCharacterCharacterLook
    in  "ASK|"
          <> word64Dec (unCharacterId $ base ^. characterId)
          <> "|"
          <> byteString
               (encodeTextStrict $ unCharacterName $ base ^. characterName)
          <> "|"
          <> word32Dec (unCharacterLevel $ base ^. characterLevel)
          <> "|"
          <> intDec (unBreedId $ base ^. characterBreedId)
          <> "|"
          <> bool "0" "1" (unCharacterSex $ look ^. characterLookSex)
          <> "|"
          <> word32Dec (unGfxId $ look ^. characterLookGfxId)
          <> "|"
          <> intDec (unCharacterColor $ look ^. characterLookFirstColor)
          <> "|"
          <> intDec (unCharacterColor $ look ^. characterLookSecondColor)
          <> "|"
          <> intDec (unCharacterColor $ look ^. characterLookThirdColor)
          <> "|"
          <> "" -- TODO: inventory content
          <> "|"
  toNetwork GameCreationSuccess = "GCK|1|"
  toNetwork (GameDataMap i d k) =
    "GDM|"
      <> word32Dec (unMapId i)
      <> "|"
      <> byteString (encodeTextStrict $ unMapCreationDate d)
      <> "|"
      <> byteString (encodeTextStrict $ unMapDataKey k)

