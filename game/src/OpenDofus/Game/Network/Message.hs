{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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

module OpenDofus.Game.Network.Message
  ( GameMessage (..),
    ToMapActorSpawn (..),
    ToMapActorDespawn (..),
    CharacterCreationFailureReason (..),
  )
where

import Data.ByteString.Lazy.Builder
import Numeric.Lens
import OpenDofus.Core.Network.Client
import OpenDofus.Database
import OpenDofus.Game.Character
import OpenDofus.Game.Map.Actor
import OpenDofus.Prelude

data CharacterCreationFailureReason
  = CharacterCreationFailureReasonFull
  | CharacterCreationFailureReasonNameAlreadyExists
  | CharacterCreationFailureReasonBadName
  | CharacterCreationFailureReasonSubscriptionIsOver
  | CharacterCreationFailureReasonInvalidBreed
  | CharacterCreationFailureReasonInvalidInfos

data GameMessage
  = FullySerialized Builder
  | HelloGame
  | AccountTicketIsInvalid
  | AccountTicketIsValid
  | AccountRegionalVersion
  | CharacterList
      {-# UNPACK #-} !AccountRemainingSubscriptionInMilliseconds
      ![CharacterListInfo]
  | CharacterCreationSuccess
  | CharacterCreationFailure
      !CharacterCreationFailureReason
  | CharacterSelectionSuccess
      {-# UNPACK #-} !PlayerCharacter
  | GameCreationSuccess
  | GameDataMap
      {-# UNPACK #-} !MapId
      {-# UNPACK #-} !MapCreationDate
      {-# UNPACK #-} !MapDataKey
  | MapActorSpawn
      ![Actor]
  | MapActorDespawn
      ![ActorId]
  | AccountStats
  | AccountRestrictions
      {-# UNPACK #-} !ActorRestrictionSet
  | GameDataSuccess
  | GameAction
      !Bool
      {-# UNPACK #-} !EffectId
      {-# UNPACK #-} !ActorId
      {-# UNPACK #-} !ByteString
  | BasicNoOperation

instance ToNetwork GameMessage where
  toNetwork (FullySerialized x) = x
  toNetwork HelloGame = "HG"
  toNetwork AccountTicketIsInvalid = "ATE"
  toNetwork AccountTicketIsValid = "ATK0"
  toNetwork AccountRegionalVersion = "AVen"
  toNetwork (CharacterList subscription characters) =
    "ALK"
      <> word32Dec (unAccountRemainingSubscriptionInMilliseconds subscription)
      <> "|"
      <> int32Dec (fromIntegral $ length characters)
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
    "ASK|"
      <> word64Dec (unCharacterId $ baseChar ^. characterId)
      <> "|"
      <> byteString
        (encodeTextStrict $ unCharacterName $ baseChar ^. characterName)
      <> "|"
      <> word32Dec (unCharacterLevel $ baseChar ^. characterLevel)
      <> "|"
      <> word32Dec (unBreedId $ baseChar ^. characterBreedId)
      <> "|"
      <> bool "0" "1" (unCharacterSex $ look ^. characterLookSex)
      <> "|"
      <> word32Dec (unGfxId $ look ^. characterLookGfxId)
      <> "|"
      <> int32Dec (unCharacterColor $ look ^. characterLookFirstColor)
      <> "|"
      <> int32Dec (unCharacterColor $ look ^. characterLookSecondColor)
      <> "|"
      <> int32Dec (unCharacterColor $ look ^. characterLookThirdColor)
      <> "|"
      <> "" -- TODO: inventory content look
      <> "|"
    where
      baseChar = pc ^. playerCharacterBaseCharacter
      look = pc ^. playerCharacterCharacterLook
  toNetwork GameCreationSuccess = "GCK|1|"
  toNetwork (GameDataMap i d k) =
    "GDM|"
      <> word32Dec (unMapId i)
      <> "|"
      <> byteString (encodeTextStrict $ unMapCreationDate d)
      <> "|"
      <> byteString (encodeTextStrict $ unMapDataKey k)
  -- TODO: account/character statistics
  toNetwork AccountStats = "As"
  toNetwork GameDataSuccess = "GDK"
  toNetwork (MapActorSpawn actors) =
    "GM" <> toNetwork (FoldNetwork (ToMapActorSpawn <$> actors))
  toNetwork (MapActorDespawn actors) =
    "GM" <> toNetwork (FoldNetwork (ToMapActorDespawn <$> actors))
  toNetwork (AccountRestrictions restrictions) =
    "AR" <> string8 (encodeRestrictions restrictions ^. re (base 36))
  toNetwork BasicNoOperation =
    "BN"
  toNetwork (GameAction ackRequired eid aid params) =
    "GA" <> bool mempty (word32Dec (unEffectId eid)) ackRequired
      <> ";"
      <> word32Dec (unEffectId eid)
      <> ";"
      <> word64Dec (unActorId aid)
      <> ";"
      <> byteString params
  {-# INLINE toNetwork #-}

newtype ToMapActorSpawn = ToMapActorSpawn Actor

instance ToNetwork ToMapActorSpawn where
  toNetwork (ToMapActorSpawn a) =
    "|+" <> word32Dec cid <> ";" <> word8Dec dir <> ";" <> go a
    where
      cid = unCellId $ a ^. actorLocation . actorLocationCellId
      dir = fromIntegral . fromEnum $ a ^. direction
      aid = a ^. to actorId
      go (Actor _ _ _ (ActorSpecializationPC pc)) =
        "0" -- entity type: player character
          <> ";"
          <> word64Dec (unActorId aid)
          <> ";"
          <> byteString
            (encodeTextStrict $ unCharacterName $ baseChar ^. characterName)
          <> ";"
          <> word32Dec (unBreedId $ baseChar ^. characterBreedId)
          -- TODO: ,titleId*titleParams
          <> ";"
          <> word32Dec (unGfxId $ look ^. characterLookGfxId)
          <> "^"
          <> word32Dec (unGfxSize $ look ^. characterLookGfxSize)
          <> ";"
          <> bool "0" "1" (unCharacterSex $ look ^. characterLookSex)
          <> ";"
          <> "0,0,0" -- TODO: alignment
          <> ","
          <> word64Dec
            ( unActorId aid
                + fromIntegral (unCharacterLevel $ baseChar ^. characterLevel)
            )
          <> ";"
          <> int32HexFixed
            (fromIntegral $ unCharacterColor $ look ^. characterLookFirstColor)
          <> ";"
          <> int32HexFixed
            ( fromIntegral $ unCharacterColor $ look ^. characterLookSecondColor
            )
          <> ";"
          <> int32HexFixed
            (fromIntegral $ unCharacterColor $ look ^. characterLookThirdColor)
          <> ";"
          <> "" -- TODO: inventory content look
          <> ";"
          <> "2" -- TODO: aura
          <> ";"
          <> "0" -- TODO: last emote
          <> ";"
          <> "0" -- TODO: emote timer
          <> ";"
          <> "" -- TODO: guild name
          <> ";"
          <> "" -- TODO: guild emblem
          <> ";"
          <> string8
            ( encodeRestrictions (pc ^. playerCharacterRestrictions)
                ^. re (base 36)
            )
          <> ";"
          <> "" -- TODO: mount light infos
          <> ";"
        where
          baseChar = pc ^. playerCharacterBaseCharacter
          look = pc ^. playerCharacterCharacterLook
      {-# INLINE go #-}
  {-# INLINE toNetwork #-}

newtype ToMapActorDespawn = ToMapActorDespawn ActorId

instance ToNetwork ToMapActorDespawn where
  toNetwork (ToMapActorDespawn aid) = "|-" <> word64Dec (unActorId aid)
  {-# INLINE toNetwork #-}
