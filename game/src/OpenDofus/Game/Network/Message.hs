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

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Game.Network.Message where

import           Data.ByteString.Lazy.Builder
import qualified Data.Vector                   as V
import           Numeric.Lens
import           OpenDofus.Core.Network.Client
import           OpenDofus.Game.Character
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

newtype ToMapActorSpawn a = ToMapActorSpawn (GameActor a)

instance ToNetwork (ToMapActorSpawn a) where
  toNetwork (ToMapActorSpawn !actor) =
    "|+" <> word32Dec cid <> ";" <> intDec dir <> ";" <> go actor
   where
    !cid = unCellId $ snd $ actor ^. to position
    !dir = fromEnum $ actor ^. to direction
    !aid = actor ^. to actorId
    go (GameActorPC pc) =
      "0" -- entity type
        <> ";"
        <> word64Dec (unActorId aid)
        <> ";"
        <> byteString
             (encodeTextStrict $ unCharacterName $ baseChar ^. characterName)
        <> ";"
        <> intDec (unBreedId $ baseChar ^. characterBreedId)
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
             (fromIntegral $ unCharacterColor $ look ^. characterLookSecondColor
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
             (  encodeRestrictions (pc ^. playerCharacterRestrictions)
             ^. re (base 36)
             )
        <> ";"
        <> "" -- TODO: mount light infos
        <> ";"
     where
      !baseChar = pc ^. playerCharacterBaseCharacter
      !look     = pc ^. playerCharacterCharacterLook

newtype ToMapActorDespawn a = ToMapActorDespawn (GameActor a)

instance ToNetwork (ToMapActorDespawn a) where
  toNetwork (ToMapActorDespawn !actor) = "|-" <> word64Dec (unActorId aid)
    where !aid = actor ^. to actorId

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
      !AccountRemainingSubscriptionInMilliseconds
      !(V.Vector CharacterListInfo)
  | CharacterCreationSuccess
  | CharacterCreationFailure !CharacterCreationFailureReason
  | CharacterSelectionSuccess !(PlayerCharacter GameClient)
  | GameCreationSuccess
  | GameDataMap !MapId !MapCreationDate !MapDataKey
  | MapActorSpawn !(V.Vector (GameActor GameClientController))
  | MapActorDespawn !(V.Vector (GameActor GameClientController))
  | AccountStats
  | AccountRestrictions !ActorRestrictionSet
  | GameDataSuccess

instance ToNetwork GameMessage where
  toNetwork HelloGame              = "HG"

  toNetwork AccountTicketIsInvalid = "ATE"

  toNetwork AccountTicketIsValid   = "ATK0"

  toNetwork AccountRegionalVersion = "AVen"

  toNetwork (CharacterList !subscription !characters) =
    "ALK"
      <> word32Dec (unAccountRemainingSubscriptionInMilliseconds subscription)
      <> "|"
      <> intDec (length characters)
      <> toNetwork (FoldNetwork characters)

  toNetwork (CharacterCreationFailure !reason) = "AAE" <> go reason
   where
    go CharacterCreationFailureReasonFull = "f"
    go CharacterCreationFailureReasonNameAlreadyExists = "a"
    go CharacterCreationFailureReasonBadName = "n"
    go CharacterCreationFailureReasonSubscriptionIsOver = "s"
    go _ = ""

  toNetwork CharacterCreationSuccess = "AAK"

  toNetwork (CharacterSelectionSuccess !pc) =
    "ASK|"
      <> word64Dec (unCharacterId $ baseChar ^. characterId)
      <> "|"
      <> byteString
           (encodeTextStrict $ unCharacterName $ baseChar ^. characterName)
      <> "|"
      <> word32Dec (unCharacterLevel $ baseChar ^. characterLevel)
      <> "|"
      <> intDec (unBreedId $ baseChar ^. characterBreedId)
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
      <> "" -- TODO: inventory content look
      <> "|"
   where
    !baseChar = pc ^. playerCharacterBaseCharacter
    !look     = pc ^. playerCharacterCharacterLook

  toNetwork GameCreationSuccess = "GCK|1|"

  toNetwork (GameDataMap !i !d !k) =
    "GDM|"
      <> word32Dec (unMapId i)
      <> "|"
      <> byteString (encodeTextStrict $ unMapCreationDate d)
      <> "|"
      <> byteString (encodeTextStrict $ unMapDataKey k)

  toNetwork AccountStats    = "As" -- TODO: fix

  toNetwork GameDataSuccess = "GDK"

  toNetwork (MapActorSpawn !actors) =
    "GM" <> toNetwork (FoldNetwork (ToMapActorSpawn <$> actors))

  toNetwork (MapActorDespawn !actors) =
    "GM" <> toNetwork (FoldNetwork (ToMapActorDespawn <$> actors))

  toNetwork (AccountRestrictions !restrictions) =
    "AR" <> string8 (encodeRestrictions restrictions ^. re (base 36))
