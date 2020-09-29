-- CharacterSelection.hs ---

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

{-# LANGUAGE TypeApplications #-}

module OpenDofus.Game.Frame.CharacterSelection
  ( characterSelectionHandler
  )
where

import           Data.Attoparsec.ByteString.Char8
                                               as A
import           Data.Attoparsec.ByteString.Lazy
                                               as AL
import qualified Data.ByteString.Lazy          as BS
import           OpenDofus.Core.Network.Server
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Game.Character
import           OpenDofus.Game.Frame.GameCreation
import           OpenDofus.Game.Map.Actor
import           OpenDofus.Game.Network.Message
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

type CharacterCreationInfos
  = ( CharacterName
    , BreedId
    , CharacterSex
    , CharacterColor
    , CharacterColor
    , CharacterColor
    )

{-# INLINE parseCharacterCreationInfos #-}
parseCharacterCreationInfos :: Parser CharacterCreationInfos
parseCharacterCreationInfos =
  let dec = A.char '|' *> signed decimal
  in  (,,,,,)
        <$> (CharacterName . decodeLazyByteString . BS.fromStrict <$> A.takeTill
              (== '|')
            )
        <*> (BreedId <$> dec)
        <*> (   CharacterSex
            <$> (A.char '|' *> do
                  x <- digit
                  case x of
                    '0' -> pure False
                    _   -> pure True
                )
            )
        <*> (CharacterColor <$> dec)
        <*> (CharacterColor <$> dec)
        <*> (CharacterColor <$> dec)

sendCharacterList :: Account -> GameHandlerCallback ()
sendCharacterList acc = do
  wid        <- asks (view (handlerInputServer . gameServerWorldId))
  characters <- runVolatile @GameDbConn
    $ getCharacterList wid (acc ^. accountId)
  remainingSubscription <- getAccountRemainingSubscriptionInMilliseconds
    (acc ^. accountSubscriptionExpirationDate)
  sendMessage $ CharacterList remainingSubscription characters

characterCreationHandler
  :: Account
  -> CharacterCreationInfos
  -> GameHandlerCallback (Maybe CharacterCreationFailureReason)
characterCreationHandler acc (cn, bi, sex, c1, c2, c3) =
  runSerializable @GameDbConn go
 where
  go :: GameQuery (Maybe CharacterCreationFailureReason)
  go = do
    c <- getCharacterByName cn
    b <- getBreedById bi
    case (c, b) of
      (Just _, _) ->
        pure $ Just CharacterCreationFailureReasonNameAlreadyExists
      (_, Nothing) -> pure $ Just CharacterCreationFailureReasonInvalidBreed
      (Nothing, Just foundBreed) -> do
        void $ createNewCharacter (acc ^. accountId) cn foundBreed sex c1 c2 c3
        pure Nothing

characterSelectionHandler :: Account -> GameClientHandler
characterSelectionHandler acc = MessageHandlerCont $ go =<< ask
  (view handlerInputMessage)
 where
  go (ClientSent ('A' :- ('V' :- _))) = do
    sendMessage AccountRegionalVersion
    pure $ characterSelectionHandler acc
  go (ClientSent ('A' :- ('L' :- _))) = do
    sendCharacterList acc
    pure $ characterSelectionHandler acc
  go (ClientSent ('A' :- ('A' :- providedCharInfos))) =
    case AL.parse parseCharacterCreationInfos providedCharInfos of
      AL.Done _ charInfos -> do
        creationResult <- characterCreationHandler acc charInfos
        case creationResult of
          Just failureReason ->
            sendMessage $ CharacterCreationFailure failureReason
          Nothing -> do
            sendMessage CharacterCreationSuccess
            sendCharacterList acc
        pure $ characterSelectionHandler acc
      _ -> do
        sendMessage $ CharacterCreationFailure
          CharacterCreationFailureReasonInvalidInfos
        pure $ characterSelectionHandler acc
  go (ClientSent ('A' :- ('S' :- providedCharacterId))) =
    case AL.parse (decimal @Word64) providedCharacterId of
      AL.Done _ cid -> do
        gc       <- asks (view handlerInputClient)
        loadedPc <- loadPlayerCharacter gc (CharacterId cid)
        case loadedPc of
          Just pc -> do
            sendMessage $ CharacterSelectionSuccess pc
            pure $ gameCreationHandler pc
          Nothing -> pure $ characterSelectionHandler acc
      _ -> pure $ characterSelectionHandler acc
  go _ = pure $ characterSelectionHandler acc
