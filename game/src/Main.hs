{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- Main.hs ---

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

module Main where

import qualified Data.Attoparsec.ByteString.Char8 as AL
import Data.Either
import qualified Data.HashMap.Strict as HM
import Database.Beam.Postgres
import OpenDofus.Core.Application
import OpenDofus.Core.Data.Constructible
import OpenDofus.Core.Network.Server
import OpenDofus.Database
import OpenDofus.Game.Character (getCharacterList)
import OpenDofus.Game.Map
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Types
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Server
import OpenDofus.Prelude
import qualified StmContainers.Map as M

-- * GAME CREATION

gameCreation :: Account -> PlayerCharacter -> GameHandler GameState
gameCreation acc pc = do
  gs <- view handlerInputServer
  let mid = pc ^. actorLocation . actorLocationMapId
      ctls = gs ^. gameServerMapControllers
  case ctls ^. at mid of
    (Just ctl) -> do
      let mapTemplate =
            ctl ^. mapControllerInstance . mapInstanceTemplate
          gameDataMapMsg =
            GameDataMap
              (mapTemplate ^. mapId)
              (mapTemplate ^. mapCreationDate)
              <$> (mapTemplate ^. mapDataKey)
      case gameDataMapMsg of
        Just dataMapMessage -> do
          raiseMapEvent mapTemplate $ MapEventActorSpawn $ GameActorPC pc
          emit GameCreationSuccess
          emit dataMapMessage
          emit GameDataSuccess
          pure $ InGame acc pc mapTemplate
        Nothing -> do
          -- impossible as the controller is loaded only if the map is valid
          critical $
            "Map without key"
              <> showText mid
              <> ", player kicked: "
              <> showText (pc ^. playerCharacterBaseCharacter . characterName)
          kick
          stay
    _ -> do
      -- should be impossible except if we messed with the db
      critical $
        "Map controller not found: "
          <> showText mid
      kick
      stay

-- * CHARACTER SELECTION

type EncodedCharacterId = ClientPacket

type EncodedCharacterCreationInfos = ClientPacket

type CharacterCreationInfos =
  ( CharacterName,
    BreedId,
    CharacterSex,
    CharacterColor,
    CharacterColor,
    CharacterColor
  )

parseCharacterCreationInfos :: AL.Parser CharacterCreationInfos
parseCharacterCreationInfos =
  let dec :: (Num a, Integral a) => AL.Parser a
      dec = AL.char '|' *> AL.signed AL.decimal
   in (,,,,,)
        <$> (CharacterName . decodeStrictByteString <$> AL.takeTill (== '|'))
        <*> (BreedId <$> dec)
        <*> ( CharacterSex
                <$> ( AL.char '|' *> do
                        x <- AL.digit
                        case x of
                          '0' -> pure False
                          _ -> pure True
                    )
            )
        <*> (CharacterColor <$> dec)
        <*> (CharacterColor <$> dec)
        <*> (CharacterColor <$> dec)

charactersList :: Account -> GameHandler GameState
charactersList acc = do
  wid <- view (handlerInputServer . gameServerWorldId)
  characters <-
    runVolatile @GameDbConn $ getCharacterList wid (acc ^. accountId)
  remainingSubscription <-
    getAccountRemainingSubscriptionInMilliseconds
      (acc ^. accountSubscriptionExpirationDate)
  emit $ CharacterList remainingSubscription characters
  stay

characterCreationHandler ::
  Account ->
  CharacterCreationInfos ->
  GameHandler (Maybe CharacterCreationFailureReason)
characterCreationHandler acc (cn, bi, sex, c1, c2, c3) =
  runSerializable @GameDbConn $ do
    c <- getCharacterByName cn
    b <- getBreedById bi
    case (c, b) of
      (Just _, _) ->
        pure $ Just CharacterCreationFailureReasonNameAlreadyExists
      (_, Nothing) ->
        pure $ Just CharacterCreationFailureReasonInvalidBreed
      (Nothing, Just foundBreed) -> do
        void $ createNewCharacter (acc ^. accountId) cn foundBreed sex c1 c2 c3
        pure Nothing

characterCreation ::
  Account ->
  EncodedCharacterCreationInfos ->
  GameHandler GameState
characterCreation acc encodedCharInfos =
  case AL.parseOnly parseCharacterCreationInfos encodedCharInfos of
    Right charInfos -> do
      creationResult <- characterCreationHandler acc charInfos
      case creationResult of
        Just failureReason -> do
          emit $ CharacterCreationFailure failureReason
          stay
        Nothing -> do
          emit CharacterCreationSuccess
          charactersList acc
    Left _ -> do
      warn $
        "Could not decode character creation infos: " <> showText encodedCharInfos
      emit $
        CharacterCreationFailure
          CharacterCreationFailureReasonInvalidInfos
      stay

characterSelection :: Account -> EncodedCharacterId -> GameHandler GameState
characterSelection acc encodedCharacterId =
  case AL.parseOnly (AL.decimal @Word64) encodedCharacterId of
    Right cid -> do
      loadedPc <- loadPlayerCharacter (CharacterId cid)
      case loadedPc of
        Just pc -> do
          debug $ "Login player: " <> showText pc
          playerActors <- view (handlerInputServer . gameServerPlayerActors)
          client <- view handlerInputClient
          liftIO $ atomically $ M.insert client (pc ^. to actorId) playerActors
          emit $ CharacterSelectionSuccess pc
          pure $ GameCreation acc pc
        Nothing -> do
          warn $ "Could not load character: id=" <> showText encodedCharacterId
          stay
    Left _ -> do
      warn $ "Could not decode character id: " <> showText encodedCharacterId
      stay

-- * LOGIN

defaultTicketTimeout :: NominalDiffTime
defaultTicketTimeout = 5

ticketHasTimedOut :: UTCTime -> AccountTicketCreationDate -> Bool
ticketHasTimedOut now tickDate =
  diffUTCTime now (unAccountTicketCreationDate tickDate) >= defaultTicketTimeout

loginWithTicket :: UTCTime -> AccountTicketId -> AuthQuery (Maybe Account)
loginWithTicket now tickId = go =<< getAccountByTicket tickId
  where
    go (Just (tick, acc))
      | not (ticketHasTimedOut now (tick ^. accountTicketCreationDate))
          && not (alreadyOnline acc) = do
        setIsOnline acc True
        pure $ Just acc
    go (Just (tick, acc))
      | not (ticketHasTimedOut now (tick ^. accountTicketCreationDate))
          && alreadyOnline acc = do
        setIsOnline acc False
        pure Nothing
    go _ =
      pure Nothing

    setIsOnline acc =
      setAccountIsOnline (acc ^. accountId) . AccountIsOnline

    alreadyOnline acc =
      unAccountIsOnline (acc ^. accountIsOnline)

loginAccount :: AccountTicketId -> GameHandler GameState
loginAccount ticket = do
  now <- getCurrentTime
  result <- runSerializable @AuthDbConn $ loginWithTicket now ticket
  case result of
    Just acc -> do
      debug $ "Login account: " <> showText acc
      emit AccountTicketIsValid
      pure $ CharacterSelection acc
    Nothing -> do
      emit AccountTicketIsInvalid
      stay

-- * CONNECTION

onClientDisconnected :: Maybe Account -> Maybe PlayerCharacter -> GameHandler GameState
onClientDisconnected loggedAccount loggedPc = do
  debug "Client disconnected"
  traverse_ playerDisconnect loggedPc
  traverse_ accountDisconnect loggedAccount
  stay
  where
    playerDisconnect pc = do
      -- TODO: trigger map event
      debug $ "Logout player: " <> showText pc
      playerActors <- view (handlerInputServer . gameServerPlayerActors)
      liftIO $ atomically $ M.delete (pc ^. to actorId) playerActors

    accountDisconnect acc = do
      debug $ "Logout account: " <> showText acc
      runVolatile @AuthDbConn $
        setAccountIsOnline (acc ^. accountId) $
          AccountIsOnline False

onClientConnected :: GameHandler GameState
onClientConnected = do
  debug "Client connected"
  emit HelloGame
  stay

stateHandler :: GameState -> ClientMessage -> GameHandler GameState
stateHandler _ ClientConnected =
  onClientConnected
stateHandler s ClientDisconnected =
  case s of
    Greeting ->
      onClientDisconnected Nothing Nothing
    CharacterSelection acc ->
      onClientDisconnected (Just acc) Nothing
    GameCreation acc pc ->
      -- TODO: handler player character disconnection
      onClientDisconnected (Just acc) (Just pc)
    InGame acc pc _ ->
      -- TODO: handler player character disconnection
      onClientDisconnected (Just acc) (Just pc)
stateHandler s (ClientSent packet) =
  case (s, packet) of
    (Greeting, 'A' :- 'T' :- providedTicket) ->
      case fromASCIIBytes providedTicket of
        Just ticket ->
          loginAccount (AccountTicketId ticket)
        Nothing -> do
          kick
          stay
    (Greeting, _) -> do
      kick
      stay
    (CharacterSelection acc, _) ->
      case packet of
        ('A' :- 'V' :- _) -> do
          emit AccountRegionalVersion
          stay
        ('A' :- 'L' :- _) ->
          charactersList acc
        ('A' :- 'A' :- encodedCharInfos) ->
          characterCreation acc encodedCharInfos
        ('A' :- 'S' :- encodedCharId) ->
          characterSelection acc encodedCharId
        _ ->
          stay
    (GameCreation acc pc, 'G' :- 'C' :- _) -> do
      gameCreation acc pc
    (InGame acc pc m, 'G' :- 'I' :- _) -> do
      raiseMapEvent m $ MapEventDispatchInformations $ GameActorPC pc
      pure $ InGame acc pc m
    (_, _) -> do
      debug $ "Unhandled packet: " <> showText packet
      stay

writeState :: GameState -> GameHandler ()
writeState s = write =<< cell
  where
    write = liftIO . (`writeIORef` s)
    cell = view (handlerInputClient . gameClientState)

readState :: GameHandler GameState
readState = read =<< cell
  where
    read = liftIO . readIORef
    cell = view (handlerInputClient . gameClientState)

stay :: GameHandler GameState
stay = readState

dispatchToHandler :: GameHandler ()
dispatchToHandler = writeState =<< transition =<< readStateAndMsg
  where
    transition = uncurry stateHandler
    readStateAndMsg = (,) <$> readState <*> currentMsg
    currentMsg = view handlerInputMessage

dispatchMessages :: M.Map ActorId GameClient -> ActorId -> [GameMessage] -> IO ()
dispatchMessages playerActors actId msgs = do
  client <- atomically $ M.lookup actId playerActors
  traverse_ (`sendClientMessages` msgs) client

app :: IO ()
app = do
  let makeConnInfo =
        ConnectInfo "localhost" 5432 "nerded" "nerded"
  authDbPool <- createConnPool $ makeConnInfo "opendofus_auth"
  gameDbPool <- createConnPool $ makeConnInfo "opendofus_game"

  playerActors <- M.newIO

  debug "Loading map templates"
  mapTemplates <-
    runReaderT (runVolatile @GameDbConn getMaps) gameDbPool
  debug $ "Map template loaded: " <> showText (length mapTemplates)

  debug "Creating map instances"
  (_, mapInstances) <-
    partitionEithers
      <$> traverse
        (\t -> runReaderT (createMapInstance t) gameDbPool)
        mapTemplates
  debug $ "Map instances created: " <> showText (length mapInstances)

  debug "Creating map controllers"
  mapControllers <-
    runMaps (dispatchMessages playerActors) mapInstances

  debug "Starting game server"
  server <-
    GameServer
      <$> (ServerState 8081 makeClient <$> M.newIO)
      <*> pure authDbPool
      <*> pure gameDbPool
      <*> pure (WorldId 614)
      <*> pure (HM.fromList $ controllerEntry <$> mapControllers)
      <*> pure playerActors
  startServer server dispatchToHandler
  where
    controllerEntry c =
      (c ^. mapControllerInstance . mapInstanceTemplate . mapId, c)
    makeClient conn = GameClient <$> newIORef Greeting <*> pure conn

main :: IO ()
main = runApp app
