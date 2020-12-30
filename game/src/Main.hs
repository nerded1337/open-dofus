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

import Control.Concurrent.Chan.Unagi.NoBlocking as U
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Compact
import Data.Either
import qualified Data.HashTable.IO as H
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
import System.Mem (performMajorGC, performMinorGC)

type EffectIdText = ByteString

-- * GAME

raiseMapEvent :: MapEvent -> GameHandler ()
raiseMapEvent e = do
  st <- readState
  case st of
    (GameCreation _ pc) -> go (pc ^. to actorId)
    (InGame _ aid) -> go aid
    _ -> pure ()
  where
    go aid = do
      s <- view handlerInputServer
      mid <- liftIO $ atomically $ M.lookup aid $ s ^. gameServerPlayerToMap
      case mid of
        Just foundMapId -> do
          mcs <- liftIO $ H.lookup (s ^. gameServerMapChannels) foundMapId
          case mcs of
            Just foundChans -> do
              liftIO $ U.writeChan (foundChans ^. mapEventChannelsIn) e
            _ -> do
              pure ()
        _ -> do
          pure ()
{-# INLINE raiseMapEvent #-}

gameActionAbort ::
  ActorId ->
  EffectIdText ->
  Either String MapEvent
gameActionAbort aid effectIdText =
  go <$> A.parseOnly (A.decimal @CellId) effectIdText
  where
    go cid =
      MapEventActorActionAbort
        aid
        cid

gameActionAck ::
  ActorId ->
  EffectIdText ->
  Either String MapEvent
gameActionAck aid effectIdText =
  go <$> A.parseOnly (A.decimal @EffectId) effectIdText
  where
    go eid =
      MapEventActorActionAck
        aid
        eid

gameActionStart ::
  ActorId ->
  EffectIdText ->
  EffectParams ->
  Either String MapEvent
gameActionStart aid effectIdText params =
  go <$> A.parseOnly (A.decimal @EffectId) effectIdText
  where
    go eid =
      MapEventActorActionStart
        aid
        eid
        params

-- * GAME CREATION

gameCreation :: Account -> PlayerCharacter -> GameHandler GameState
gameCreation acc pc = do
  s <- view handlerInputServer
  ctl <-
    liftIO $
      H.lookup
        (s ^. gameServerMapControllers)
        (pc ^. playerCharacterCharacterPosition . characterPositionMapId)
  case ctl of
    (Just foundCtl) -> do
      let m = foundCtl ^. mapControllerInstance . mapInstanceTemplate
      case m ^. mapDataKey of
        Just dataKey -> do
          liftIO $
            atomically $
              M.insert (m ^. mapId) (pc ^. to actorId) $ s ^. gameServerPlayerToMap
          raiseMapEvent $
            MapEventActorSpawn
              ( Actor
                  ActorIdle
                  ( ActorLocation
                      (pc ^. playerCharacterCharacterPosition . characterPositionMapId)
                      (pc ^. playerCharacterCharacterPosition . characterPositionCellId)
                  )
                  SouthEast
                  (ActorSpecializationPC pc)
              )
          emit GameCreationSuccess
          emit $ GameDataMap (m ^. mapId) (m ^. mapCreationDate) dataKey
          emit GameDataSuccess
          pure $ InGame acc (pc ^. to actorId)
        Nothing -> do
          -- impossible as the controller is loaded only if the map is valid
          kick
          stay
    _ -> do
      -- should be impossible except if we messed with the db
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

parseCharacterCreationInfos :: A.Parser CharacterCreationInfos
parseCharacterCreationInfos =
  let dec :: (Num a, Integral a) => A.Parser a
      dec = A.char '|' *> A.signed A.decimal
   in (,,,,,)
        <$!> (CharacterName . decodeStrictByteString <$> A.takeTill (== '|'))
        <*> (BreedId <$> dec)
        <*> ( CharacterSex
                <$> ( A.char '|' *> do
                        x <- A.digit
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
  case A.parseOnly parseCharacterCreationInfos encodedCharInfos of
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
  case A.parseOnly (A.decimal @Word64) encodedCharacterId of
    Right cid -> do
      loadedPc <- loadPlayerCharacter (CharacterId cid)
      case loadedPc of
        Just pc -> do
          debug $ "Login player: " <> showText pc
          playerToClient <- view (handlerInputServer . gameServerPlayerToClient)
          client <- view handlerInputClient
          liftIO $ atomically $ M.insert client (pc ^. to actorId) playerToClient
          emit $ CharacterSelectionSuccess pc
          emit $ AccountRestrictions (pc ^. playerCharacterRestrictions)
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

onAccountDisconnected :: Account -> GameHandler ()
onAccountDisconnected acc = do
  debug $ "Logout account: " <> showText acc
  runVolatile @AuthDbConn $
    setAccountIsOnline (acc ^. accountId) $
      AccountIsOnline False

onPlayerDisconnected :: ActorId -> GameHandler ()
onPlayerDisconnected aid = do
  debug $ "Logout player: " <> showText aid
  raiseMapEvent $ MapEventActorDespawn aid
  playerToClient <- view (handlerInputServer . gameServerPlayerToClient)
  liftIO $ atomically $ M.delete aid playerToClient
  playerToMap <- view (handlerInputServer . gameServerPlayerToMap)
  liftIO $ atomically $ M.delete aid playerToMap

onClientConnected :: GameHandler GameState
onClientConnected = do
  emit HelloGame
  stay

stateHandler :: GameState -> ClientMessage -> GameHandler GameState
stateHandler _ ClientConnected =
  onClientConnected
stateHandler s ClientDisconnected =
  case s of
    Greeting ->
      stay
    CharacterSelection acc -> do
      onAccountDisconnected acc
      stay
    GameCreation acc pc -> do
      onAccountDisconnected acc
      onPlayerDisconnected (pc ^. to actorId)
      stay
    InGame acc aid -> do
      onAccountDisconnected acc
      onPlayerDisconnected aid
      stay
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
    (InGame _ aid, _) -> do
      case packet of
        'G' :- 'I' :- _ ->
          raiseMapEvent $ MapEventDispatchInformations aid
        'G' :- 'A' :- a0 :- a1 :- a2 :- params ->
          traverse_
            raiseMapEvent
            $ gameActionStart aid (a0 :- a1 :- a2 :- mempty) params
        'G' :- 'K' :- 'K' :- eid ->
          traverse_
            raiseMapEvent
            $ gameActionAck aid eid
        'G' :- 'K' :- 'E' :- '1' :- '|' :- cid ->
          traverse_
            raiseMapEvent
            $ gameActionAbort aid cid
        _ -> do
          debug $ "Unhandled game packet: " <> showText packet
      stay
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

logMessage :: GameHandler ()
logMessage = do
  debug . showText =<< view handlerInputMessage

dispatchMessage :: M.Map ActorId GameClient -> ActorId -> GameMessage -> IO ()
dispatchMessage playerToClient actId msg = do
  client <- atomically $ M.lookup actId playerToClient
  traverse_ (`emitToClient` Identity msg) client

app :: IO ()
app = do
  compactStore <- compact ()

  let makeConnInfo =
        ConnectInfo "localhost" 5432 "nerded" "nerded"
  authDbPool <- createConnPool $ makeConnInfo "opendofus_auth"
  gameDbPool <- createConnPool $ makeConnInfo "opendofus_game"

  playerToClient <- M.newIO

  playerToMap <- M.newIO

  mapChannels <- H.new

  debug "Loading map templates"
  mapTemplates <-
    traverse (fmap getCompact . compactAddWithSharing compactStore)
      =<< runReaderT
        (runVolatile @GameDbConn getMaps)
        gameDbPool
  debug $ "Map template loaded: " <> showText (length mapTemplates)

  debug "Creating map instances"
  (_, mapInstances) <-
    (traverse . traverse) (fmap getCompact . compactAddWithSharing compactStore)
      . partitionEithers
      =<< traverse
        (\t -> runReaderT (createMapInstance t) gameDbPool)
        mapTemplates
  debug $ "Map instances created: " <> showText (length mapInstances)

  debug "Creating map controllers"
  mapControllers <-
    runMaps
      mapChannels
      (dispatchMessage playerToClient)
      mapInstances

  threadDelay 2000000

  debug "Performing initial garbage collection"
  performMinorGC
  performMajorGC

  debug "Starting game server"
  server <-
    GameServer
      <$> (ServerState 8081 makeClient <$> M.newIO)
      <*> pure authDbPool
      <*> pure gameDbPool
      <*> pure (WorldId 614)
      <*> H.fromList (controllerEntry <$> mapControllers)
      <*> pure mapChannels
      <*> pure playerToClient
      <*> pure playerToMap
      <*> pure compactStore
  startServer server dispatchToHandler
  where
    controllerEntry c =
      (c ^. mapControllerInstance . mapInstanceTemplate . mapId, c)
    makeClient conn = GameClient <$> newIORef Greeting <*> pure conn

main :: IO ()
main = runApp app
