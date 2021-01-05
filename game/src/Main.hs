{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

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

import Control.Concurrent
import Control.Concurrent.Chan.Unagi.NoBlocking as UB
import Control.Concurrent.Chan.Unagi.NoBlocking.Unboxed as U
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import Data.Compact
import Data.Either
import qualified Data.HashTable.IO as H
import Data.Ratio
import Database.Beam.Postgres
import OpenDofus.Core.Application
import OpenDofus.Core.Data.Constructible
import OpenDofus.Core.Data.Record
import OpenDofus.Core.Network.Server
import OpenDofus.Database
import OpenDofus.Game.Character (getCharacterList)
import OpenDofus.Game.Map
import OpenDofus.Game.Map.Action
import OpenDofus.Game.Map.Actor
import OpenDofus.Game.Map.Event
import OpenDofus.Game.Map.Types
import OpenDofus.Game.Network.Message
import OpenDofus.Game.Server
import OpenDofus.Game.Time
import OpenDofus.Prelude
import qualified StmContainers.Map as M
import System.Mem

type EffectIdText = ByteString

-- * GAME

raiseMapEvent :: f :<: MapEventVariant => f -> GameHandler ()
raiseMapEvent e = do
  st <- readState
  case st of
    (GameCreation _ aid) -> go aid
    (InGame _ aid) -> go aid
    _ -> pure ()
  where
    go aid = do
      s <- view handlerInputServer
      mid <- liftIO $ atomically $ M.lookup aid $ s ^. gameServerPlayerToMap
      case mid of
        Just foundMapId -> do
          ctl <- liftIO $ H.lookup (s ^. gameServerMapControllers) foundMapId
          case ctl of
            Just (_ :<*>: foundChans) -> do
              liftIO $
                U.writeChan
                  (foundChans ^. mapEventChannelsIn)
                  (MapEvent $ inj e)
            _ -> do
              pure ()
        _ -> do
          pure ()
{-# INLINE raiseMapEvent #-}

gameActionAbort ::
  ActorId ->
  EffectIdText ->
  Either String MapEventActorActionAbort
gameActionAbort aid effectIdText =
  go <$> A.parseOnly (A.decimal @CellId) effectIdText
  where
    go cid =
      MapEventActorActionAbort $ aid :<*>: cid

gameActionAck ::
  ActorId ->
  EffectIdText ->
  Either String MapEventActorActionAck
gameActionAck aid effectIdText =
  go <$> A.parseOnly (A.decimal @EffectId) effectIdText
  where
    go eid =
      MapEventActorActionAck $ aid :<*>: eid

gameActionStart ::
  ActorId ->
  EffectIdText ->
  EffectParams ->
  Either String MapEventActorActionStart
gameActionStart aid effectIdText params =
  go =<< A.parseOnly (A.decimal @EffectId) effectIdText
  where
    go EffectActionMapMovement =
      case serializeCompressedPath params of
        Just serializedPath ->
          Right $
            MapEventActorActionStart $
              aid :<*>: inj (MapEventActorActionStartMovement serializedPath)
        Nothing ->
          Left $
            "Could not serialize path, probably too long: len: "
              <> show (BS.length params `quot` 3)
              <> ", path: "
              <> show params
    go _ =
      Left $
        "Unknown game action: "
          <> show effectIdText
          <> ", params: "
          <> show params

-- * GAME CREATION

gameCreation :: Account -> ActorId -> GameHandler GameState
gameCreation acc aid = do
  s <- view handlerInputServer
  cp <- runVolatile @GameDbConn $ getCharacterPosition $ coerce aid
  case cp of
    Just actualPosition -> do
      ctl <-
        liftIO $
          H.lookup
            (s ^. gameServerMapControllers)
            (actualPosition ^. characterPositionMapId)
      case ctl of
        (Just (foundCtl :<*>: _)) -> do
          let m = foundCtl ^. mapControllerInstance . mapInstanceTemplate
          liftIO $
            atomically $
              M.insert
                (m ^. mapId)
                aid
                (s ^. gameServerPlayerToMap)
          raiseMapEvent $
            MapEventActorSpawn $
              MapEventActorSpawnTypePlayer :<*>: aid
          pure $ InGame acc aid
        _ -> do
          -- should be impossible except if we messed with the db
          kick
          stay
    Nothing -> do
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
  wid <- view $ handlerInputServer . gameServerWorldId
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
        let characterMinimumId = 100000
            defaultGfxSize = 100
        cid <-
          GameQuery $
            fmap ((+ 1) . fromMaybe characterMinimumId . join) $
              runSelectReturningOne $
                select $
                  aggregate_ (max_ . view characterId) $
                    all_ $ gameDb ^. character
        let cp =
              CharacterPosition
                (CharacterPK cid)
                (MapPK 36)
                142

            cl =
              CharacterLook
                (CharacterPK cid)
                ( fromIntegral $
                    bi * 10
                      + if unCharacterSex sex then 1 else 0
                )
                defaultGfxSize
                sex
                c1
                c2
                c3
        void $
          createNewCharacter
            (acc ^. accountId)
            cid
            cn
            foundBreed
            cp
            cl
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

characterSelection ::
  Account ->
  EncodedCharacterId ->
  GameHandler GameState
characterSelection acc characterIdText = do
  either (const stay) go $ A.parseOnly (A.decimal @CharacterId) characterIdText
  where
    go :: CharacterId -> GameHandler GameState
    go cid = do
      characterLinked <-
        runVolatile @GameDbConn $ isAccountCharacter (acc ^. accountId) cid
      case characterLinked of
        True -> do
          loadedPc <- loadPlayerCharacter cid
          case loadedPc of
            Just pc -> do
              debug $ "Login player: " <> showText pc
              playerToClient <- view $ handlerInputServer . gameServerPlayerToClient
              client <- view handlerInputClient
              liftIO $ atomically $ M.insert client (pc ^. to actorId) playerToClient
              emit $ CharacterSelectionSuccess pc
              emit $ AccountRestrictions $ pc ^. playerCharacterRestrictions
              pure $ GameCreation acc (coerce cid)
            Nothing -> do
              warn $ "Could not load character: id: " <> showText characterIdText
              stay
        False ->
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
  playerToClient <- view $ handlerInputServer . gameServerPlayerToClient
  liftIO $ atomically $ M.delete aid playerToClient
  playerToMap <- view $ handlerInputServer . gameServerPlayerToMap
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
    GameCreation acc aid -> do
      onAccountDisconnected acc
      onPlayerDisconnected aid
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
    (GameCreation acc cid, 'G' :- 'C' :- _) -> do
      gameCreation acc cid
    (InGame _ aid, _) -> do
      case packet of
        'G' :- 'I' :- _ ->
          raiseMapEvent $
            MapEventDispatchInformations aid
        'G' :- 'A' :- a0 :- a1 :- a2 :- params ->
          bitraverse_
            debugShow
            raiseMapEvent
            $ gameActionStart aid (a0 :- a1 :- a2 :- mempty) params
        'G' :- 'K' :- 'K' :- eid ->
          bitraverse_
            debugShow
            raiseMapEvent
            $ gameActionAck aid eid
        'G' :- 'K' :- 'E' :- '1' :- '|' :- cid ->
          bitraverse_
            debugShow
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
    cell = view $ handlerInputClient . gameClientState

readState :: GameHandler GameState
readState = read =<< cell
  where
    read = liftIO . readIORef
    cell = view $ handlerInputClient . gameClientState

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

mkDispatchMessage :: M.Map ActorId GameClient -> IO (ActorId -> GameMessage -> IO ())
mkDispatchMessage playerToClient = do
  (i, o) <- UB.newChan @(ActorId :<*>: GameMessage)
  nbCap <- getNumCapabilities
  streams <- UB.streamChan nbCap o
  forM_ streams $ forkIO . go
  pure $ \aid msg -> UB.writeChan i (aid :<*>: msg)
  where
    go s = do
      e <- UB.tryReadNext s
      go =<< case e of
        Next (aid :<*>: msg) s' -> do
          client <- atomically $ M.lookup aid playerToClient
          traverse_ (`emitToClient` Identity msg) client
          pure s'
        Pending -> do
          gameDelay $ ms $ 1000 % 64
          pure s

loadMapControllers ::
  Pool AuthDbConn ->
  Pool GameDbConn ->
  IO
    ( M.Map ActorId GameClient
        :<*>: Compact ()
        :<*>: [MapController :<*>: MapEventChannels]
    )
loadMapControllers authDbPool gameDbPool = do
  compactStore <- compact ()

  let doCompact :: MonadIO m => a -> m a
      doCompact =
        fmap getCompact
          . liftIO
          . compactAddWithSharing compactStore

  (_, mapInstances) <-
    runReaderT
      ( do
          debug "Loading map templates"
          mapTemplates <-
            runVolatile @GameDbConn getMaps

          debug $
            "Map template loaded: "
              <> showText (length mapTemplates)

          debug "Loading interactive objects"
          interactiveObjects <-
            doCompact
              =<< runVolatile @GameDbConn getInteractiveObjectGfxIds

          debug "Creating map instances"
          partitionEithers
            <$> pooledMapConcurrently
              (createMapInstance interactiveObjects)
              mapTemplates
      )
      gameDbPool

  debug "Compacting instances"
  compactedMapInstances <-
    traverse doCompact mapInstances

  debug
    . ("Compact data size: " <>)
    . (<> "mb")
    . showText
    . (`quot` 1000000)
    =<< compactSize compactStore

  debug $
    "Map instances created: "
      <> showText (length compactedMapInstances)

  playerToClient <- M.newIO

  dispatchMessage <-
    mkDispatchMessage playerToClient

  gameDelay $ ms $ 10000
  debug "Creating map controllers"
  mapControllers <-
    traverse
      ( runMap
          authDbPool
          gameDbPool
          dispatchMessage
      )
      compactedMapInstances

  pure $ playerToClient :<*>: compactStore :<*>: mapControllers

loadDbPools :: IO (Pool AuthDbConn :<*>: Pool GameDbConn)
loadDbPools = do
  let makeConnInfo =
        ConnectInfo "localhost" 5432 "nerded" "nerded"
  authDbPool <- createConnPool 20 $ makeConnInfo "opendofus_auth"
  gameDbPool <- createConnPool 180 $ makeConnInfo "opendofus_game"
  pure $ authDbPool :<*>: gameDbPool

app :: IO ()
app = do
  authDbPool :<*>: gameDbPool <- loadDbPools

  playerToClient :<*>: compactStore :<*>: mapControllers <-
    loadMapControllers authDbPool gameDbPool

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
      <*> pure playerToClient
      <*> M.newIO
      <*> pure compactStore
  startServer server dispatchToHandler
  where
    controllerEntry t@(ctl :<*>: _) =
      (ctl ^. mapControllerInstance . mapInstanceTemplate . mapId, t)
    makeClient conn =
      GameClient <$> newIORef Greeting <*> pure conn

main :: IO ()
main = runApp $ do
  runApp app
