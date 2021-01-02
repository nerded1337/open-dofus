{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Monad.Random
import qualified Data.ByteString.Char8 as BS
import Data.Char (ord)
import OpenDofus.Auth.Network.Message
import OpenDofus.Auth.Server
  ( AuthClient (..),
    AuthServer (..),
    AuthState (..),
    authClientState,
  )
import OpenDofus.Core.Application
import OpenDofus.Core.Data.Constructible
import OpenDofus.Core.Game.Constant (gameProtocolVersion)
import OpenDofus.Core.Network.Server
  ( ClientHandler,
    ClientMessage (..),
    ClientPacket,
    HasHandlerInput (..),
    ServerState (..),
    emit,
    kick,
    startServer,
  )
import OpenDofus.Database
import OpenDofus.Prelude
import qualified StmContainers.Map as M

type Credentials = BS.ByteString

type HashedAccountPassword = BS.ByteString

type AuthHandler a = ClientHandler IO AuthServer a

hash :: BS.ByteString
hash = fromString $ ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['-', '_']

hashPassword :: Salt -> BS.ByteString -> BS.ByteString
hashPassword salt password = fold go
  where
    key = unSalt salt

    hl = BS.length hash

    project :: Int -> Int -> ByteString
    project pkey ppass =
      BS.singleton (BS.index hash an)
        <> BS.singleton (BS.index hash an')
      where
        apass = floor $ toRational ppass / 16
        akey = ppass `mod` 16
        an = (apass + pkey) `mod` hl
        an' = (akey + pkey) `mod` hl

    go =
      ( \i ->
          project
            (fromIntegral $ ord $ BS.index key i)
            (fromIntegral $ ord $ BS.index password i)
      )
        <$> [0 .. BS.length password - 1]

loginAccount ::
  Salt ->
  HashedAccountPassword ->
  Account ->
  AuthQuery (Either AuthFailureReason Account)
loginAccount salt hashedPassword acc
  | passwordIsValid && banned = pure $ Left AuthFailureBanned
  | passwordIsValid && not alreadyOnline = do
    setOnline True
    pure $ Right acc
  | passwordIsValid && alreadyOnline = do
    -- TODO: we should disconnect the client using the account
    setOnline False
    pure $ Left AuthFailureAlreadyConnected
  | otherwise = pure $ Left AuthFailureInvalidCredentials
  where
    setOnline = setAccountIsOnline (acc ^. accountId) . AccountIsOnline

    passwordIsValid =
      hashPassword
        salt
        (encodeTextStrict $ unAccountPassword $ acc ^. accountPassword)
        == hashedPassword

    banned = unAccountIsBanned $ acc ^. accountIsBanned

    alreadyOnline = unAccountIsOnline $ acc ^. accountIsOnline

checkLogin ::
  Salt ->
  AccountName ->
  HashedAccountPassword ->
  AuthQuery (Either AuthFailureReason Account)
checkLogin salt accName hashedPassword = do
  acc <- getAccountByName accName
  case acc of
    Just accFound -> loginAccount salt hashedPassword accFound
    Nothing -> pure $ Left AuthFailureInvalidCredentials

parseLogin ::
  [Credentials] ->
  Either AuthFailureReason (AccountName, HashedAccountPassword)
parseLogin [accName, '1' :- hashedPassword] =
  Right (AccountName $ decodeStrictByteString accName, hashedPassword)
parseLogin _ =
  Left AuthFailureInvalidCredentials

findWorld :: [WorldServer] -> BS.ByteString -> Maybe WorldServer
findWorld worlds providedWorldId = do
  actualWorldId <- readMaybe $ BS.unpack providedWorldId
  find (isWorldId actualWorldId) worlds
  where
    isWorldId i w = (w ^. worldServerId) == i

onWorldSelection ::
  Account ->
  [WorldServer] ->
  ClientPacket ->
  AuthHandler AuthState
onWorldSelection acc worlds selectedWorldId = do
  case findWorld worlds selectedWorldId of
    (Just world) -> do
      ticket <-
        runVolatile @AuthDbConn $ generateAccountTicket (acc ^. accountId)
      emit $ WorldSelectionSuccess (WorldServerEndpointInfo world) ticket
    Nothing ->
      emit WorldSelectionFailure
  stay

onCharactersList :: Account -> [WorldServer] -> AuthHandler AuthState
onCharactersList acc worlds = do
  remainingSub <-
    getAccountRemainingSubscriptionInMilliseconds $
      acc ^. accountSubscriptionExpirationDate
  emit $ WorldCharacterList (view worldServerId <$> worlds) remainingSub
  stay

onLogin :: Salt -> ClientPacket -> AuthHandler AuthState
onLogin salt credentials = do
  loginResult <-
    traverseCollapse
      (runSerializable @AuthDbConn . uncurry (checkLogin salt))
      (parseLogin (BS.split '#' (BS.filter (/= '\n') credentials)))
  case loginResult of
    Right acc -> do
      debug "Login account"
      worlds <- runVolatile @AuthDbConn getWorldServers
      emit $ AuthSuccess $ acc ^. accountIsAdmin
      emit $ AccountCurrentNickName $ acc ^. accountNickName
      emit $ WorldServerList $ WorldServerInfo <$> worlds
      pure $ LoggedIn acc worlds
    Left reason -> do
      emit $ AuthFailure reason
      pure Kicked

onProtocolRequired :: Salt -> ClientPacket -> AuthHandler AuthState
onProtocolRequired salt protocol
  | protocol == fromString (show gameProtocolVersion) =
    pure $ LoggingIn salt
  | otherwise = do
    emit $
      AuthFailure $
        AuthFailureInvalidProtocol $
          fromString $
            show gameProtocolVersion
    pure Kicked

onClientDisconnected :: Maybe Account -> AuthHandler AuthState
onClientDisconnected loggedAccount = do
  traverse_ setOffline loggedAccount
  stay
  where
    setOffline acc = do
      debug "Logout account"
      runVolatile @AuthDbConn $
        setAccountIsOnline (acc ^. accountId) $
          AccountIsOnline False

onClientConnected :: AuthHandler AuthState
onClientConnected = do
  salt <- evalRandTIO newSalt
  emit $ HelloConnect salt
  pure $ ProtocolRequired salt

stateHandler :: AuthState -> ClientMessage -> AuthHandler AuthState
stateHandler _ ClientConnected =
  onClientConnected
stateHandler (LoggedIn acc _) ClientDisconnected =
  onClientDisconnected (Just acc)
stateHandler _ ClientDisconnected =
  onClientDisconnected Nothing
stateHandler (ProtocolRequired salt) (ClientSent protocol) =
  onProtocolRequired salt protocol
stateHandler (LoggingIn salt) (ClientSent credentials) =
  onLogin salt credentials
stateHandler (LoggedIn acc worlds) (ClientSent packet) =
  case packet of
    ('A' :- 'x' :- _) ->
      onCharactersList acc worlds
    ('A' :- 'X' :- selectedWorldId) ->
      onWorldSelection acc worlds selectedWorldId
    _ -> stay
stateHandler Kicked _ = do
  kick
  stay
stateHandler _ _ =
  stay

writeState :: AuthState -> AuthHandler ()
writeState s = write =<< cell
  where
    write = liftIO . flip writeIORef s
    cell = view $ handlerInputClient . authClientState

readState :: AuthHandler AuthState
readState = read =<< cell
  where
    read = liftIO . readIORef
    cell = view $ handlerInputClient . authClientState

stay :: AuthHandler AuthState
stay = readState

dispatchToHandler :: AuthHandler ()
dispatchToHandler = writeState =<< transition =<< readStateAndMsg
  where
    transition = uncurry stateHandler
    readStateAndMsg = (,) <$> readState <*> currentMsg
    currentMsg = view handlerInputMessage

logMessage :: AuthHandler ()
logMessage = debug . showText =<< view handlerInputMessage

app :: IO ()
app = do
  authDbPool <-
    createConnPool $
      ConnectInfo
        "localhost"
        5432
        "nerded"
        "nerded"
        "opendofus_auth"
  -- gameDbPool <- liftIO $ createConnPool @GameDbConn $ ConnectInfo
  --   "localhost"
  --   5432
  --   "nerded"
  --   "nerded"
  --   "opendofus_game"
  -- runReaderT createAuthDb authDbPool
  -- runReaderT
  --   (do
  --     createGameDb
  --     populateGameDb "data/dofus"
  --   )
  --   gameDbPool
  debug "Starting auth server"
  server <-
    AuthServer
      <$> (ServerState 8080 makeClient <$> M.newIO)
      <*> pure authDbPool
  startServer server (logMessage *> dispatchToHandler)
  where
    makeClient conn = AuthClient <$> newIORef Greeting <*> pure conn

main :: IO ()
main = runApp app
