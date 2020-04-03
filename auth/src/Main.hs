-- Main.hs ---

-- Copyright (C) 2019 Nerd Ed

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

module Main where

import qualified Data.ByteString.Lazy.Char8        as BS
import           Database.Beam.Postgres
import           OpenDofus.Auth.Frame.HelloConnect
import           OpenDofus.Auth.Server
import           OpenDofus.Core.Network.Server
import           OpenDofus.Database
import           OpenDofus.Prelude

loggingHandler :: AuthClientHandler
loggingHandler = MessageHandlerCont go
  where
    go = do
      m <- asks (view handlerInputMessage)
      liftIO $ BS.putStrLn $ BS.pack $ show m
      pure loggingHandler

app :: RIO OpenDofusApp ()
app = do
  authDbPool <-
    liftIO $ createConnPool $
    ConnectInfo "localhost" 5432 "nerded" "nerded" "opendofus_auth"
      -- gameDbConnInfo =
      --   GameDbConn $
      --   ConnectInfo "localhost" 5432 "nerded" "nerded" "opendofus_game"
  -- runReaderT createAuthDb authDbConnInfo
  -- runReaderT
  --   (do createGameDb
  --       populateGameDb "data/dofus")
  --   gameDbConnInfo
  serv <- AuthServer <$> (mkServer 8080 1000 128 mkClient) <*> pure authDbPool
  logInfo "Starting authentication server"
  result <- startServer serv (loggingHandler <> helloConnectHandler)
  case result of
    Right _  -> logInfo "Server shutdown successfully"
    Left err -> logWarn $ "Failed to start server: " <> displayShow err

main :: IO ()
main = runOpenDofusApp False app
