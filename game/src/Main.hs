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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8     as BS
import           Database.Beam.Postgres
import           OpenDofus.Core.Network.Server
import           OpenDofus.Database
import           OpenDofus.Game.Frame.HelloGame
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

loggingHandler :: GameClientHandler
loggingHandler = MessageHandlerCont go
  where
    go = do
      m <- asks (view handlerInputMessage)
      liftIO $ BS.putStrLn $ BS.pack $ show m
      pure loggingHandler

app :: RIO OpenDofusApp ()
app = do
  authDbPool <-
    createConnPool $
    ConnectInfo "localhost" 5432 "nerded" "nerded" "opendofus_auth"
  gameDbPool <-
    createConnPool $
    ConnectInfo "localhost" 5432 "nerded" "nerded" "opendofus_game"
  serv <-
    GameServer <$> (mkServer 8081 1000 128 mkClient) <*> pure authDbPool <*>
    pure gameDbPool
  logInfo "Starting game server"
  result <- startServer serv (loggingHandler <> helloGameHandler)
  case result of
    Right _ -> logInfo "Server shutdown successfully"
    Left err -> logWarn $ "Failed to start server: " <> displayShow err

main :: IO ()
main = runOpenDofusApp False app
