{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Main
  ( main,
  )
where

import OpenDofus.Core.Application
import OpenDofus.Database
import OpenDofus.Prelude

main :: IO ()
main = runApp $ do
  authDbPool <-
    createConnPool @AuthDbConn
      1
      ( ConnectInfo
          "localhost"
          5432
          "nerded"
          "nerded"
          "opendofus_auth"
      )
  gameDbPool <-
    liftIO $
      createConnPool @GameDbConn
        1
        ( ConnectInfo
            "localhost"
            5432
            "nerded"
            "nerded"
            "opendofus_game"
        )

  runReaderT
    ( do
        result <- checkDbSchemas @AuthDb authDbChecked
        case result of
          VerificationSucceeded -> do
            debug "Auth database is up to date."
          VerificationFailed x -> do
            traverse_ warnShow x
            warn "Auth database should be updated..."
            void bringAuthDbUpToDate
    )
    authDbPool
  runReaderT
    ( do
        result <- checkDbSchemas @GameDb gameDbChecked
        case result of
          VerificationSucceeded -> do
            debug "Game database is up to date."
          VerificationFailed x -> do
            traverse_ warnShow x
            warn "Game database should be updated..."
            void bringGameDbUpToDate
            populateGameDb "data/dofus"
    )
    gameDbPool
