-- Authentication.hs ---

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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module OpenDofus.Game.Frame.Authentication
  ( authenticationHandler
  ) where

import           Data.UUID
import           OpenDofus.Core.Network.Server
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Game.Frame.CharacterSelection
import           OpenDofus.Game.Frame.Logout
import           OpenDofus.Game.Network.Message
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

ticketTimeOut :: NominalDiffTime
ticketTimeOut = 5

ticketHasTimedOut :: UTCTime -> AccountTicketCreationDate -> Bool
ticketHasTimedOut now tickDate =
  diffUTCTime now (unAccountTicketCreationDate tickDate) >= ticketTimeOut

authenticateWithTicket ::
     UTCTime -> AccountTicketId -> AuthQuery (Maybe Account)
authenticateWithTicket now tickId = go =<< getAccountByTicket tickId
  where
    go (Just (tick, acc))
      | not (ticketHasTimedOut now (tick ^. accountTicketCreationDate)) &&
          not (alreadyOnline acc) = do
        setIsOnline acc True
        pure $ Just acc
    go (Just (tick, acc))
      | not (ticketHasTimedOut now (tick ^. accountTicketCreationDate)) &&
          alreadyOnline acc = do
        setIsOnline acc False
        pure Nothing
    go _ = pure Nothing
    setIsOnline :: Account -> Bool -> AuthQuery ()
    setIsOnline acc = setAccountIsOnline (acc ^. accountId) . AccountIsOnline
    alreadyOnline :: Account -> Bool
    alreadyOnline acc = unAccountIsOnline (acc ^. accountIsOnline)

authenticationHandler :: GameClientHandler
authenticationHandler = MessageHandlerCont $ go =<< asks (view handlerInputMessage)
  where
    go (ClientSent ('A' :- ('T' :- providedTicket))) = do
      let ticketUUID = fromLazyASCIIBytes providedTicket
      now <- getCurrentTime
      result <-
        traverseCollapse (runSerializable @AuthDbConn . authenticateWithTicket now)
        $ AccountTicketId <$> ticketUUID
      case result of
        Just acc -> do
          sendMessage AccountTicketIsValid
          pure $ characterSelectionHandler acc <> logoutHandler acc
        Nothing -> do
          sendMessage AccountTicketIsInvalid
          pure $ MessageHandlerDisconnect mempty
    go _ = do
      sendMessage AccountTicketIsInvalid
      pure $ MessageHandlerDisconnect mempty
