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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module OpenDofus.Game.Frame.Authentication where

import           Data.UUID
import           OpenDofus.Core.Network.Server
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Game.Frame.CharacterSelection
import           OpenDofus.Game.Network.Message
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

ticketTimeout :: NominalDiffTime
ticketTimeout = 5

authenticationHandler :: GameClientHandler
authenticationHandler = MessageHandlerCont $ go =<< asks (view handlerInputMessage)
  where
    go (ClientSent ('A' :- ('T' :- providedTicket))) = do
      let ticketUUID = fromLazyASCIIBytes providedTicket
      ticket <-
        traverseCollapse (runVolatile @AuthDbConn . getAccountTicket)
        $ AccountTicketId <$> ticketUUID
      case ticket of
        Just existingTicket -> do
          now <- getCurrentTime
          if (diffUTCTime
              now
               (unAccountTicketCreationDate $
                 existingTicket ^. accountTicketCreationDate)) <
             ticketTimeout
          then do
            sendMessage AccountTicketIsValid
            pure characterSelectionHandler
          else do
            sendMessage AccountTicketIsInvalid
            pure MessageHandlerDisconnect
        Nothing -> do
          pure MessageHandlerDisconnect
    go _ = do
      pure MessageHandlerDisconnect
