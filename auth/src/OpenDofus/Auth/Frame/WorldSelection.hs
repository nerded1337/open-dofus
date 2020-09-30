-- WorldSelection.hs ---

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
{-# LANGUAGE TypeApplications #-}

module OpenDofus.Auth.Frame.WorldSelection where

import qualified Data.ByteString.Lazy.Char8    as BS
import qualified Data.Vector                   as V
import           OpenDofus.Auth.Network.Message
import           OpenDofus.Auth.Server
import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Prelude

findWorld :: V.Vector WorldServer -> BS.ByteString -> Maybe WorldServer
findWorld worlds providedWorldId = do
  actualWorldId <- readMaybe $ BS.unpack providedWorldId
  find (isWorldId actualWorldId) worlds
  where isWorldId i w = (w ^. worldServerId) == i

worldSelectionHandler :: V.Vector WorldServer -> Account -> AuthClientHandler
worldSelectionHandler worlds acc = MessageHandlerCont $ go =<< asks
  (view handlerInputMessage)
 where
  go (ClientSent ('A' :- ('X' :- providedWorldId))) = do
    case findWorld worlds providedWorldId of
      (Just worldFound) -> do
        ticket <- runVolatile @AuthDbConn
          $ generateAccountTicket (acc ^. accountId)
        sendMessage
          $ WorldSelectionSuccess (WorldServerEndpointInfo worldFound) ticket
        pure $ MessageHandlerDisconnect mempty
      Nothing -> do
        sendMessage WorldSelectionFailure
        pure $ MessageHandlerDisconnect mempty

  go (ClientSent ('A' :- ('x' :- _))) = do
    remainingSubscription <-
      getAccountRemainingSubscriptionInMilliseconds
      $  acc
      ^. accountSubscriptionExpirationDate
    sendMessage $ WorldCharacterList (view worldServerId <$> worlds)
                                     remainingSubscription
    pure $ worldSelectionHandler worlds acc

  go _ = do
    pure $ worldSelectionHandler worlds acc
