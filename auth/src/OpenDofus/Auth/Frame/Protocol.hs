-- Protocol.hs ---

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

module OpenDofus.Auth.Frame.Protocol where

import           OpenDofus.Auth.Frame.Authentication
import           OpenDofus.Auth.Network.Message
import           OpenDofus.Auth.Server
import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server
import           OpenDofus.Game.Constant
import           OpenDofus.Prelude
import qualified RIO.ByteString.Lazy                 as BS

isProtocolValid :: BS.ByteString -> Bool
isProtocolValid protocol = protocol == fromString (show gameProtocolVersion)

protocolHandler :: Salt -> AuthClientHandler
protocolHandler salt = MessageHandlerCont $ go =<< asks (view handlerInputMessage)
  where
    go (ClientSent protocol)
      | isProtocolValid protocol = pure $ authenticationHandler salt
      | otherwise = do
        sendMessage
          (AuthFailure
             (AuthFailureInvalidProtocol $ fromString $ show gameProtocolVersion))
        pure MessageHandlerDisconnect
    go _ = pure MessageHandlerDisconnect
