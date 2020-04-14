-- Logout.hs ---

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

module OpenDofus.Game.Frame.Logout where

import           OpenDofus.Core.Network.Client
import           OpenDofus.Database
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

logoutHandler :: Account -> GameClientHandler
logoutHandler acc = MessageHandlerCont $ go =<< asks (view handlerInputMessage)
  where
    go ClientDisconnected = do
      runSerializable @AuthDbConn $
        setAccountIsOnline (acc ^. accountId) (AccountIsOnline False)
      pure $ MessageHandlerDisconnect $ pure mempty
    go _ = pure $ logoutHandler acc
