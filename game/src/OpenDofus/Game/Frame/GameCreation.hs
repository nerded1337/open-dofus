-- GameCreation.hs ---

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

module OpenDofus.Game.Frame.GameCreation
  ( gameCreationHandler
  )
where

import           OpenDofus.Core.Network.Server
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Game.Frame.Map
import           OpenDofus.Game.Map
import           OpenDofus.Game.Network.Message
import           OpenDofus.Game.Server
import           OpenDofus.Prelude

gameCreationHandler :: PlayerCharacter GameClient -> GameClientHandler
gameCreationHandler pc = MessageHandlerCont $ go =<< asks
  (view handlerInputMessage)
 where
  go (ClientSent ('G' :- 'C' :- _)) = do
    let mid = pc ^. playerCharacterCharacterPosition . characterPositionMapId
    mapCtls <- asks (view (handlerInputServer . gameServerMaps))
    case mapCtls ^. at mid of
      Just mapCtl -> do
        let gameDataMapMsg =
              GameDataMap
                  (mapCtl ^. mapControllerMap . mapInstanceTemplate . mapId)
                  (  mapCtl
                  ^. mapControllerMap
                  .  mapInstanceTemplate
                  .  mapCreationDate
                  )
                <$> (  mapCtl
                    ^. mapControllerMap
                    .  mapInstanceTemplate
                    .  mapDataKey
                    )
        case gameDataMapMsg of
          Just msg -> do
            pcRef <- traverse (newIORef . Just) pc
            raiseMapEvent mapCtl $ MapEventSpawnActor $ GameActorPC pcRef
            sendMessages [GameCreationSuccess, msg, GameDataSuccess]
            pure $ mapHandler pcRef mapCtl
          Nothing -> pure $ MessageHandlerDisconnect mempty
      Nothing -> pure $ MessageHandlerDisconnect mempty
  go _ = pure $ gameCreationHandler pc
