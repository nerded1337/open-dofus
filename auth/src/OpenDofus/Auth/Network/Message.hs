-- Message.hs ---

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

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module OpenDofus.Auth.Network.Message
  ( AuthMessage(..)
  , AuthFailureReason(..)
  , WorldServerInfo(..)
  , WorldServerEndpointInfo(..)
  , Salt(unSalt)
  , newSalt
  )
where

import           Control.Monad.Random
import           Data.ByteString.Builder       as BS
import           Data.ByteString.Char8         as BS
import qualified Data.Vector                   as V
import           OpenDofus.Core.Network.Client
import           OpenDofus.Database.Auth.Account
import           OpenDofus.Database.Auth.WorldServer
import           OpenDofus.Prelude

newtype Salt =
  Salt
    { unSalt :: BS.ByteString
    }

newtype WorldServerInfo =
  WorldServerInfo
    { unWorldServerInfo :: WorldServer
    }

instance ToNetwork WorldServerInfo where
  toNetwork (WorldServerInfo (WorldServer !i !c !s _ _)) =
    "|"
      <> intDec (unWorldId i)
      <> ";"
      <> intDec (fromEnum s)
      <> ";"
      <> intDec (fromEnum c)
      <> ";"
      <> canLogin s
   where
    canLogin WorldStatusOnline = "1"
    canLogin _                 = "0"

newtype WorldServerEndpointInfo =
  WorldServerEndpointInfo
    { unWorldServerEndpointInfo :: WorldServer
    }

instance ToNetwork WorldServerEndpointInfo where
  toNetwork (WorldServerEndpointInfo (WorldServer _ _ _ !i !p)) =
    byteString (encodeTextStrict $ unIP i) <> ":" <> stringUtf8 (show p)

data AuthFailureReason = AuthFailureInvalidProtocol !BS.ByteString
    | AuthFailureInvalidCredentials
    | AuthFailureBanned
    | AuthFailureAlreadyConnected
    | AuthFailureServerBusy

data AuthMessage = HelloConnect !Salt
    | AuthFailure !AuthFailureReason
    | AuthSuccess !AccountIsAdmin
    | AccountCurrentNickName !AccountNickName
    | WorldServerList !(V.Vector WorldServerInfo)
    | WorldCharacterList !(V.Vector WorldId)
                     !AccountRemainingSubscriptionInMilliseconds
    | WorldSelectionSuccess !WorldServerEndpointInfo !AccountTicket
    | WorldSelectionFailure

instance ToNetwork AuthMessage where
  toNetwork (HelloConnect !salt  ) = "HC" <> byteString (unSalt salt)

  toNetwork (AuthFailure  !reason) = "AlE" <> go reason
   where
    go (AuthFailureInvalidProtocol !requiredVersion) =
      "v" <> byteString requiredVersion
    go AuthFailureInvalidCredentials = "f"
    go AuthFailureBanned             = "b"
    go AuthFailureAlreadyConnected   = "c"
    go AuthFailureServerBusy         = "w"

  toNetwork (AuthSuccess (AccountIsAdmin !b)) = "AlK" <> bool "0" "1" b

  toNetwork (WorldServerList !worlds) = "AH" <> toNetwork (FoldNetwork worlds)

  toNetwork (AccountCurrentNickName !nickName) =
    "Ad" <> byteString (encodeTextStrict (unAccountNickName nickName))

  toNetwork (WorldCharacterList !worlds !subscription) =
    "AxK"
      <> stringUtf8 (show subscription)
      <> foldMap (("|" <>) . (<> ",1") . stringUtf8 . show) worlds

  toNetwork (WorldSelectionSuccess !endpoint !ticket) =
    "AYK" <> toNetwork endpoint <> ";" <> stringUtf8
      (show $ ticket ^. accountTicketId)

  toNetwork WorldSelectionFailure = "AXEd"

newSalt :: (RandomGen a, Monad m) => RandT a m Salt
newSalt = Salt . foldMap BS.singleton <$> replicateM ticketLength
                                                     (getRandomR ('A', 'z'))
  where !ticketLength = 32
