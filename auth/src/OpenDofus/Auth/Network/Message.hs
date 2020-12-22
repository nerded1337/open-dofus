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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenDofus.Auth.Network.Message
  ( AuthMessage (..),
    AuthFailureReason (..),
    WorldServerInfo (..),
    WorldServerEndpointInfo (..),
    Salt (..),
    newSalt,
  )
where

import Control.Monad.Random
  ( Monad,
    MonadRandom (..),
    RandT,
    RandomGen,
    replicateM,
  )
import Data.ByteString.Builder as BS
  ( byteString,
    intDec,
    stringUtf8,
    word32Dec,
  )
import Data.ByteString.Char8 as BS (ByteString, singleton)
import OpenDofus.Core.Network.Client
  ( FoldNetwork (FoldNetwork),
    ToNetwork (..),
  )
import OpenDofus.Database.Auth.Account
  ( AccountIsAdmin (..),
    AccountNickName (..),
    AccountRemainingSubscriptionInMilliseconds,
    AccountTicket,
    accountTicketId,
  )
import OpenDofus.Database.Auth.WorldServer
  ( WorldId (..),
    WorldServer,
    WorldServerT (..),
    WorldStatus (..),
  )
import OpenDofus.Prelude

data AuthFailureReason
  = AuthFailureInvalidProtocol {-# UNPACK #-} !BS.ByteString
  | AuthFailureInvalidCredentials
  | AuthFailureBanned
  | AuthFailureAlreadyConnected
  | AuthFailureServerBusy
  deriving Show

data AuthMessage
  = HelloConnect {-# UNPACK #-} !Salt
  | AuthFailure !AuthFailureReason
  | AuthSuccess !AccountIsAdmin
  | AccountCurrentNickName {-# UNPACK #-} !AccountNickName
  | WorldServerList ![WorldServerInfo]
  | WorldCharacterList
      ![WorldId]
      {-# UNPACK #-} !AccountRemainingSubscriptionInMilliseconds
  | WorldSelectionSuccess
      {-# UNPACK #-} !WorldServerEndpointInfo
      {-# UNPACK #-} !AccountTicket
  | WorldSelectionFailure
  | BasicNoOperation
  deriving Show

instance ToNetwork AuthMessage where
  toNetwork (HelloConnect salt) = "HC" <> byteString (unSalt salt)
  toNetwork (AuthFailure reason) = "AlE" <> go reason
    where
      go (AuthFailureInvalidProtocol requiredVersion) =
        "v" <> byteString requiredVersion
      go AuthFailureInvalidCredentials = "f"
      go AuthFailureBanned = "b"
      go AuthFailureAlreadyConnected = "c"
      go AuthFailureServerBusy = "w"
  toNetwork (AuthSuccess (AccountIsAdmin b)) = "AlK" <> bool "0" "1" b
  toNetwork (WorldServerList worlds) = "AH" <> toNetwork (FoldNetwork worlds)
  toNetwork (AccountCurrentNickName nickName) =
    "Ad" <> byteString (encodeTextStrict (unAccountNickName nickName))
  toNetwork (WorldCharacterList worlds subscription) =
    "AxK"
      <> stringUtf8 (show subscription)
      <> foldMap (("|" <>) . (<> ",1") . stringUtf8 . show) worlds
  toNetwork (WorldSelectionSuccess endpoint ticket) =
    "AYK" <> toNetwork endpoint <> ";"
      <> stringUtf8
        (show $ ticket ^. accountTicketId)
  toNetwork WorldSelectionFailure = "AXEd"
  toNetwork BasicNoOperation = "BN"
  {-# INLINE toNetwork #-}

newtype Salt = Salt
  { unSalt :: BS.ByteString
  }
  deriving (Show)

newtype WorldServerInfo = WorldServerInfo
  { unWorldServerInfo :: WorldServer
  }
  deriving (Show)

instance ToNetwork WorldServerInfo where
  toNetwork (WorldServerInfo (WorldServer i c s _ _)) =
    "|"
      <> word32Dec (unWorldId i)
      <> ";"
      <> intDec (fromEnum s)
      <> ";"
      <> intDec (fromEnum c)
      <> ";"
      <> canLogin s
    where
      canLogin WorldStatusOnline = "1"
      canLogin _ = "0"
  {-# INLINE toNetwork #-}

newtype WorldServerEndpointInfo = WorldServerEndpointInfo
  { unWorldServerEndpointInfo :: WorldServer
  } deriving newtype Show

instance ToNetwork WorldServerEndpointInfo where
  toNetwork (WorldServerEndpointInfo (WorldServer _ _ _ i p)) =
    byteString (encodeTextStrict $ unIP i) <> ":" <> stringUtf8 (show p)
  {-# INLINE toNetwork #-}

newSalt :: (RandomGen a, Monad m) => RandT a m Salt
newSalt =
  Salt . foldMap BS.singleton
    <$> replicateM
      ticketLength
      (getRandomR ('A', 'z'))
  where
    ticketLength = 32
{-# INLINE newSalt #-}
