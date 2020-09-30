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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module OpenDofus.Auth.Frame.Authentication where

import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Char
import           OpenDofus.Auth.Frame.Logout
import           OpenDofus.Auth.Frame.WorldSelection
import           OpenDofus.Auth.Network.Message
import           OpenDofus.Auth.Server
import           OpenDofus.Core.Network.Client
import           OpenDofus.Core.Network.Server
import           OpenDofus.Data.Constructible
import           OpenDofus.Database
import           OpenDofus.Prelude

hash :: BS.ByteString
hash = fromString $ ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['-', '_']

cryptPassword :: Salt -> BS.ByteString -> BS.ByteString
cryptPassword !salt !password = fold go
 where
  !key = unSalt salt

  !hl  = BS.length hash

  project :: Int -> Int -> ByteString
  project !pkey !ppass = BS.singleton (BS.index hash an)
    <> BS.singleton (BS.index hash an')
   where
    !apass = floor $ toRational ppass / 16
    !akey  = ppass `mod` 16
    !an    = (apass + pkey) `mod` hl
    !an'   = (akey + pkey) `mod` hl

  go =
    (\(!i) -> project (fromIntegral $ ord $ BS.index key i)
                      (fromIntegral $ ord $ BS.index password i)
      )
      <$> [0 .. BS.length password - 1]

parseCredentials
  :: [BS.ByteString] -> Either AuthFailureReason (AccountName, BS.ByteString)
parseCredentials (providedAccountName : ('1' :- providedEncryptedPassword) : _)
  = Right (accName, providedEncryptedPassword)
  where accName = AccountName $ decodeStrictByteString providedAccountName
parseCredentials _ = Left AuthFailureInvalidCredentials

handleAuthResult
  :: (MonadIO m, MonadReader (HandlerInput AuthServer AuthClient) m)
  => Either AuthFailureReason Account
  -> m AuthClientHandler
handleAuthResult (Right !acc) = do
  worlds <- runVolatile @AuthDbConn getWorldServers
  sendMessages
    [ AccountCurrentNickName $ acc ^. accountNickName
    , AuthSuccess $ acc ^. accountIsAdmin
    , WorldServerList $ WorldServerInfo <$> worlds
    ]
  pure $ worldSelectionHandler worlds acc <> logoutHandler acc
handleAuthResult (Left !reason) = do
  sendMessage $ AuthFailure reason
  pure $ MessageHandlerDisconnect mempty

loginAccount
  :: Salt
  -> BS.ByteString
  -> Account
  -> AuthQuery (Either AuthFailureReason Account)
loginAccount !salt !encryptedPassword acc
  | passwordIsValid && banned = pure $ Left AuthFailureBanned
  | passwordIsValid && not alreadyOnline = do
    setIsOnline True
    pure $ Right acc
  | passwordIsValid && alreadyOnline = do
    setIsOnline False
    pure $ Left AuthFailureAlreadyConnected
  | otherwise = pure $ Left AuthFailureInvalidCredentials
 where
  setIsOnline :: Bool -> AuthQuery ()
  setIsOnline = setAccountIsOnline (acc ^. accountId) . AccountIsOnline

  passwordIsValid :: Bool
  !passwordIsValid =
    cryptPassword
        salt
        (encodeTextStrict $ unAccountPassword $ acc ^. accountPassword)
      == encryptedPassword

  banned :: Bool
  !banned = unAccountIsBanned (acc ^. accountIsBanned)

  alreadyOnline :: Bool
  !alreadyOnline = unAccountIsOnline (acc ^. accountIsOnline)

authenticationHandler :: Salt -> AuthClientHandler
authenticationHandler !salt = MessageHandlerCont $ go =<< asks
  (view handlerInputMessage)
 where
  go (ClientSent !credentials) = handleAuthResult =<< fmap
    join
    (traverse authenticate $ parseCredentials $ BS.split
      '#'
      (LBS.toStrict credentials)
    )
   where
    authenticate (!providedAccountName, !providedEncryptedPassword) =
      runSerializable @AuthDbConn $ do
        !acc <- getAccountByName providedAccountName
        maybe (pure (Left AuthFailureInvalidCredentials))
              (loginAccount salt providedEncryptedPassword)
              acc

  go _ = pure $ MessageHandlerDisconnect mempty
