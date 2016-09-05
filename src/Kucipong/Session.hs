
module Kucipong.Session
    ( -- * Session type
      Session(..)
      -- * Markers for either AdminSession or StoreSession
    , Admin
    , Store
      -- * Helper functions for encrypting / decrypting a 'Session'
    , encryptSession
      -- * Classes and functions dealing with Session Key
    , HasSessionKey(..)
    )
    where

import Kucipong.Prelude

import "emailaddress" Text.Email.Validate ( toByteString )
import Web.ClientSession ( Key, encryptIO )

class HasSessionKey r where
    getSessionKey :: r -> Key

instance HasSessionKey Key where
    getSessionKey :: Key -> Key
    getSessionKey = id

-- | Tag for 'AdminSession'
data Admin

-- | Tag for 'StoreSession'.
data Store

data Session :: * -> * where
    AdminSession :: EmailAddress -> Session Admin
    StoreSession :: EmailAddress -> Session Store

encryptSession
    :: ( HasSessionKey r
       , MonadIO m
       , MonadReader r m
       )
    => Session sessionType -> m Text
encryptSession (AdminSession email) = encryptEmail email
encryptSession (StoreSession email) = encryptEmail email

encryptEmail
    :: ( HasSessionKey r
       , MonadIO m
       , MonadReader r m
       )
    => EmailAddress -> m Text
encryptEmail email = do
    key <- reader getSessionKey
    encryptedEmail <- liftIO . encryptIO key $ toByteString email
    pure $ decodeUtf8 encryptedEmail
