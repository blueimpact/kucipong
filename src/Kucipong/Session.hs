
module Kucipong.Session
  ( -- * Session type
    Session(..)
    -- * Markers for either AdminSession or StoreSession
  , SessionType(..)
    -- * Helper functions for encrypting / decrypting a 'Session'
  , decryptAdminSession
  , decryptStoreSession
  , encryptSession
    -- * Classes and functions dealing with Session Key
  , HasSessionKey(..)
  )
  where

import Kucipong.Prelude

import Text.EmailAddress (emailAddressFromText, toText)
import Web.ClientSession (IV, Key, decrypt, encrypt, randomIV)

-- $setup
--
-- >>> import Control.FromSum (fromEither)
-- >>> import Data.ByteString.Base64 (decode)
-- >>> import Web.ClientSession (initKey, mkIV)
--
-- Create a @testKey@ that can be used in tests:
--
-- >>> let rawTestKey = "lCNWb2gFVE8QtvV+dqjmYMWK6aq1Y9vQ5PmJb0ZMiZ5AG6G9zp+bJY8aficESqo+uX+UEbhQN5dUqQXSEk0H8F/FGLUGywKCvnw8e7UcPx5rgK7xCdeGLJXm8R4B2ihK"
-- >>> let eitherTestKey = initKey =<< decode rawTestKey
-- >>> let testKey = fromEither (error "bad test key") eitherTestKey
--
-- Also create a @testIV@ that can be used in tests:
--
-- >>> let rawTestIV = "\208\228\130\ESC\212\f\155a\DC1\141\158\204\163\SI\184\225"
-- >>> let maybeTestIV = mkIV rawTestIV
-- >>> let testIV = fromMaybe (error "bad test iv") maybeTestIV

class HasSessionKey r where
  getSessionKey :: r -> Key

instance HasSessionKey Key where
  getSessionKey :: Key -> Key
  getSessionKey = id

-- | Tag for 'Session' types.
data SessionType = SessionTypeAdmin | SessionTypeStore

data Session :: SessionType -> * where
  AdminSession :: EmailAddress -> Session 'SessionTypeAdmin
  StoreSessionRaw :: Int64 -> Session 'SessionTypeStore

deriving instance Show (Session a)

encryptSession
  :: ( HasSessionKey r
     , MonadIO m
     , MonadReader r m
     )
  => Session sessionType -> m Text
encryptSession (AdminSession email) = encryptText $ toText email
encryptSession (StoreSessionRaw storeKey) = encryptText $ tshow storeKey

encryptText
  :: ( HasSessionKey r
     , MonadIO m
     , MonadReader r m
     )
  => Text -> m Text
encryptText text = liftIO randomIV >>= encryptTextWithIV text

-- | Encrypt a given 'Text' with an 'IV'.
--
-- >>> encryptTextWithIV "hello" testIV testKey
-- "w9U4hpRrNbOVJwVNIC1eOsl5FoGopw9tGOzzEPQsgS/Q5IIb1AybYRGNnsyjD7jhADUg+xg="
encryptTextWithIV
  :: ( HasSessionKey r
     , MonadReader r m
     )
  => Text -> IV -> m Text
encryptTextWithIV text iv = do
  key <- reader getSessionKey
  let encryptedByteString = encrypt key iv $ encodeUtf8 text
  pure $ decodeUtf8 encryptedByteString

-- | Decrypt an input 'Text' from an 'Admin' user's coookie representing a
-- @'Session' 'Admin'@.
--
-- >>> let encryptedSession = encryptTextWithIV "foo@bar.com" testIV testKey
-- >>> decryptAdminSession encryptedSession testKey
-- Just (AdminSession "foo@bar.com")
decryptAdminSession
  :: ( HasSessionKey r
     , MonadReader r m
     )
  => Text -> m (Maybe (Session 'SessionTypeAdmin))
decryptAdminSession =
  decryptSessionGeneric $ Just . AdminSession <=< emailAddressFromText

-- | Decrypt an input 'Text' from an 'Store' user's coookie representing a
-- @'Session' 'Store'@.
--
-- >>> let encryptedSession = encryptTextWithIV "13" testIV testKey
-- >>> decryptStoreSession encryptedSession testKey
-- Just (StoreSessionRaw 13)
decryptStoreSession
  :: ( HasSessionKey r
     , MonadReader r m
     )
  => Text -> m (Maybe (Session 'SessionTypeStore))
decryptStoreSession = decryptSessionGeneric $ Just . StoreSessionRaw <=< readMay

decryptSessionGeneric
  :: ( HasSessionKey r
     , MonadReader r m
     )
  => (Text -> Maybe (Session sessionType))
  -> Text
  -> m (Maybe (Session sessionType))
decryptSessionGeneric createSessionF encryptedSession = do
  key <- reader getSessionKey
  pure $
    decrypt key (encodeUtf8 encryptedSession) >>=
    pure . decodeUtf8 >>=
    createSessionF
