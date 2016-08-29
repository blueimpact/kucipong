
module Kucipong.Session
    ( -- * Session type
      Session(..)
      -- * Markers for either AdminSession or StoreSession
    , Admin
    , Store
      -- * Classes and functions dealing with Session Key
    , HasSessionKey(..)
    )
    where

import Kucipong.Prelude

import Web.ClientSession ( Key )

class HasSessionKey r where
    getSessionKey :: r -> Key

instance HasSessionKey Key where
    getSessionKey :: Key -> Key
    getSessionKey = id

-- | Raw session data.  This is used by 'AdminSession' and 'StoreSession'.
newtype RawSession = RawSession { unRawSession :: Text }
    deriving (Data, Eq, Generic, Show, Typeable)

-- | Tag for 'AdminSession'
data Admin

-- | Tag for 'StoreSession'.
data Store

data Session :: * -> * where
    AdminSession :: RawSession -> Session Admin
    StoreSession :: RawSession -> Session Store

createRawSession :: MonadIO m => m RawSession
createRawSession = undefined
