
module Kucipong.LoginToken where

import Kucipong.Prelude

import Control.Monad.Random ( MonadRandom, getRandoms )
import Data.ByteString.Base64 ( encode )
import Database.Persist ( PersistField(..) )
import Database.Persist.Sql ( PersistFieldSql(..) )

newtype LoginToken = LoginToken { unLoginToken :: Text }
    deriving (Data, Eq, Generic, PersistField, PersistFieldSql, Show, Typeable)

createRandomLoginToken :: MonadRandom m => m LoginToken
createRandomLoginToken =
    LoginToken . decodeUtf8 . encode . pack . take 50 <$> getRandoms
