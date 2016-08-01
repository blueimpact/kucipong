{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Db.Models.Base where

import Kucipong.Prelude

import Control.Lens.TH ( makeLenses )
import Data.Aeson ( FromJSON, ToJSON )
import Database.Persist ( PersistField )
import Database.Persist.Sql ( PersistFieldSql )

--------------
-- Password --
--------------

newtype Password = Password { unPassword :: Text }
    deriving
        ( Data, Eq, FromJSON, Generic, IsString, Ord, Show, ToJSON, Typeable
        )

-------------------
-- Password Hash --
-------------------

newtype PasswordHash = PasswordHash { _passwordHash :: ByteString }
    deriving
        ( Data, Eq, Generic, IsString, Ord, PersistField, PersistFieldSql, Show
        , Typeable
        )
makeLenses ''PasswordHash

------------------------------------
-- Created, modified, delete time --
------------------------------------

newtype CreatedTime = CreatedTime { unCreatedTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

newtype UpdatedTime = UpdatedTime { unUpdatedTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

newtype DeletedTime = DeletedTime { unDeletedTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )
