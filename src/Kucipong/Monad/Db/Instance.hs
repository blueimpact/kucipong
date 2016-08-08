{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Db.Instance where

import Kucipong.Prelude

import Control.Lens ( to, view )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.Trans.Control
    ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
    , defaultLiftBaseWith, defaultRestoreM )
import Crypto.PasswordStore ( verifyPassword )
import Database.Persist ( getBy, insert, entityKey, entityVal )

import Kucipong.Config ( Config )
import Kucipong.Db ( Admin, Key )
import Kucipong.Errors ( AppErr )
import Kucipong.Monad.Db.Class ( MonadKucipongDb(..) )
import Kucipong.Monad.Db.Trans ( KucipongDbT(..) )

instance ( MonadBaseControl IO m
         , MonadIO m
         , MonadError AppErr m
         , MonadReader Config m
         ) => MonadKucipongDb (KucipongDbT m) where

    dbInsertNewUser :: EmailAddress -> KucipongDbT m (Key Admin)
    dbInsertNewUser _ = KucipongDbT $ undefined
        -- lift go
      -- where
        -- go :: m (Key User)
        -- go = runDbSafeCurrTime $ \currentTime -> do
        --         let user = User
        --                     (CreatedTime currentTime)
        --                     (UpdatedTime currentTime)
        --                     Nothing
        --                     email
        --                     hash
        --         insert user

    dbLoginUser :: EmailAddress -> KucipongDbT m (Maybe (Key Admin))
    dbLoginUser _ = KucipongDbT $ undefined
        -- lift go
      -- where
        -- go :: m (Maybe (Key User))
        -- go = do
        --     maybeUserEntity <- runDb . getBy $ UniqueUserEmail email
        --     pure $ do
        --         userEntity <- maybeUserEntity
        --         let userKey = entityKey userEntity
        --             user = entityVal userEntity
        --             hash = view (userPasswordHash . passwordHash) user
        --         if verifyPassword (encodeUtf8 password) hash
        --             then Just userKey
        --             else Nothing
