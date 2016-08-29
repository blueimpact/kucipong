{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Db.Instance where

import Kucipong.Prelude

import Control.Monad.Random ( MonadRandom(..) )
import Control.Monad.Time ( MonadTime(..) )
import Database.Persist ( Entity(..), (==.), insert, selectFirst )

import Kucipong.Config ( Config )
import Kucipong.Db
    ( Admin(..), AdminLoginToken(..), CreatedTime(..), EntityField(..), Key
    , LoginTokenExpirationTime(..), UpdatedTime(..), runDb )
import Kucipong.Errors ( AppErr )
import Kucipong.LoginToken ( LoginToken, createRandomLoginToken )
import Kucipong.Monad.Db.Class ( MonadKucipongDb(..) )
import Kucipong.Monad.Db.Trans ( KucipongDbT(..) )
import Kucipong.Util ( addOneDay )

instance ( MonadBaseControl IO m
         , MonadIO m
         , MonadError AppErr m
         , MonadRandom m
         , MonadReader Config m
         , MonadTime m
         ) => MonadKucipongDb (KucipongDbT m) where

    dbCreateAdmin :: EmailAddress -> Text -> KucipongDbT m (Entity Admin)
    dbCreateAdmin email name = lift go
      where
        go :: m (Entity Admin)
        go = do
            currTime <- currentTime
            let admin =
                    Admin email (CreatedTime currTime) (UpdatedTime currTime)
                        Nothing name
            adminKey <- runDb $ insert admin
            pure $ Entity adminKey admin

    dbCreateAdminMagicLoginToken :: Key Admin -> KucipongDbT m (Entity AdminLoginToken)
    dbCreateAdminMagicLoginToken adminKey = lift go
      where
        go :: m (Entity AdminLoginToken)
        go = do
            currTime <- currentTime
            randomLoginToken <- createRandomLoginToken
            let plusOneDay = addOneDay currTime
            let adminLoginToken =
                    AdminLoginToken adminKey (CreatedTime currTime)
                        (UpdatedTime currTime) Nothing randomLoginToken
                        (LoginTokenExpirationTime plusOneDay)
            adminLoginTokenKey <- runDb $ insert adminLoginToken
            pure $ Entity adminLoginTokenKey adminLoginToken

    dbFindAdminLoginToken :: LoginToken -> KucipongDbT m (Maybe (Entity AdminLoginToken))
    dbFindAdminLoginToken loginToken = lift go
      where
        go :: m (Maybe (Entity AdminLoginToken))
        go = runDb $ selectFirst [AdminLoginTokenLoginToken ==. loginToken] []
