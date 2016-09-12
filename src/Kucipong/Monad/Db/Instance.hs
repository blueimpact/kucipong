{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Db.Instance where

import Kucipong.Prelude

import Control.Lens ( (^.) )
import Control.Monad.Random ( MonadRandom(..) )
import Control.Monad.Time ( MonadTime(..) )
import Database.Persist
    ( Entity(..), (==.), (=.), get, insert, repsert, selectFirst, updateGet )

import Kucipong.Config ( Config )
import Kucipong.Db
    ( Admin(..), AdminLoginToken(..), CreatedTime(..), EntityField(..), Key(..)
    , LoginTokenExpirationTime(..), UpdatedTime(..), adminName, runDb
    , runDbCurrTime )
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
            let newAdminLoginTokenVal =
                    AdminLoginToken adminKey (CreatedTime currTime)
                        (UpdatedTime currTime) Nothing randomLoginToken
                        (LoginTokenExpirationTime plusOneDay)
            runDb $ repsert (AdminLoginTokenKey adminKey) newAdminLoginTokenVal
            pure $ Entity (AdminLoginTokenKey adminKey) newAdminLoginTokenVal

    dbFindAdminLoginToken :: LoginToken -> KucipongDbT m (Maybe (Entity AdminLoginToken))
    dbFindAdminLoginToken loginToken = lift go
      where
        go :: m (Maybe (Entity AdminLoginToken))
        go = runDb $ selectFirst [AdminLoginTokenLoginToken ==. loginToken] []

    dbUpsertAdmin :: EmailAddress -> Text -> KucipongDbT m (Entity Admin)
    dbUpsertAdmin email name = lift go
      where
        go :: m (Entity Admin)
        go = runDbCurrTime $ \currTime -> do
            maybeExistingAdminVal <- get (AdminKey email)
            case maybeExistingAdminVal of
                Just existingAdminVal -> do
                    -- admin already exists.  update the name if it is different
                    if (existingAdminVal ^. adminName /= name)
                        then do
                            newAdminVal <- updateGet (AdminKey email) [AdminName =. name]
                            pure $ Entity (AdminKey email) newAdminVal
                        else
                            pure $ Entity (AdminKey email) existingAdminVal
                Nothing -> do
                    -- couldn't find an existing admin, so we will create a new
                    -- one
                    let newAdminVal = Admin email (CreatedTime currTime)
                            (UpdatedTime currTime) Nothing name
                    newAdminKey <- insert newAdminVal
                    pure $ Entity newAdminKey newAdminVal
