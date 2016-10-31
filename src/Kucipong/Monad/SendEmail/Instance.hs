{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.SendEmail.Instance where

import Kucipong.Prelude

import Kucipong.Email ( EmailError, HasHailgunContext )
import qualified Kucipong.Email as Email
import Kucipong.Host ( HasHost, HasProtocol )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad.SendEmail.Class ( MonadKucipongSendEmail(..) )
import Kucipong.Monad.SendEmail.Trans ( KucipongSendEmailT(..) )

instance
    ( HasHailgunContext r
    , HasHost r
    , HasProtocol r
    , MonadIO m
    , MonadReader r m
    ) => MonadKucipongSendEmail (KucipongSendEmailT m) where

    sendAdminLoginEmail :: EmailAddress
                        -> LoginToken
                        -> KucipongSendEmailT m (Maybe EmailError)
    sendAdminLoginEmail email loginToken =
      either Just (const Nothing) <$> Email.sendAdminLoginEmail email loginToken

    sendStoreLoginEmail :: EmailAddress
                        -> LoginToken
                        -> KucipongSendEmailT m (Maybe EmailError)
    sendStoreLoginEmail email loginToken =
      either Just (const Nothing) <$> Email.sendStoreLoginEmail email loginToken
