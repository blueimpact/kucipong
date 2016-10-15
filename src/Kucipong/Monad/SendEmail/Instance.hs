{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.SendEmail.Instance where

import Kucipong.Prelude

import Kucipong.Email ( HasHailgunContext )
import qualified Kucipong.Email as Email
import Kucipong.Errors ( AppErr )
import Kucipong.Host ( HasHost, HasProtocol )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad.SendEmail.Class ( MonadKucipongSendEmail(..) )
import Kucipong.Monad.SendEmail.Trans ( KucipongSendEmailT(..) )

instance
    ( HasHailgunContext r
    , HasHost r
    , HasProtocol r
    , MonadError AppErr m
    , MonadIO m
    , MonadReader r m
    ) => MonadKucipongSendEmail (KucipongSendEmailT m) where

    sendAdminLoginEmail :: EmailAddress -> LoginToken -> KucipongSendEmailT m ()
    sendAdminLoginEmail = (void .) . Email.sendAdminLoginEmail

    sendStoreLoginEmail :: EmailAddress -> LoginToken -> KucipongSendEmailT m ()
    sendStoreLoginEmail = (void .) . Email.sendStoreLoginEmail
