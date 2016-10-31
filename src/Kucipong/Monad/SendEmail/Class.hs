{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Kucipong.Monad.SendEmail.Class where

import Kucipong.Prelude

import Control.Monad.Trans ( MonadTrans )
import Web.Spock ( ActionCtxT )

import Kucipong.Email (EmailError)
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad.Cookie.Trans ( KucipongCookieT )
import Kucipong.Monad.Db.Trans ( KucipongDbT )

-- |
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongSendEmail m where
    sendAdminLoginEmail
        :: EmailAddress
        -> LoginToken
        -> m (Maybe EmailError)
    default sendAdminLoginEmail
        :: ( MonadKucipongSendEmail n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> LoginToken -> t n (Maybe EmailError)
    sendAdminLoginEmail = (lift .) . sendAdminLoginEmail

    sendStoreLoginEmail
        :: EmailAddress
        -> LoginToken
        -> m (Maybe EmailError)
    default sendStoreLoginEmail
        :: ( MonadKucipongSendEmail n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> LoginToken -> t n (Maybe EmailError)
    sendStoreLoginEmail = (lift .) . sendStoreLoginEmail

instance MonadKucipongSendEmail m => MonadKucipongSendEmail (ActionCtxT ctx m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (ExceptT e m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (IdentityT m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (KucipongCookieT m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (KucipongDbT m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (ReaderT r m)
