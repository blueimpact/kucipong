{-# LANGUAGE DefaultSignatures #-}

module Kucipong.Monad.SendEmail.Class where

import Kucipong.Prelude

import Control.Monad.Trans ( MonadTrans )
import Web.Spock ( ActionCtxT )

import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad.Db.Trans ( KucipongDbT )

-- |
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongSendEmail m where
    sendAdminLoginEmail
        :: EmailAddress
        -> LoginToken
        -> m ()
    default sendAdminLoginEmail
        :: ( Monad (t n)
           , MonadKucipongSendEmail n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> LoginToken -> t n ()
    sendAdminLoginEmail = (lift .) . sendAdminLoginEmail

instance MonadKucipongSendEmail m => MonadKucipongSendEmail (ExceptT e m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (IdentityT m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (KucipongDbT m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (ReaderT r m)
instance MonadKucipongSendEmail m => MonadKucipongSendEmail (ActionCtxT ctx m)
