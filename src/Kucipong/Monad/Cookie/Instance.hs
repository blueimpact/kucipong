{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Cookie.Instance where

import Kucipong.Prelude

import Kucipong.Monad.Cookie.Class ( MonadKucipongCookie(..) )
import Kucipong.Monad.Cookie.Trans ( KucipongCookieT(..) )
import Kucipong.Session ( HasSessionKey, Session, encryptSession )

instance
    ( HasSessionKey r
    , MonadIO m
    , MonadReader r m
    ) => MonadKucipongCookie (KucipongCookieT m) where

    encryptSessionCookie :: Session sessionType -> KucipongCookieT m Text
    encryptSessionCookie = lift . encryptSession
