
module Kucipong.Handler.Admin where

import Kucipong.Prelude

import Control.Lens ( (^.) )
import Control.Monad.Time ( MonadTime(..) )
import Database.Persist ( Entity(..) )
import Network.HTTP.Types ( forbidden403 )
import Web.Spock
    ( ActionCtxT, Path, SpockCtxT, (<//>), get, html, root, runSpock
    , setStatus, spockT, text, var )

import Kucipong.Db ( LoginTokenExpirationTime(..), adminLoginTokenExpirationTime )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad ( MonadKucipongDb(..), MonadKucipongSendEmail )
import Kucipong.Util ( fromMaybeM )

login
    :: forall ctx m
     . ( MonadIO m
       , MonadKucipongDb m
       , MonadTime m
       )
    => LoginToken -> ActionCtxT ctx m ()
login loginToken = do
    maybeAdminLoginTokenEntity <- dbFindAdminLoginToken loginToken
    (Entity _ adminLoginToken) <-
        fromMaybeM noAdminLoginTokenError maybeAdminLoginTokenEntity
    -- check date on admin login token
    now <- currentTime
    let (LoginTokenExpirationTime expirationTime) =
            adminLoginToken ^. adminLoginTokenExpirationTime
    when (now > expirationTime) tokenExpiredError
    -- TODO: Set the cookie.
    html "<p>okay</p>"
    -- TODO: Figure out what to actually return.  Also relevant below in the
    -- error cases.
  where
    noAdminLoginTokenError :: ActionCtxT ctx m a
    noAdminLoginTokenError = do
        setStatus forbidden403
        html "<p>loginToken incorrect</p>"

    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = do
        setStatus forbidden403
        html "<p>token expired error</p>"

adminComponent
    :: forall m ctx
     . ( MonadIO m
       , MonadKucipongDb m
       , MonadKucipongSendEmail m
       , MonadTime m
       )
    => SpockCtxT ctx m ()
adminComponent = do
    get ("login" <//> var) login
