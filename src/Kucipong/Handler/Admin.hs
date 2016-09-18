{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Admin where

import Kucipong.Prelude

import Control.Lens ( (^.) )
import Control.Monad.Time ( MonadTime(..) )
import Data.Aeson ( (.=) )
import Data.HVect ( HVect(..) )
import Database.Persist ( Entity(..) )
import Network.HTTP.Types ( forbidden403 )
import Text.EDE ( eitherParse, eitherRender, fromPairs )
import Web.Spock
    ( ActionCtxT, Path, SpockCtxT, (<//>), get, getContext, html, prehook, root
    , redirect, renderRoute, runSpock, setStatus, spockT, text, var )

import Kucipong.Db
    ( Admin, AdminId, AdminLoginToken, Key(..), LoginTokenExpirationTime(..)
    , adminLoginTokenExpirationTime )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad
    ( MonadKucipongCookie, MonadKucipongDb(..), MonadKucipongSendEmail )
import Kucipong.Spock
    ( ContainsAdminSession, getAdminCookie, getAdminEmail, setAdminCookie )
import Kucipong.Session ( Admin, Session(..) )
import Kucipong.Util ( fromEitherM, fromMaybeM )

-- | Login an admin.  Take the admin's 'LoginToken', and send them a session
-- cookie.
login
    :: forall ctx m
     . ( MonadIO m
       , MonadKucipongCookie m
       , MonadKucipongDb m
       , MonadTime m
       )
    => LoginToken -> ActionCtxT ctx m ()
login loginToken = do
    maybeAdminLoginTokenEntity <- dbFindAdminLoginToken loginToken
    (Entity (AdminLoginTokenKey (AdminKey adminEmail)) adminLoginToken) <-
        fromMaybeM noAdminLoginTokenError maybeAdminLoginTokenEntity
    -- check date on admin login token
    now <- currentTime
    let (LoginTokenExpirationTime expirationTime) =
            adminLoginToken ^. adminLoginTokenExpirationTime
    when (now > expirationTime) tokenExpiredError
    setAdminCookie adminEmail
    redirect $ renderRoute root
  where
    -- TODO: What should actually be returned for these two error cases?
    noAdminLoginTokenError :: ActionCtxT ctx m a
    noAdminLoginTokenError = do
        setStatus forbidden403
        html "<p>loginToken incorrect</p>"

    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = do
        setStatus forbidden403
        html "<p>token expired error</p>"

-- | Return the store create page for an admin.
storeCreate
    :: forall xs n m
     . ( ContainsAdminSession n xs
       , MonadIO m
       , MonadLogger m
       )
    => ActionCtxT (HVect xs) m ()
storeCreate = do
    (AdminSession email) <- getAdminEmail
    -- TODO: Actually return the correct html from here.
    let rawTemplate = "{{ hello }}{% if var %}\nHello, {{ var }}!\n{% else %}\nnegative!\n{% endif %}\n"
        env = fromPairs [ "var1" .= ("World" :: Text) ]
        eitherParsedTemplate = eitherParse rawTemplate
    template <- fromEitherM
        (html . ("err occured when parsing template: " <>) . pack)
        eitherParsedTemplate
    let eitherRenderedTemplate = eitherRender template env
    renderedTemplate <- fromEitherM
        (html . ("err occured when trying to render template: " <>) . pack)
        eitherRenderedTemplate
    html . toStrict $ "rendered template: " <> renderedTemplate
    -- $(renderTemplateFromEnv "adminUser_admin_store_create.html")

adminAuthHook
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Admin) ': xs))
adminAuthHook = do
    maybeAdminSession <- getAdminCookie
    case maybeAdminSession of
        -- TODO: Need to return an error page here that tells the user they
        -- need to be logged in as an admin to access this page.
        Nothing -> html "<p>Need to be logged in as admin in order to access this page.</p>"
        Just adminSession -> do
            oldCtx <- getContext
            return $ adminSession :&: oldCtx

adminComponent
    :: forall m xs
     . ( MonadIO m
       , MonadKucipongCookie m
       , MonadKucipongDb m
       , MonadKucipongSendEmail m
       , MonadLogger m
       , MonadTime m
       )
    => SpockCtxT (HVect xs) m ()
adminComponent = do
    get ("login" <//> var) login
    prehook adminAuthHook $
        get ("store" <//> "create") storeCreate
