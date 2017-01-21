{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler where

import Kucipong.Prelude

import Data.HVect (HVect(HNil))
import Network.Wai (Middleware)
import Web.Spock
       (ActionCtxT, redirect, renderRoute, root, runSpock)
import Web.Spock.Core (spockT, get, prehook, subcomponent)

import Kucipong.Config (Config)
import Kucipong.Handler.Admin (adminComponent, adminUrlPrefix)
import Kucipong.Handler.Static (staticComponent, staticUrlPrefix)
import Kucipong.Handler.Store (storeComponent, storeUrlPrefix)
import Kucipong.Host (HasPort(..))
import Kucipong.Monad (KucipongM, runKucipongM)

runKucipongMHandleErrors :: Config -> KucipongM a -> IO a
runKucipongMHandleErrors config = either throwIO pure <=< runKucipongM config

baseHook
  :: Monad m
  => ActionCtxT () m (HVect '[])
baseHook = pure HNil

app :: Middleware -> Config -> IO ()
app middleware config = do
  spockMiddleware <- spockMiddlewareIO
  runSpock (getPort config) . pure $ middleware . spockMiddleware
  where
    spockMiddlewareIO :: IO Middleware
    spockMiddlewareIO =
      spockT (runKucipongMHandleErrors config) $ do
        subcomponent staticUrlPrefix staticComponent
        prehook baseHook $ do
          subcomponent adminUrlPrefix adminComponent
          subcomponent storeUrlPrefix storeComponent
        get root $
          redirect . renderRoute $ "static/chat.html"
