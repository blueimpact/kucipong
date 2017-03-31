{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Error
  ( resp404
  ) where

import Kucipong.Prelude

import Network.HTTP.Types (status404)
import Web.Spock (ActionCtxT, setStatus)

import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Handler.Error.TemplatePath (template404)

resp404 :: MonadIO m => [Text] -> ActionCtxT ctx m ()
resp404 errors = do
  setStatus status404
  $(renderTemplateFromEnv template404)
