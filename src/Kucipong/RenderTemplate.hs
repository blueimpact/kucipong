{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Kucipong.RenderTemplate where

import           Kucipong.Prelude           hiding (try)

import           Text.Blaze.Renderer.Text   (renderMarkup)
import           Language.Haskell.TH        (Exp, Q, appE)
import           Text.Heterocephalus        (compileHtmlFileWithDefault)
import           Web.Spock                  (html)

templateDirectory :: FilePath
templateDirectory = "frontend" </> "dist"

-- | Render a template file with adding empty @errors@ and @messages@ keys/values
-- if they don't already exist in scope.
renderTemplateFromEnv :: String -> Q Exp
renderTemplateFromEnv filename = renderer `appE` body
  where
    -- This is the full path of the template file.
    fullFilePath :: FilePath
    fullFilePath = templateDirectory </> filename

    body :: Q Exp
    body =
      compileHtmlFileWithDefault
        fullFilePath
        [("errors", [|empty :: [Text]|]), ("messages", [|empty :: [Text]|])]

    renderer :: Q Exp
    renderer = [|html . toStrict . renderMarkup|]
