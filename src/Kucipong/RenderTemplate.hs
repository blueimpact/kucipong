{-# LANGUAGE TemplateHaskell #-}

module Kucipong.RenderTemplate
  ( fromParams
  , renderTemplate
  , renderTemplateFromEnv
  ) where

import Kucipong.Prelude hiding (try)

import Data.Default (def)
import Language.Haskell.TH (Exp, Q, appE)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Heterocephalus
       (ScopeM, compileHtmlFileWith, overwrite, setDefault)
import Web.Spock (html)

import Kucipong.I18n (label)
import Kucipong.View (format, key, toName)

templateDirectory :: FilePath
templateDirectory = "frontend" </> "dist" </> "templates"

-- | The `fromParams` construct a `ScopeM ()` from parameter lists.
fromParams
  :: Q Exp -- ^ @$(qdict) :: [('Text', 'Text')]@
  -> [Text]
  -> ScopeM ()
fromParams qdict = foldr f (pure ())
  where
    f :: Text -> ScopeM () -> ScopeM ()
    f name scope = do
      scope
      overwrite (fromString . unpack $ name) ([|lookup name|] `appE` qdict)

-- | Render a template file with adding empty @errors@ and @messages@ keys/values
-- if they don't already exist in scope.
renderTemplate :: String -> ScopeM () -> Q Exp
renderTemplate filename extraScope = renderer `appE` body
    -- This is the full path of the template file.
  where
    fullFilePath :: FilePath
    fullFilePath = templateDirectory </> filename
    body :: Q Exp
    body =
      compileHtmlFileWith fullFilePath $ do
        setDefault "errors" [|empty :: [Text]|]
        setDefault "messages" [|empty :: [Text]|]
        overwrite "isSelected" [|isSelected|]
        overwrite "isChecked" [|isChecked|]
        overwrite "key" [|key|]
        overwrite "label" [|label def|]
        overwrite "format" [|format|]
        overwrite "toName" [|toName|]
        extraScope
    renderer :: Q Exp
    renderer = [|html . toStrict . renderMarkup|]

-- | Render a template file with adding empty @errors@ and @messages@ keys/values
-- if they don't already exist in scope.
renderTemplateFromEnv :: String -> Q Exp
renderTemplateFromEnv filename = renderTemplate filename (pure ())

-- | For the purpose of rendering 'selected' attributes in the template file.
isSelected
  :: (Eq a)
  => Maybe a -- A selected key. 'Nothing' when no options are selected.
  -> a -- A key to check if it is selected.
  -> Text -- "selected" | ""
isSelected (Just a) b
  | a == b = "selected"
isSelected _ _ = ""

-- | For the purpose of rendering 'checked' attributes in the template file.
isChecked
  :: (Eq a)
  => [a] -- A key list of selected objects.
  -> a -- A key to check if it is checked.
  -> Text -- "checked" | ""
isChecked ls k
  | k `elem` ls = "checked"
  | otherwise = ""
