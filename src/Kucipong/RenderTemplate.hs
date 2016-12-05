{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Kucipong.RenderTemplate
  ( renderTemplateFromEnv
  ) where

import           Kucipong.Prelude           hiding (try)

import           Text.Blaze.Renderer.Text   (renderMarkup)
import           Language.Haskell.TH        (Exp, Q, appE)
import           Text.Heterocephalus        (compileHtmlFileWithDefault)
import           Web.Spock                  (html)

import           Kucipong.I18n (Lang(..), label)

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
        [ ("errors", [|empty :: [Text]|])
        , ("messages", [|empty :: [Text]|])
        , ("isSelected", [|isSelected|])
        , ("isChecked", [|isChecked|])
        , ("key", [|key|])
        , ("label", [|label EnUS|])
        ]

    renderer :: Q Exp
    renderer = [|html . toStrict . renderMarkup|]

-- | For the purpose of rendering 'selected' attributes in the template file.
isSelected :: (Eq a)
           => Maybe a  -- A selected key. 'Nothing' when no options are selected.
           -> a       -- A key to check if it is selected.
           -> Text    -- "selected" | ""
isSelected (Just a) b
  | a == b = "selected"
isSelected _ _ = ""

-- | For the purpose of rendering 'checked' attributes in the template file.
isChecked :: (Eq a)
          => [a]    -- A key list of selected objects.
          -> a      -- A key to check if it is checked.
          -> Text   -- "checked" | ""
isChecked ls k
  | k `elem` ls = "checked"
  | otherwise = ""

-- | Key
key :: (Show a) => a -> Text
key = tshow
