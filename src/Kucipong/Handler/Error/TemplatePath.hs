module Kucipong.Handler.Error.TemplatePath where

import Kucipong.Prelude

-- =====================
--  Template file paths
-- =====================

-- | File path to template file directory for admin pages.
templateRoot :: FilePath
templateRoot = ""

template404 :: FilePath
template404 = templateRoot </> "404.html"
