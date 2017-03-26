module Kucipong.Handler.Admin.TemplatePath where

import Kucipong.Prelude

-- =====================
--  Template file paths
-- =====================

-- | File path to template file directory for admin pages.
templateRoot :: FilePath
templateRoot = "admin"

templateLogin :: FilePath
templateLogin = templateRoot </> "login.html"

templateStoreLogin :: FilePath
templateStoreLogin = templateRoot </> "store_login.html"

templateStoreCreate :: FilePath
templateStoreCreate = templateRoot </> "store_create.html"

templateStoreDelete :: FilePath
templateStoreDelete = templateRoot </> "store_delete.html"

templateStoreDeleteConfirm :: FilePath
templateStoreDeleteConfirm = templateRoot </> "store_delete_confirm.html"
