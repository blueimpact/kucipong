module Kucipong.Handler.Consumer.TemplatePath where

import Kucipong.Prelude

-- =====================
--  Template file paths
-- =====================

-- | File path to template file directory for admin pages.
templateRoot :: FilePath
templateRoot = "."

templateCouponId :: FilePath
templateCouponId = templateRoot </> "coupon_id.html"

templateCategory :: FilePath
templateCategory = templateRoot </> "category.html"
