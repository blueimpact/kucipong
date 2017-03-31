module Kucipong.Handler.Store.TemplatePath where

import Kucipong.Prelude

-- =====================
--  Template file paths
-- =====================

-- | File path to template file directory for admin pages.
templateRoot :: FilePath
templateRoot = "store"

templateLogin :: FilePath
templateLogin = templateRoot </> "login.html"

templateStore :: FilePath
templateStore = templateRoot </> "store.html"

templateStoreEdit :: FilePath
templateStoreEdit = templateRoot </> "store_edit.html"

templateCoupon :: FilePath
templateCoupon = templateRoot </> "coupon.html"

templateCouponDelete :: FilePath
templateCouponDelete = templateRoot </> "coupon_delete.html"

templateCouponId :: FilePath
templateCouponId = templateRoot </> "coupon_id.html"

templateCouponIdEdit :: FilePath
templateCouponIdEdit = templateRoot </> "coupon_id_edit.html"
