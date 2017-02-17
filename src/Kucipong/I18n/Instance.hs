{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.I18n.Instance where

import Kucipong.Prelude

import Kucipong.I18n.Class (I18n(..))
import Kucipong.I18n.Types (Lang(..))

import Kucipong.Db (Store(..))
import Kucipong.Db.Models.Base
       (BeautyDetail(..), BusinessCategory(..),
        BusinessCategoryDetail(..), CommonDetail(..), CouponType(..),
        FashionDetail(..), GourmetDetail(..), GadgetDetail(..),
        TravelingDetail(..), foldAllBusinessCategoryDetail)
import Kucipong.Handler.Admin.Types (AdminError(..), AdminMsg(..))
import Kucipong.Handler.Store.Types (StoreError(..), StoreMsg(..))
import Kucipong.Monad.Db.Class (StoreDeleteResult(..))

import Text.EmailAddress (toText)

instance I18n AdminError where
  label EnUS AdminErrorNoAdminEmail =
    "Could not login. This email address has not been registered as an admin user yet."
  label EnUS AdminErrorCouldNotSendEmail =
    "Could not send email. Please try again."
  label EnUS AdminErrorNoAdminLoginToken =
    "Failed to login. Please try again."
  label EnUS AdminErrorTokenExpired =
    "This login URL has been expired. Please try again."
  label EnUS AdminErrorStoreWithSameEmailExists =
    "Store with that email address already exists."
  label EnUS AdminErrorStoreCreateDbProblem =
    "Problem with database. Please try again."
  label EnUS AdminErrorSendEmailFailure =
    "Could not send email. Please try again."
  label EnUS AdminErrorNoStoreEmail =
    "Could not find a store with that email address."
  label EnUS AdminErrorNoAdminSession =
    "Need to be logged in as admin in order to access this page."

instance I18n AdminMsg where
  label EnUS AdminMsgSentVerificationEmail =
    "We have sent you an email with verification URL."

instance I18n StoreDeleteResult where
  label EnUS StoreDeleteSuccess = "Successfully deleted store."
  label EnUS (StoreDeleteErrNameDoesNotMatch realStore given) =
    "Entered store name \"" <> given <> "\" does not match the real store name \"" <>
    storeName realStore <>
    "\"."
  label EnUS (StoreDeleteErrDoesNotExist email) =
    "Store with email address of \"" <> toText email <> "\" does not exist."

instance I18n StoreError where
  label EnUS StoreErrorBusinessCategoryDetailIncorrect =
    "Business category details do not belong to the selected business category."
  label EnUS StoreErrorCouldNotSendEmail =
    "Could not send email. Please try again."
  label EnUS StoreErrorCouldNotUploadImage =
    "Could not upload image. Please try again."
  label EnUS StoreErrorNoImage =
    "Failed to upload image. Please try again."
  label EnUS (StoreErrorNoStoreEmail email) =
    "Could not find store for email " <> tshow email <> "."
  label EnUS StoreErrorNotAnImage =
    "Uploaded file is not an image.  Please upload an image file."

instance I18n StoreMsg where
  label EnUS StoreMsgSentVerificationEmail =
    "We have sent you an email with verification URL."

instance I18n BusinessCategory where
  label EnUS Gourmet = "Gourmet"
  label EnUS Fashion = "Fashion"
  label EnUS Gadget = "Gadget"
  label EnUS Traveling = "Traveling"
  label EnUS Beauty = "Beauty"

instance I18n BusinessCategoryDetail where
  label lang = foldAllBusinessCategoryDetail (Proxy :: Proxy I18n) (label lang)

instance I18n GourmetDetail where
  label EnUS GourmetPizza = "Pizza"
  label EnUS GourmetSushi = "Sushi"

instance I18n FashionDetail where
  label EnUS FashionWomen = "Women"
  label EnUS FashionMen = "Men"

instance I18n GadgetDetail where
  label EnUS GadgetCamera = "Camera"
  label EnUS GadgetPC = "PC"

instance I18n TravelingDetail where
  label EnUS TravelingAsia = "Asia"
  label EnUS TravelingEurope = "Europe"

instance I18n BeautyDetail where
  label EnUS BeautyHairSalon = "Hair salon"
  label EnUS BeautySpa = "Spa"

instance I18n CommonDetail where
  label EnUS CommonPoliteService = "Polite service"
  label EnUS CommonChainStore = "Chain store"

instance I18n CouponType where
  label EnUS CouponTypeDiscount = "Discount"
  label EnUS CouponTypeGift = "Gift"
  label EnUS CouponTypeSet = "Set"
  label EnUS CouponTypeOther = "Other"
