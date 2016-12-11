{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.I18n.Instance where

import Kucipong.Prelude

import Kucipong.I18n.Class (I18n(..))
import Kucipong.I18n.Types (Lang(..))

import Kucipong.Db (Store(..))
import Kucipong.Db.Models.Base
       (BeautyDetail(..), BusinessCategory(..),
        BusinessCategoryDetail(..), CommonDetail(..), FashionDetail(..),
        GourmetDetail(..), GadgetDetail(..), TravelingDetail(..))
import Kucipong.Handler.Admin.Types (AdminError(..), AdminMsg(..))
import Kucipong.Monad.Db.Class (StoreDeleteResult(..))

import Text.EmailAddress (toText)

instance I18n AdminError where
  label EnUS AdminErrorNoAdminEmail =
    "Could not login."
  label EnUS AdminErrorCouldNotSendEmail =
    "Could not send email."
  label EnUS AdminErrorNoAdminLoginToken =
    "Failed to log in X(\nPlease try again."
  label EnUS AdminErrorTokenExpired =
    "This log in URL has been expired X(\nPlease try again."
  label EnUS AdminErrorStoreWithSameEmailExists =
    "Store with that email address already exists."
  label EnUS AdminErrorStoreCreateDbProblem =
    "Problem with database."
  label EnUS AdminErrorSendEmailFailure =
    "Could not send email."
  label EnUS AdminErrorNoStoreEmail =
    "Could not find a store with that email address."
  label EnUS AdminErrorNoAdminSession =
    "Need to be logged in as admin in order to access this page."

instance I18n AdminMsg where
  label EnUS AdminMsgSentVerificationEmail =
    "We have sent you email with verification URL."

instance I18n StoreDeleteResult where
  label EnUS StoreDeleteSuccess = "Successfully deleted store."
  label EnUS (StoreDeleteErrNameDoesNotMatch realStore given) =
    "Store name \"" <> given <>
      "\" does not match the real store name \"" <>
      storeName realStore <>
      "\"."
  label EnUS (StoreDeleteErrDoesNotExist email) =
    "Store with email address of \"" <> toText email <> "\" does not exist"

instance I18n BusinessCategory where
  label EnUS Gourmet = "Gourmet"
  label EnUS Fashion = "Fashion"
  label EnUS Gadget = "Gadget"
  label EnUS Traveling = "Traveling"
  label EnUS Beauty = "Beauty"

instance I18n BusinessCategoryDetail where
  label lang (GourmetDetail a) = label lang a
  label lang (FashionDetail a) = label lang a
  label lang (GadgetDetail a) = label lang a
  label lang (TravelingDetail a) = label lang a
  label lang (BeautyDetail a) = label lang a
  label lang (CommonDetail a) = label lang a

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
