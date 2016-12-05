{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.I18n.Instance where

import Kucipong.I18n.Class (I18n(..))
import Kucipong.I18n.Types (Lang(..))

import Kucipong.Db.Models.Base
       (BusinessCategory(..), BusinessCategoryDetail(..),
        FashionDetail(..), GourmetDetail(..), GadgetDetail(..),
        TravelingDetail(..), BeautyDetail(..), CommonDetail(..))

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
