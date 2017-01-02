{-|
Module      : Kucipong.Dev
Description : functions for playing around with in GHCI

This module defines functions for playing around with in GHCI.
None of these functions should be used in real code.

All of the functions in this module are marked DEPRECATED so they show up as
warnings if they are used in real code.
-}
module Kucipong.Dev
  ( module Kucipong.Dev
  , module Text.Pretty.Simple
  ) where

import Kucipong.Prelude

import Text.Pretty.Simple (pPrint, pShow)

import Kucipong.Config (Config)

{-# WARNING
configDev "This function is only to be used in GHCI during development."
 #-}

configDev :: Config
configDev = error "This has not been implemented yet."
