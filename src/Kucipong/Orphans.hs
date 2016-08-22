{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Description : Orphan instances

This is a hack to make orphan instances of some of the types we use.  This file
will mostly be making persistent ('PersistField' and 'PersistFieldSql') and
aeson ('FromJSON' and 'ToJSON') instances for types we use that are defined in
other libraries but that we use here.
-}

module Kucipong.Orphans where

import ClassyPrelude
