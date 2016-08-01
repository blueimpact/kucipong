module Kucipong.Environment where

import Kucipong.Prelude

-- | Data type representing the kind of environment we are running in.
data Environment = Development
                 | Test
                 | Production
    deriving (Eq, Show, Read)

-- | Class for things that have an 'Environment'.
class HasEnv a where
    getEnv :: a -> Environment

