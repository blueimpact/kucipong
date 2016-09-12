
module Kucipong.Host where

import Kucipong.Prelude

import Network.Wai.Handler.Warp ( Port )

-- TODO: Pull this file out into a separate package.

type Host = Text

-- | Class for things that have a host.  For example, "localhost",
-- "example.com", or "google.com:8080".
class HasHost a where
    getHost :: a -> Host

mHost :: (HasHost r, MonadReader r m) => m Host
mHost = reader getHost

type Protocol = Text

-- | Class for things that have a protocol.  For example, "http" or "https".
class HasProtocol a where
    getProtocol :: a -> Protocol

mProtocol :: (HasProtocol r, MonadReader r m) => m Protocol
mProtocol = reader getProtocol

-- | Class for things that have a port.
class HasPort a where
    getPort :: a -> Port

mPort :: (HasPort r, MonadReader r m) => m Port
mPort = reader getPort
