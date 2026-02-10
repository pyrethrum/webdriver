{-|
Module: WebDriverPreCore.Extended.Capabilities
Description: Parameterised full-capabilities wrapper

Temporary unified capabilities — workaround until protocols share
a common capability abstraction.
-}
module WebDriverPreCore.Extended.Capabilities
  ( -- * Full Capabilities
    FullCapabilities (..),

    -- * Type Synonyms
    HttpFullCapabilities,
    BiDiFullCapabilities,

    -- * Re-exports — protocol-specific capability types
    HTTP.Capabilities (..),
    BiDi.Capability (..),
  )
where

import WebDriverPreCore.HTTP.Protocol qualified as HTTP
import WebDriverPreCore.BiDi.Protocol qualified as BiDi

-- | Parameterised full-capabilities wrapper that unifies the HTTP and BiDi
--   capability envelope shapes (@alwaysMatch@ + @firstMatch@).
data FullCapabilities cap = MkFullCapabilities
  { alwaysMatch :: Maybe cap
  , firstMatch :: [cap]
  }
  deriving (Show, Eq)

-- | HTTP protocol full capabilities.
type HttpFullCapabilities = FullCapabilities HTTP.Capabilities

-- | BiDi protocol full capabilities.
type BiDiFullCapabilities = FullCapabilities BiDi.Capability
