{-|
Module: WebDriverPreCore.Extended.HTTP.Protocol
Description: Extended WebDriver HTTP API with convenience functions

This module re-exports the HTTP extended API along with common utilities.

- "WebDriverPreCore.Extended.HTTP.Base.API" - HTTP API commands
- "WebDriverPreCore.Extended.HTTP.Base.Protocol" - HTTP protocol types
- "WebDriverPreCore.Utils.Timeout" - Timeout utilities
- "WebDriverPreCore.Error" - Error types
-}
module WebDriverPreCore.Extended.HTTP.Protocol
  ( -- * HTTP API (re-exported)
    module WebDriverPreCore.Extended.HTTP.Base.API,
    -- * Utils (re-exported)
    module WebDriverPreCore.Utils.Timeout,
    -- * Error (re-exported)
    module WebDriverPreCore.Error,
  )
where

import WebDriverPreCore.Error
import WebDriverPreCore.Extended.HTTP.Base.API
import WebDriverPreCore.Utils.Timeout
