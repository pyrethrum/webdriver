{-|
Module: WebDriverPreCore.Extended.BiDi.Protocol
Description: Extended WebDriver BiDi protocol with convenience functions

This module re-exports the BiDi extended API along with common utilities.

- "WebDriverPreCore.Extended.BiDi.Base.API" - BiDi API commands
- "WebDriverPreCore.Extended.BiDi.Base.Protocol" - BiDi protocol types
- "WebDriverPreCore.Utils.Timeout" - Timeout utilities
- "WebDriverPreCore.Error" - Error types
-}
module WebDriverPreCore.Extended.BiDi.Protocol
  ( -- * Utils (re-exported)
    module WebDriverPreCore.Utils.Timeout,
    -- * Error (re-exported)
    module WebDriverPreCore.Error,
  )
where

import WebDriverPreCore.Error
import WebDriverPreCore.Utils.Timeout
