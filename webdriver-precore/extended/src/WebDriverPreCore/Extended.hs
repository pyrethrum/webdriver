{-|
Module: WebDriverPreCore.Extended
Description: Extended WebDriver API with convenience functions

This module provides documentation on available sub-modules. Due to
naming conflicts between HTTP and BiDi protocols (e.g., Cookie, SameSite),
users should import the specific sub-modules they need:

- "WebDriverPreCore.Extended.HTTP.Base.API" - HTTP API commands
- "WebDriverPreCore.Extended.HTTP.Base.Protocol" - HTTP protocol types
- "WebDriverPreCore.Extended.BiDi.Base.API" - BiDi API commands  
- "WebDriverPreCore.Extended.BiDi.Base.Protocol" - BiDi protocol types
-}
module WebDriverPreCore.Extended
  ( -- * HTTP API (re-exported)
    module WebDriverPreCore.Extended.HTTP.Base.API,
  )
where

import WebDriverPreCore.Extended.HTTP.Base.API
