{-|
Module: WebDriverPreCore.Extended
Description: Extended WebDriver API with convenience functions

This module provides documentation on available sub-modules. Due to
naming conflicts between HTTP and BiDi protocols (e.g., Cookie, SameSite),
users should import the specific sub-modules they need:

- "WebDriverPreCore.Extended.Base.HTTP.API" - HTTP API commands
- "WebDriverPreCore.Extended.Base.HTTP.Protocol" - HTTP protocol types
- "WebDriverPreCore.Extended.Base.BiDi.API" - BiDi API commands  
- "WebDriverPreCore.Extended.Base.BiDi.Protocol" - BiDi protocol types
-}
module WebDriverPreCore.Extended
  ( -- * HTTP API (re-exported)
    module WebDriverPreCore.Extended.Base.HTTP.API,
  )
where

import WebDriverPreCore.Extended.Base.HTTP.API
