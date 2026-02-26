{-|
Module: WebDriverPreCore.Extended.HTTP.ProtocolExclusive
Description: HTTP-exclusive WebDriver API — types and commands specific to HTTP transport

This module re-exports HTTP-specific types and commands that are exclusive
to the HTTP transport and not shared with the BiDi protocol.

- "WebDriverPreCore.Extended.HTTP.Base.API" - HTTP API commands
-}
module WebDriverPreCore.Extended.HTTP.ProtocolExclusive
  ( -- * HTTP API (re-exported)
    module WebDriverPreCore.Extended.HTTP.Base.API,
  )
where

import WebDriverPreCore.Extended.HTTP.Base.API
