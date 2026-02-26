-- | Internal module: cross-protocol (HTTP + BiDi) shared types and utilities.
-- Not intended for direct import by end users; use the protocol-specific
-- modules ("WebDriverPreCore.Extended.HTTP.Protocol" or
-- "WebDriverPreCore.Extended.BiDi.Protocol") instead.
module WebDriverPreCore.Extended.XProtocol
  ( -- * Error (re-exported)
    module WebDriverPreCore.Error,
    -- * Timeout utilities (re-exported)
    module WebDriverPreCore.Utils.Timeout,
  )
where

import WebDriverPreCore.Error
import WebDriverPreCore.Utils.Timeout
