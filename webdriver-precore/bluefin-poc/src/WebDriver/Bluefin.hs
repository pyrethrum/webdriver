{-|
Module: WebDriver.Bluefin
Description: Bluefin-based wrapper for WebDriver (proof of concept)

Re-exports from sub-modules so consumers can import @WebDriver.Bluefin@
as a single entry point.

This mirrors 'WebDriver.RIO' but uses Bluefin 'Eff'/'IOE' handles instead
of RIO's @Has*@ typeclass constraints and @RIO env a@ monad.
-}
module WebDriver.Bluefin
  ( -- * Capabilities
    module WebDriverPreCore.Extended.Capabilities,

    -- * Core effects (log / pause)
    module WebDriver.Bluefin.Core,

    -- * Handle Types
    module WebDriver.Bluefin.HTTP.Core,

    -- * App / Runners
    module WebDriver.Bluefin.App,

    -- * HTTP Runner
    module WebDriverPreCore.HttpRunner,

    -- * BiDi URL & runner types
    module WebDriverPreCore.BiDiRunner,
  )
where

import WebDriverPreCore.Extended.Capabilities
import WebDriver.Bluefin.Core
import WebDriver.Bluefin.HTTP.Core
import WebDriver.Bluefin.App
import WebDriverPreCore.HttpRunner
import WebDriverPreCore.BiDiRunner
