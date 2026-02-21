{-|
Module: WebDriver.RIO
Description: RIO-based wrapper for WebDriver (proof of concept)

Re-exports from sub-modules so consumers can import @WebDriver.RIO@
as a single entry point.
-}
module WebDriver.RIO
  ( -- * Capabilities
    module WebDriverPreCore.Extended.Capabilities,

    -- * Environment Types & Typeclasses
    module WebDriver.RIO.Env,

    -- * App / Runners
    module WebDriver.RIO.App,

    -- * Logging
    module WebDriver.RIO.Logging,

    -- * HTTP Runner - todo review if this should be re-exported here or just used internally by App1
    module WebDriverPreCore.HttpRunner,
  )
where

import WebDriverPreCore.Extended.Capabilities
import WebDriver.RIO.Env
import WebDriver.RIO.Logging
import WebDriver.RIO.App
import WebDriverPreCore.HttpRunner
