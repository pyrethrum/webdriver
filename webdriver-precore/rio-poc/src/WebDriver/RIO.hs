{-|
Module: WebDriver.RIO
Description: RIO-based wrapper for WebDriver (proof of concept)

Re-exports from sub-modules so consumers can import @WebDriver.RIO@
as a single entry point.
-}
module WebDriver.RIO
  ( -- * Capabilities
    module WebDriver.RIO.Capabilities,

    -- * Environment Types
    module WebDriver.RIO.Env,

    -- * Runner Typeclasses
    module WebDriver.RIO.App,

    -- * Logging
    module WebDriver.RIO.Logging,
  )
where

import WebDriver.RIO.Capabilities
import WebDriver.RIO.Env
import WebDriver.RIO.Logging
import WebDriver.RIO.App
