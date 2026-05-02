{-|
Module: WebDriver.Effectful
Description: Effectful-based wrapper for WebDriver (proof of concept)

Re-exports from sub-modules so consumers can import @WebDriver.Effectful@
as a single entry point.

This mirrors 'WebDriver.Bluefin' but uses Effectful algebraic effects instead
of Bluefin 'Eff'\/'IOE' handles.  The core difference: instead of passing
handles explicitly, WebDriver effects ('WebDriverHttp', 'WebDriverBiDi',
'Logger', 'LogPause') are tracked in the @es@ type-level list and threaded
implicitly via the @(:>)@ constraint.
-}
module WebDriver.Effectful
  ( -- * Capabilities
    module WebDriverPreCore.Extended.Capabilities,

    -- * Core effects (log / pause)
    Logger, withLogger, log, logDebug, logInfo, logWarn, logError,
    Pause, runPause, runNoPause, pause, pauseAtLeast, sleep, getLogFn,
    -- | Logging severity level
    Severity,

    -- * Effect + interpreter types
    module WebDriver.Effectful.HTTP.Core,

    -- * App / Runners
    module WebDriver.Effectful.App,

    -- * HTTP Runner
    module WebDriverPreCore.HttpRunner,

    -- * BiDi URL & runner types
    module WebDriverPreCore.BiDiRunner,
  )
where

import Prelude hiding (log)
import WebDriverPreCore.Extended.Capabilities
import WebDriver.Effectful.Logger (Logger, withLogger, log, logDebug, logInfo, logWarn, logError, getLogFn, Severity)
import WebDriver.Effectful.Pause (Pause, runPause, runNoPause, pause, pauseAtLeast, sleep)
import WebDriver.Effectful.HTTP.Core
import WebDriver.Effectful.App
import WebDriverPreCore.HttpRunner
import WebDriverPreCore.BiDiRunner
