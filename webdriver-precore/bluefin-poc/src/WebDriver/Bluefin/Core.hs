{-# LANGUAGE DerivingVia #-}

-- |
-- Module: WebDriver.Bluefin.Core
-- Description: Logger and LogPause effects for Bluefin WebDriver tests
--
-- Provides two first-class Bluefin handles:
--
-- * 'Logger' — structured console logging backed by Katip; introduce with
--   'withLogger'.  Log output goes to both the terminal and @eval.log@.
-- * 'LogPause' — configurable sleep between driver actions; introduce with
--   'withLogPause'.
--
-- The Katip integration lives in "WebDriver.Bluefin.LoggingImp"; this module
-- re-exports 'Severity' from there.
--
-- Typical usage:
--
-- @
-- withLogger io $ \logger ->
--   withLogPause io behaviour.pauseDuration $ \lp -> do
--     log logger \"=== step ==="
--     pause lp
-- @
module WebDriver.Bluefin.Core
  (
    -- * Logger handle
    Logger (..),


    -- * Logger introducer
    withLogger,

    -- * Logger effects
    log,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * LogPause handle
    LogPause (..),

    -- * LogPause introducer
    withLogPause,

    -- * LogPause effect
    pause,
  )
where

import Bluefin.Compound (Handle, OneWayCoercible (..), OneWayCoercibleHandle (..), gOneWayCoercible)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE, effIO, withEffToIO_)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriver.Bluefin.Logger (Severity (..), withKatipLogFunc)
import WebDriverPreCore.Utils.Timeout (Timeout (..))
import Prelude hiding (log)

-- ---------------------------------------------------------------------------
-- Logger handle
-- ---------------------------------------------------------------------------

-- | Bluefin handle for structured logging.
--
-- Introduce with 'withLogger'; use 'log', 'logInfo', etc. to emit messages.
data Logger e = MkLogger
  { logFunc :: Severity -> Text -> IO (),
    loggerIO :: IOE e
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle Logger

instance (e :> es) => OneWayCoercible (Logger e) (Logger es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- ---------------------------------------------------------------------------
-- Logger introducer
-- ---------------------------------------------------------------------------

-- | Introduce a 'Logger' backed by Katip.
--
-- Log messages are written to both the terminal (with colour when supported)
-- and to @eval.log@ in the current working directory.  The Katip environment
-- is initialised via 'WebDriver.Bluefin.LoggingImp.withKatipLogFunc' and is
-- cleaned up safely even when the action throws an exception.
--
-- @
-- withLogger io $ \logger -> do
--   log logger "Hello"
-- @
withLogger :: (e :> es) => IOE e -> (Logger e -> Eff es a) -> Eff es a
withLogger io action =
  withEffToIO_ io $ \runInIO ->
    withKatipLogFunc "eval.log" $ \lf ->
      runInIO (action MkLogger {loggerIO = io, logFunc = lf})

-- ---------------------------------------------------------------------------
-- Logger effects
-- ---------------------------------------------------------------------------

-- | Emit a message at 'Info' level.  Alias for 'logInfo'.
log :: (e :> es) => Logger e -> Text -> Eff es ()
log = logInfo

lgSev :: (e :> es) => Logger e -> Severity -> Text -> Eff es ()
lgSev logger sev = effIO logger.loggerIO . logger.logFunc sev

-- | Emit a message at 'LvlDebug' level.
logDebug :: (e :> es) => Logger e -> Text -> Eff es ()
logDebug logger = lgSev logger Debug

-- | Emit a message at 'LvlInfo' level.
logInfo :: (e :> es) => Logger e -> Text -> Eff es ()
logInfo logger = lgSev logger Info

-- | Emit a message at 'LvlWarn' level.
logWarn :: (e :> es) => Logger e -> Text -> Eff es ()
logWarn logger = lgSev logger Warn

-- | Emit a message at 'LvlError' level.
logError :: (e :> es) => Logger e -> Text -> Eff es ()
logError logger = lgSev logger Error

-- ---------------------------------------------------------------------------
-- LogPause handle
-- ---------------------------------------------------------------------------

-- | Bluefin handle that carries an 'IOE' handle and a pause duration.
--
-- Introduce it with 'withLogPause'; use 'pause' to sleep between actions.
data LogPause e = MkLogPause
  { lpIO :: IOE e,
    lpPauseDuration :: Timeout
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle LogPause

instance (e :> es) => OneWayCoercible (LogPause e) (LogPause es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- ---------------------------------------------------------------------------
-- LogPause introducer
-- ---------------------------------------------------------------------------

-- | Run an action with a 'LogPause' handle built from an existing 'IOE' and
-- a pause duration.
--
-- @
-- withLogPause io (5 * seconds) $ \lp -> do
--   pause lp
-- @
withLogPause :: IOE e -> Timeout -> (LogPause e -> Eff es a) -> Eff es a
withLogPause io dur action = action MkLogPause {lpIO = io, lpPauseDuration = dur}

-- ---------------------------------------------------------------------------
-- LogPause effect
-- ---------------------------------------------------------------------------

-- | Sleep for the 'lpPauseDuration' stored in the 'LogPause' handle.
pause :: (e :> es) => LogPause e -> Eff es ()
pause lp = effIO lp.lpIO $ threadDelay (let MkTimeout us = lp.lpPauseDuration in us)
