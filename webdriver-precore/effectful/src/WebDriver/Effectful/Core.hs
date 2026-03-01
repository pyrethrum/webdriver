-- |
-- Module: WebDriver.Effectful.Core
-- Description: Logger and LogPause effects for Effectful WebDriver tests
--
-- Provides two first-class Effectful static effects:
--
-- * 'Logger' — structured console logging backed by Katip; introduce with
--   'withLogger'.  Log output goes to both the terminal and @eval.log@.
-- * 'LogPause' — configurable sleep between driver actions; introduce with
--   'withLogPause'.
--
-- These mirror the Bluefin handles in "WebDriver.Bluefin.Core" but are
-- implemented as Effectful static effects rather than Bluefin compound handles.
-- The key difference: instead of passing a handle explicitly, effects are
-- threaded implicitly through the @es@ constraint.
--
-- Typical usage:
--
-- @
-- withLogger "eval.log" $
--   withLogPause (100 * milliseconds) $ do
--     log "=== step ==="
--     pause
-- @
module WebDriver.Effectful.Core
  (
    -- * Logger effect
    Logger,

    -- * Logger introducer
    withLogger,

    -- * Logger operations
    log,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * LogPause effect
    LogPause,

    -- * LogPause introducer
    withLogPause,

    -- * LogPause operation
    pause,

    -- * Logger internals (for App module)
    getLogFn,

    -- * Re-export
    Severity (..),
  )
where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Effectful (Effect, Dispatch (..), DispatchOf, Eff, IOE, (:>), liftIO, withSeqEffToIO)
import Effectful.Dispatch.Static
  ( StaticRep,
    SideEffects (..),
    evalStaticRep,
    getStaticRep,
  )
import WebDriver.KatipLogging (Severity (..), withKatipLogFunc)
import WebDriverPreCore.Utils.Timeout (Timeout (..))
import Prelude hiding (log)

-- ---------------------------------------------------------------------------
-- Logger effect
-- ---------------------------------------------------------------------------

-- | Effectful static effect for structured logging backed by Katip.
--
-- Introduce with 'withLogger'; use 'log', 'logInfo', etc. to emit messages.
data Logger :: Effect

type instance DispatchOf Logger = Static WithSideEffects

-- | The static rep holds the Katip-backed log function.
newtype instance StaticRep Logger = LoggerRep (Severity -> Text -> IO ())

-- ---------------------------------------------------------------------------
-- Logger introducer
-- ---------------------------------------------------------------------------

-- | Introduce a 'Logger' effect backed by Katip.
--
-- Messages are written to both the terminal (with colour when supported)
-- and to @logFile@ in the current working directory.  The Katip environment
-- is initialised via 'withKatipLogFunc' and is cleaned up safely even when
-- the action throws an exception.
--
-- @
-- withLogger "eval.log" $ do
--   log "Hello"
-- @
withLogger :: (IOE :> es) => FilePath -> Eff (Logger : es) a -> Eff es a
withLogger logFile action =
  withSeqEffToIO $ \runInIO ->
    withKatipLogFunc logFile $ \lf ->
      runInIO (evalStaticRep (LoggerRep lf) action)

-- ---------------------------------------------------------------------------
-- Logger operations
-- ---------------------------------------------------------------------------

logAtSev :: (Logger :> es, IOE :> es) => Severity -> Text -> Eff es ()
logAtSev sev msg = do
  LoggerRep lf <- getStaticRep @Logger
  liftIO $ lf sev msg

-- | Emit a message at 'Info' level.  Alias for 'logInfo'.
log :: (Logger :> es, IOE :> es) => Text -> Eff es ()
log = logInfo

-- | Emit a message at 'Debug' level.
logDebug :: (Logger :> es, IOE :> es) => Text -> Eff es ()
logDebug = logAtSev Debug

-- | Emit a message at 'Info' level.
logInfo :: (Logger :> es, IOE :> es) => Text -> Eff es ()
logInfo = logAtSev Info

-- | Emit a message at 'Warn' level.
logWarn :: (Logger :> es, IOE :> es) => Text -> Eff es ()
logWarn = logAtSev Warn

-- | Emit a message at 'Error' level.
logError :: (Logger :> es, IOE :> es) => Text -> Eff es ()
logError = logAtSev Error

-- | Extract the underlying @IO@ log function from the 'Logger' static effect.
--
-- Useful in 'WebDriver.Effectful.App' to wire driver-level logging through
-- the existing 'Logger' effect without creating a second Katip environment.
getLogFn :: (Logger :> es) => Eff es (Severity -> Text -> IO ())
getLogFn = do
  LoggerRep lf <- getStaticRep @Logger
  pure lf

-- ---------------------------------------------------------------------------
-- LogPause effect
-- ---------------------------------------------------------------------------

-- | Effectful static effect carrying a configurable pause duration.
--
-- Introduce with 'withLogPause'; use 'pause' to sleep between actions.
data LogPause :: Effect

type instance DispatchOf LogPause = Static NoSideEffects

-- | The static rep holds the pause 'Timeout'.
newtype instance StaticRep LogPause = LogPauseRep Timeout

-- ---------------------------------------------------------------------------
-- LogPause introducer
-- ---------------------------------------------------------------------------

-- | Run an action with a 'LogPause' effect providing the given 'Timeout'.
withLogPause :: Timeout -> Eff (LogPause : es) a -> Eff es a
withLogPause = evalStaticRep . LogPauseRep

-- ---------------------------------------------------------------------------
-- LogPause operation
-- ---------------------------------------------------------------------------

-- | Sleep for the duration stored in the 'LogPause' effect.
pause :: (LogPause :> es, IOE :> es) => Eff es ()
pause = do
  LogPauseRep d <- getStaticRep @LogPause
  liftIO $ threadDelay d.microseconds
