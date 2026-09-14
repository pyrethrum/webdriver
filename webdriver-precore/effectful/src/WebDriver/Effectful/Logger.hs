-- |
-- Module: WebDriver.Effectful.Logger
-- Description: Katip-based Logger effect for Effectful WebDriver
--
-- Provides the 'Logger' effect (an alias for Katip's 'KatipE' static effect)
-- and 'withLogger', which registers a terminal scribe and a file scribe inside
-- the 'KatipE' environment.
--
-- Log operations delegate directly to 'Effectful.Katip', so structured
-- context (@katipAddContext@, @katipAddNamespace@) is available wherever
-- 'Logger' is in scope.
--
-- Typical usage:
--
-- @
-- withLogger "eval.log" $ do
--   log "session started"
-- @
module WebDriver.Effectful.Logger
  ( -- * Logger effect
    Logger,

    -- * Logger introducer
    withLogger,

    -- * Logger resource management
    LoggerData (..),
    acquireLogger,
    acquireNoOpLogger,
    releaseLogger,
    runLogger,

    -- * Logger operations
    log,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- * Logger internals (for App module)
    getLogFn,

    -- * Re-export
    Severity (..),
  )
where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Effectful (Eff, IOE, liftIO, withSeqEffToIO, (:>))
import Effectful.Katip
  ( Item (..),
    ItemFormatter,
    KatipE,
    Severity (..),
    getLogEnv,
    logStr,
    renderSeverity,
    runKatipE,
    unLogStr,
  )
import Effectful.Katip qualified as EK
import Katip (initLogEnv)
import Katip qualified as K
import Katip.Scribes.Handle (colorBySeverity)
import System.IO (Handle, IOMode (..), hClose, openFile, stdout)
import Prelude hiding (log)

-- ---------------------------------------------------------------------------
-- Logger effect
-- ---------------------------------------------------------------------------

-- | The Logger effect; backed by Katip's 'KatipE' static effect.
--
-- Introduce with 'withLogger'; structured context propagation
-- (@katipAddContext@, @katipAddNamespace@) is available wherever 'Logger'
-- is in scope.
type Logger = KatipE

-- ---------------------------------------------------------------------------
-- Local-time formatter
-- ---------------------------------------------------------------------------

-- | Katip 'ItemFormatter' that displays timestamps in the local time zone.
localBracketFormat :: TimeZone -> ItemFormatter a
localBracketFormat tz withColor _verb Item {..} =
  brackets nowStr
    <> brackets (fromText (colorBySeverity withColor _itemSeverity (renderSeverity _itemSeverity)))
    <> fromText " "
    <> unLogStr _itemMessage
  where
    localTime = utcToLocalTime tz _itemTime
    nowStr = fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime

brackets :: Builder -> Builder
brackets m = "[" <> m <> "]"

-- ---------------------------------------------------------------------------
-- Logger handle
-- ---------------------------------------------------------------------------

-- | Opaque handle holding a Katip 'K.LogEnv' (with scribes registered) and
-- an optional log-file 'Handle'.
--
-- Use 'acquireLogger' \/ 'releaseLogger' as an acquire\/release pair (e.g.
-- with 'Test.Tasty.withResource') and 'runLogger' to inject the
-- 'Logger' effect into each action.  'withLogger' uses all three internally.
-- Use 'acquireNoOpLogger' to obtain a handle that discards all log output.
data LoggerData = MkLoggerData
  { 
    fileHandle :: (Maybe Handle),
    loggerEnv :: K.LogEnv
  }

-- ---------------------------------------------------------------------------
-- Logger introducer
-- ---------------------------------------------------------------------------

-- | Open a log file and register a terminal scribe and a file scribe,
-- returning a 'LoggerData'.
--
-- Pair with 'releaseLogger' to form an acquire\/release pair suitable for
-- 'Test.Tasty.withResource' or any other bracket-style combinator.
-- Use 'withLogger' when a single bracketed scope suffices.
acquireLogger :: FilePath -> IO LoggerData
acquireLogger logFile = do
  fileHandle <- openFile logFile WriteMode
  timeZone <- getCurrentTimeZone

  -- make scribes
  termScribe <- K.mkHandleScribeWithFormatter (localBracketFormat timeZone) K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  fileScribe <- K.mkHandleScribeWithFormatter (localBracketFormat timeZone) (K.ColorLog False) fileHandle (K.permitItem K.DebugS) K.V2

  -- register scribes with env
  baseLogEnv <- initLogEnv "webdriver" "eval"
  logEnvTerm <- K.registerScribe "stdout" termScribe K.defaultScribeSettings baseLogEnv
  logEnvFull <- K.registerScribe "file" fileScribe K.defaultScribeSettings logEnvTerm
  pure $ MkLoggerData (Just fileHandle) logEnvFull

-- | Create a 'LoggerData' with no scribes registered; all log output is
-- discarded.  Useful when logging is disabled but a 'LoggerData' is still
-- required (e.g. as a resource in 'Test.Tasty.withResource').
acquireNoOpLogger :: IO LoggerData
acquireNoOpLogger = MkLoggerData Nothing <$> initLogEnv "webdriver" "eval"


-- | Flush and close all scribes in a 'LoggerData', then close the
-- log-file handle if one was opened.
releaseLogger :: LoggerData -> IO ()
releaseLogger MkLoggerData{fileHandle, loggerEnv} = K.closeScribes loggerEnv >> maybe (pure ()) hClose fileHandle

-- | Run an effectful action inside the 'Logger' effect using an existing
-- 'LoggerData'.
runLogger :: (IOE :> es) => Maybe LoggerData -> Eff (Logger : es) a -> Eff es a
runLogger mlh action = do
  le <-
    maybe
      -- create a fresh env with no registered scribes (effectively no output)
      (liftIO $ initLogEnv "webdriver" "eval")
      (\(MkLoggerData le _) -> pure le)
      mlh
  runKatipE le action

-- | Introduce a 'Logger' effect backed by Katip.
--
-- Registers a terminal scribe (colour when the output is a TTY) and a file
-- scribe for @logFile@.  Scribes are finalised when the action exits.
-- This is a convenience wrapper around 'acquireLogger', 'releaseLogger',
-- and 'runLogger'.
--
-- @
-- withLogger "eval.log" $ do
--   log "Hello"
-- @
withLogger :: (IOE :> es) => FilePath -> Eff (Logger : es) a -> Eff es a
withLogger logFile action =
  withSeqEffToIO $ \runInIO ->
    bracket (acquireLogger logFile) releaseLogger $ \lh ->
      runInIO (runLogger (Just lh) action)

-- ---------------------------------------------------------------------------
-- Logger operations
-- ---------------------------------------------------------------------------

logAtSev :: (Logger :> es) => Severity -> Text -> Eff es ()
logAtSev sev txt = EK.logMsg "app" sev (logStr txt)

-- | Emit a message at 'InfoS' level.  Alias for 'logInfo'.
log :: (Logger :> es) => Text -> Eff es ()
log = logInfo

-- | Emit a message at 'DebugS' level.
logDebug :: (Logger :> es) => Text -> Eff es ()
logDebug = logAtSev DebugS

-- | Emit a message at 'InfoS' level.
logInfo :: (Logger :> es) => Text -> Eff es ()
logInfo = logAtSev InfoS

-- | Emit a message at 'WarningS' level.
logWarn :: (Logger :> es) => Text -> Eff es ()
logWarn = logAtSev WarningS

-- | Emit a message at 'ErrorS' level.
logError :: (Logger :> es) => Text -> Eff es ()
logError = logAtSev ErrorS

-- | Extract an IO-level log callback from the 'Logger' effect.
--
-- Uses the 'LogEnv' captured in the static rep to construct a plain
-- @IO ()@ action.  Useful for wiring driver-level logging through the
-- existing 'Logger' effect without creating a second Katip environment.
getLogFn :: (Logger :> es) => Eff es (K.Severity -> Text -> IO ())
getLogFn = do
  le <- getLogEnv
  pure $ \sev txt -> K.runKatipT le $ K.logMsg "app" sev (K.logStr txt)
