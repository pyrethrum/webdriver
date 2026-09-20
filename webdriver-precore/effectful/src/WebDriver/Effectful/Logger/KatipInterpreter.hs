-- |
-- Module: WebDriver.Effectful.Logger.KatipInterpreter
-- Description: Katip-backed interpreter for the 'Logger' effect
--
-- Provides 'runLogger' (and the 'LoggerData' resource) which interpret the
-- generic 'Logger' dynamic effect using Katip scribes.
--
-- Typical usage with a single bracketed scope:
--
-- @
-- withLogger "eval.log" $ do
--   logInfo "session started"
-- @
--
-- For test frameworks that need explicit acquire\/release:
--
-- @
-- withResource acquireLogger releaseLogger $ \ld ->
--   runLogger (Just ld) myTest
-- @
module WebDriver.Effectful.Logger.KatipInterpreter
  ( -- * Logger resource management
    LoggerData (..),
    acquireLogger,
    acquireNoOpLogger,
    releaseLogger,
    runLogger,

    -- * Convenience bracket
    withLogger,
  )
where

import Control.Exception (bracket)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Effectful (Eff, IOE, liftIO, withSeqEffToIO, (:>))
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Katip (Item (..), initLogEnv)
import Katip qualified as K
import Katip.Scribes.Handle (colorBySeverity)
import System.IO (Handle, IOMode (..), hClose, openFile, stdout)
import WebDriver.Effectful.Logger.Effect (Logger (..), Severity (..))

-- ---------------------------------------------------------------------------
-- Severity mapper
-- ---------------------------------------------------------------------------

-- | Translate the generic 'Severity' to Katip's 'K.Severity'.
toKatipSeverity :: Severity -> K.Severity
toKatipSeverity = \case
  Debug -> K.DebugS
  Info -> K.InfoS
  Notice -> K.NoticeS
  Warning -> K.WarningS
  Error -> K.ErrorS
  Critical -> K.CriticalS
  Alert -> K.AlertS
  Emergency -> K.EmergencyS

-- ---------------------------------------------------------------------------
-- Local-time formatter
-- ---------------------------------------------------------------------------

-- | Katip 'K.ItemFormatter' that displays timestamps in the local time zone.
localBracketFormat :: TimeZone -> K.ItemFormatter a
localBracketFormat tz withColor _verb Item {..} =
  brackets nowStr
    <> brackets (fromText (colorBySeverity withColor _itemSeverity (K.renderSeverity _itemSeverity)))
    <> fromText " "
    <> K.unLogStr _itemMessage
  where
    localTime = utcToLocalTime tz _itemTime
    nowStr = fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
    brackets :: Builder -> Builder
    brackets m = "[" <> m <> "]"

-- ---------------------------------------------------------------------------
-- Logger handle
-- ---------------------------------------------------------------------------

data LoggerData = MkLoggerData
  { fileHandle :: Maybe Handle,
    loggerEnv :: K.LogEnv
  }

-- ---------------------------------------------------------------------------
-- Acquire / release
-- ---------------------------------------------------------------------------

acquireLogger :: FilePath -> IO LoggerData
acquireLogger logFile = do
  -- get IO ingerdients
  fh <- openFile logFile WriteMode
  timeZone <- getCurrentTimeZone

  -- make scribes
  let timeFormatter :: forall a. K.ItemFormatter a
      timeFormatter = localBracketFormat timeZone
      mkScribe = K.mkHandleScribeWithFormatter 
  termScribe <- mkScribe timeFormatter K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  fileScribe <- mkScribe timeFormatter (K.ColorLog False) fh (K.permitItem K.DebugS) K.V2

  -- register scribes
  MkLoggerData (Just fh)
    <$> ( initLogEnv "webdriver" "eval"
            >>= K.registerScribe "stdout" termScribe K.defaultScribeSettings
            >>= K.registerScribe "file" fileScribe K.defaultScribeSettings
        )

acquireNoOpLogger :: IO LoggerData
acquireNoOpLogger = MkLoggerData Nothing <$> initLogEnv "webdriver" "eval"

releaseLogger :: LoggerData -> IO ()
releaseLogger MkLoggerData {fileHandle, loggerEnv} =
  K.closeScribes loggerEnv >> maybe (pure ()) hClose fileHandle

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

runLogger :: forall es a. (IOE :> es) => Maybe LoggerData -> Eff (Logger : es) a -> Eff es a
runLogger mlh action = do
  le <-
    liftIO $
      maybe
        (initLogEnv "webdriver" "eval")
        (\(MkLoggerData _ env) -> pure env)
        mlh
  interpret (katipHandler le) action
  where
    katipHandler :: K.LogEnv -> EffectHandler Logger es
    katipHandler le _ = \case
      LogAtSev sev txt ->
        liftIO $ K.runKatipT le $ K.logMsg "app" (toKatipSeverity sev) (K.logStr txt)

withLogger :: (IOE :> es) => FilePath -> Eff (Logger : es) a -> Eff es a
withLogger logFile action =
  withSeqEffToIO $ \runInIO ->
    bracket (acquireLogger logFile) releaseLogger $ \lh ->
      runInIO (runLogger (Just lh) action)
