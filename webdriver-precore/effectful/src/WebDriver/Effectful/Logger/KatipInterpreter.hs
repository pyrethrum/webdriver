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

import Effectful.Exception (bracket)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Katip (ColorStrategy, Item (..), Scribe, initLogEnv)
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
localBracketFormat :: TimeZone -> Bool -> K.Verbosity -> K.Item a -> Builder
localBracketFormat tz wantColour _verb Item {_itemSeverity, _itemTime, _itemMessage} =
  brackets nowStr
    <> brackets (fromText (colorBySeverity wantColour _itemSeverity (K.renderSeverity _itemSeverity)))
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

acquireNoOpLogger :: IO LoggerData
acquireNoOpLogger = MkLoggerData Nothing <$> initLogEnv "webdriver" "eval"

acquireLogger :: FilePath -> IO LoggerData
acquireLogger logFile = do
  -- get IO ingerdients
  fileHandle <- openFile logFile WriteMode
  timeZone <- getCurrentTimeZone

  -- make scribes
  let mkScribe :: ColorStrategy -> Handle -> IO Scribe
      mkScribe cs hndl = K.mkHandleScribeWithFormatter (localBracketFormat timeZone) cs hndl (K.permitItem K.DebugS) K.V2
  termScribe <- mkScribe K.ColorIfTerminal stdout
  fileScribe <- mkScribe (K.ColorLog False) fileHandle

  -- register scribes
  MkLoggerData (Just fileHandle)
    <$> ( initLogEnv "webdriver" "eval"
            >>= K.registerScribe "stdout" termScribe K.defaultScribeSettings
            >>= K.registerScribe "file" fileScribe K.defaultScribeSettings
        )

releaseLogger :: LoggerData -> IO ()
releaseLogger MkLoggerData {fileHandle, loggerEnv} =
  K.closeScribes loggerEnv >> maybe (pure ()) hClose fileHandle

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

runLogger :: forall es a. (IOE :> es) => K.LogEnv -> Eff (Logger : es) a -> Eff es a
runLogger lgrEnv action = do
  interpret (katipHandler lgrEnv) action
  where
    katipHandler :: K.LogEnv -> EffectHandler Logger es
    katipHandler le _ = \case
      LogAtSev sev txt ->
        liftIO $ K.runKatipT le $ K.logMsg "app" (toKatipSeverity sev) (K.logStr txt)

withLogger :: (IOE :> es) => FilePath -> Eff (Logger : es) a -> Eff es a
withLogger logFile action =
  bracket (liftIO $ acquireLogger logFile) (liftIO . releaseLogger) $ \lgData ->
    runLogger lgData.loggerEnv action
