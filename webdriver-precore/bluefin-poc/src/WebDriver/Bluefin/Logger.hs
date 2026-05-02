{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: WebDriver.Bluefin.Logger
-- Description: Katip-based logging setup for the Bluefin WebDriver POC
--
-- Provides 'Severity' and 'withKatipLogFunc', which sets up a Katip
-- environment that writes structured log messages to both the terminal
-- (with colour when the output is a TTY) and a local file (e.g. @eval.log@).
--
-- Timestamps are displayed in the __local system time zone__, obtained once
-- via 'getCurrentTimeZone' at scribe-creation time.
-- Both scribes and the 'LogEnv' are closed safely via 'bracket' even when
-- the action throws an exception.
--
-- Typical usage:
--
-- @
-- withKatipLogFunc "eval.log" $ \logIO -> do
--   logIO Info "session started"
-- @
module WebDriver.Bluefin.Logger
  ( -- * Severity
    Severity (..),

    -- * Katip-based log-function builder
    withKatipLogFunc,
  )
where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder, fromString, fromText)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Katip
  ( ColorStrategy (..),
    Item (..),
    ItemFormatter,
    Verbosity (..),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    logMsg,
    logStr,
    mkHandleScribeWithFormatter,
    permitItem,
    registerScribe,
    renderSeverity,
    runKatipT,
    unLogStr,
  )
import Katip.Scribes.Handle (colorBySeverity)
import qualified Katip as K
import System.IO (BufferMode (..), IOMode (..), hClose, hSetBuffering, openFile, stdout)

-- ---------------------------------------------------------------------------
-- Severity
-- ---------------------------------------------------------------------------

-- | Severity levels for log messages used in the Bluefin POC.
data Severity = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

toKatipSev :: Severity -> K.Severity
toKatipSev Debug = K.DebugS
toKatipSev Info  = K.InfoS
toKatipSev Warn  = K.WarningS
toKatipSev Error = K.ErrorS

-- ---------------------------------------------------------------------------
-- Local-time formatter
-- ---------------------------------------------------------------------------

-- | Katip 'ItemFormatter' that displays timestamps in the given 'TimeZone'.
localBracketFormat :: TimeZone -> ItemFormatter a
localBracketFormat tz withColor _verb Item {..} =
  brackets nowStr
    <> brackets (fromText (colorBySeverity withColor _itemSeverity (renderSeverity _itemSeverity)))
    <> fromText " "
    <> unLogStr _itemMessage
  where
    localTime = utcToLocalTime tz _itemTime
    nowStr    = fromString $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime

brackets :: Builder -> Builder
brackets m = "[" <> m <> "]"

-- ---------------------------------------------------------------------------
-- Katip log-function builder
-- ---------------------------------------------------------------------------

-- | Run an action with a Katip-backed log function that writes to both the
-- terminal (colour when the output is a TTY) and @logFile@.
withKatipLogFunc
  :: FilePath
  -> ((Severity -> Text -> IO ()) -> IO a)
  -> IO a
withKatipLogFunc logFile action = do
  tz <- getCurrentTimeZone
  bracket (openFile logFile WriteMode) hClose $ \fh -> do
    hSetBuffering fh LineBuffering
    termScribe <- mkHandleScribeWithFormatter (localBracketFormat tz) ColorIfTerminal stdout (permitItem K.DebugS) V2
    fileScribe  <- mkHandleScribeWithFormatter (localBracketFormat tz) (ColorLog False) fh   (permitItem K.DebugS) V2
    le0 <- initLogEnv "webdriver" "eval"
    let acquire = do
          le1 <- registerScribe "stdout" termScribe defaultScribeSettings le0
          registerScribe "file" fileScribe defaultScribeSettings le1
    bracket acquire closeScribes $ \le ->
      action $ \sev txt ->
        runKatipT le $ logMsg "app" (toKatipSev sev) (logStr txt)
