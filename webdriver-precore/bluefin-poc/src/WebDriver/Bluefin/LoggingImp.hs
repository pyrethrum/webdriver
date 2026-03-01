{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: WebDriver.Bluefin.LoggingImp
-- Description: Katip-based logging implementation for Bluefin WebDriver tests
--
-- Provides 'Severity' and 'withKatipLogFunc', which sets up a Katip
-- environment that writes structured log messages to both the terminal
-- (with colour when supported) and a local file (@eval.log@).
--
-- Typical usage (via 'WebDriver.Bluefin.Core.withLogger'):
--
-- @
-- withKatipLogFunc "eval.log" $ \logIO -> do
--   logIO Info "session started"
-- @
module WebDriver.Bluefin.LoggingImp
  ( -- * Severity
    Severity (..),

    -- * Katip-based log-function builder
    withKatipLogFunc,
  )
where

import Control.Exception (bracket)
import Data.Text (Text)
import Katip
  ( ColorStrategy (..),
    Verbosity (..),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    logMsg,
    logStr,
    mkFileScribe,
    mkHandleScribe,
    permitItem,
    registerScribe,
    runKatipT,
  )
import qualified Katip as K
import System.IO (stdout)

-- ---------------------------------------------------------------------------
-- Severity
-- ---------------------------------------------------------------------------

-- | Severity levels for log messages.
data Severity = Debug | Info | Warn | Error
  deriving (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Map our 'Severity' to Katip's severity type.
toKatipSev :: Severity -> K.Severity
toKatipSev Debug = K.DebugS
toKatipSev Info  = K.InfoS
toKatipSev Warn  = K.WarningS
toKatipSev Error = K.ErrorS

-- ---------------------------------------------------------------------------
-- Katip log-function builder
-- ---------------------------------------------------------------------------

-- | Run an action with a Katip-backed log function that writes to both the
-- terminal (colour when supported) and @logFile@.
--
-- The file is opened in 'AppendMode' so successive runs accumulate output.
-- All Katip scribes and the log environment are closed cleanly via 'bracket'
-- even when the action throws an exception.
--
-- @
-- withKatipLogFunc "eval.log" $ \logIO ->
--   logIO Info "session started"
-- @
withKatipLogFunc
  :: FilePath
  -- ^ Path to the log file (e.g. @"eval.log"@).
  -> ((Severity -> Text -> IO ()) -> IO a)
  -- ^ Action receiving the log function.
  -> IO a
withKatipLogFunc logFile action = do
  termScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem K.DebugS) V2
  fileScribe  <- mkFileScribe logFile (permitItem K.DebugS) V2
  le0 <- initLogEnv "webdriver" "eval"
  let acquire = do
        le1 <- registerScribe "stdout" termScribe defaultScribeSettings le0
        registerScribe "file" fileScribe defaultScribeSettings le1
  bracket acquire closeScribes $ \le ->
    action $ \sev txt ->
      runKatipT le $ logMsg "app" (toKatipSev sev) (logStr txt)
