{-|
Module: WebDriver.RIO.Logging
Description: RIO-based logging configuration for WebDriver

Provides logging setup functions for console, file, and combined
console+file output using RIO's 'LogFunc'.

Each function has a simple variant using verbose defaults (debug level,
timestamps, verbose format) and a primed (') variant accepting a
@'LogOptions' -> 'LogOptions'@ modifier for customisation.

== Example Usage

@
-- simple console logging
withConsoleLog $ \\lf ->
  runRIO lf $ logInfo "hello"

-- console + file logging
withConsoleAndFileLog "test.log" $ \\lf ->
  runRIO lf $ do
    logDebug "debug msg"
    logInfo "info msg"

-- customised: disable timestamps on console, keep defaults for file
withConsoleAndFileLog' (setLogUseTime False) id "test.log" $ \\lf ->
  runRIO lf $ logInfo "hello"
@
-}
module WebDriver.RIO.Logging
  ( -- * Simple logging (verbose defaults)
    withConsoleLog,
    withFileLog,
    withConsoleAndFileLog,

    -- * Custom logging
    withConsoleLog',
    withFileLog',
    withConsoleAndFileLog',
  )
where

import RIO
import RIO.Text qualified as T

-- | Apply verbose defaults: debug level, timestamps, verbose format
defaultOptions :: LogOptions -> LogOptions
defaultOptions opts =
  opts
    & setLogMinLevel LevelDebug
    & setLogVerboseFormat True
    & setLogUseTime True

-- | Apply verbose defaults and disable terminal ANSI codes for file output
defaultFileOptions :: LogOptions -> LogOptions
defaultFileOptions opts =
  opts
    & defaultOptions
    & setLogTerminal False

-- | Console logging with verbose defaults
withConsoleLog :: MonadUnliftIO m => (LogFunc -> m a) -> m a
withConsoleLog = withConsoleLog' id

-- | Console logging with a custom options modifier applied after verbose defaults
withConsoleLog'
  :: MonadUnliftIO m
  => (LogOptions -> LogOptions)
  -> (LogFunc -> m a)
  -> m a
withConsoleLog' modifyOpts action = do
  opts <- logOptionsHandle stdout True
  withLogFunc (opts & defaultOptions & modifyOpts) action

-- | File logging with verbose defaults
withFileLog :: MonadUnliftIO m => Text -> (LogFunc -> m a) -> m a
withFileLog = withFileLog' id

-- | File logging with a custom options modifier
withFileLog'
  :: MonadUnliftIO m
  => (LogOptions -> LogOptions)
  -> Text
  -> (LogFunc -> m a)
  -> m a
withFileLog' modifyOpts logFileName action =
  bracket (openLogFile logFileName) hClose $ \h -> do
    opts <- logOptionsHandle h True
    withLogFunc (opts & defaultFileOptions & modifyOpts) action

-- | Console and file logging with verbose defaults
withConsoleAndFileLog :: MonadUnliftIO m => Text -> (LogFunc -> m a) -> m a
withConsoleAndFileLog = withConsoleAndFileLog' id id

-- | Console and file logging with custom options modifiers
withConsoleAndFileLog'
  :: MonadUnliftIO m
  => (LogOptions -> LogOptions)
  -> (LogOptions -> LogOptions)
  -> Text
  -> (LogFunc -> m a)
  -> m a
withConsoleAndFileLog' modifyConsoleOpts modifyFileOpts logFileName action =
  bracket (openLogFile logFileName) hClose $ \h -> do
    consoleOpts <- logOptionsHandle stdout True
    fileOpts <- logOptionsHandle h True
    let consoleOpts' = consoleOpts & defaultOptions & modifyConsoleOpts
        fileOpts' = fileOpts & defaultFileOptions & modifyFileOpts
    withLogFunc consoleOpts' $ \consoleLF ->
      withLogFunc fileOpts' $ \fileLF ->
        action (consoleLF <> fileLF)

-- | Open a log file with line buffering
openLogFile :: MonadIO m => Text -> m Handle
openLogFile fileName = do
  h <- liftIO $ openFile (T.unpack fileName) WriteMode
  hSetBuffering h LineBuffering
  pure h
