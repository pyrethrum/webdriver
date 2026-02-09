-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides a runner function to set up a BaseEnv with logging and
-- capabilities, then execute a RIO action in that context.
module WebDriver.RIO.App
  ( runWebDriver,
  )
where

import RIO
import WebDriver.RIO.Capabilities (FullCapabilities)
import WebDriver.RIO.Env (BaseEnv (..))
import WebDriver.RIO.Logging (LoggerConfig, withLogging)

-- | Initialize a BaseEnv context and run a RIO action.
--
-- Takes logger config and capabilities, creates a BaseEnv, logs a debug
-- message, then executes the provided action.
runWebDriver :: LoggerConfig -> FullCapabilities cap -> RIO (BaseEnv cap) a -> IO a
runWebDriver loggerConfig caps action =
  withLogging loggerConfig $ \lf -> do
    let env = MkBaseEnv lf caps
    runRIO env $ do
      logDebug "Running action"
      action
