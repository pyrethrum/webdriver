-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides a runner function to set up a BaseEnv with logging and
-- capabilities, then execute a RIO action in that context.
module WebDriver.RIO.App
  ( runner,
  )
where

import RIO
import WebDriver.RIO.Capabilities (FullCapabilities)
import WebDriver.RIO.Env (BaseEnv (..))

-- | Initialize a BaseEnv context and run a RIO action.
--
-- Takes log options and capabilities, creates a BaseEnv, logs a debug
-- message, then executes the provided action.
runner :: LogOptions -> FullCapabilities cap -> RIO (BaseEnv cap) a -> IO a
runner logOpts caps action =
  withLogFunc logOpts $ \lf -> do
    let env = MkBaseEnv lf caps
    runRIO env $ do
      logDebug "Running action"
      action
