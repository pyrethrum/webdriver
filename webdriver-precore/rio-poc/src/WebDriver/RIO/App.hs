{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides a runner function to set up a BaseEnv with logging and
-- capabilities, then execute a RIO action in that context.
module WebDriver.RIO.App
  ( runWebDriver,
    withBiDiRunner,
  )
where

import RIO
import WebDriver.RIO.Env (BaseEnv (..), BiDiEnv (..), HasCapabilities (..), capabilitiesL)
import WebDriver.RIO.Logging (LoggerConfig, withLogging)
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.Extended.Capabilities (FullCapabilitiesRequest)

-- | Initialize a BaseEnv context and run a RIO action.
--
-- Takes logger config and capabilities, creates a BaseEnv, logs a debug
-- message, then executes the provided action.
runWebDriver :: LoggerConfig -> FullCapabilitiesRequest -> RIO BaseEnv a -> IO a
runWebDriver loggerConfig caps action =
  withLogging loggerConfig $ \lf -> do
    let env = MkBaseEnv lf caps
    runRIO env $ do
      logDebug "Running action"
      action

-- | Run an action in BiDiEnv context with the given runner.
--
-- Extracts LogFunc and capabilities from the current environment, constructs
-- a BiDiEnv with the given runner, then executes the provided action.
runBiDi :: BiDiRunner -> RIO BiDiEnv a -> RIO BaseEnv a
runBiDi runner action = do
  caps <- getCapabilities
  lf <- getLogger
  let biDiEnv = MkBiDiEnv lf caps runner
  runRIO biDiEnv action

-- | Create a BiDiRunner from capabilities and run an action in BiDiEnv context.
--
-- Extracts logger and capabilities from BaseEnv, creates a BiDiRunner with
-- the provided initialization function, then executes the action in a BiDiEnv
-- context. Uses bracket to ensure cleanup happens even if the action fails.
{-
withBiDiRunner ::
  (BiDiRunner -> IO ()) ->
  RIO BiDiEnv a ->
  RIO BaseEnv a
withBiDiRunner cleanup action = do
  caps <- getCapabilities
  bracket
    (liftIO $ mkRunner caps)
    (liftIO . cleanup)
    $ \runner -> runBiDi runner action
  -}
  

getCapabilities :: HasCapabilities env => RIO env FullCapabilitiesRequest
getCapabilities = view capabilitiesL

getLogger :: HasLogFunc env => RIO env LogFunc
getLogger = view logFuncL