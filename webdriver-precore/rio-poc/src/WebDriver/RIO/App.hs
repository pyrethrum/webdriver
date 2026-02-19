{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides a runner function to set up a BaseEnv with logging and
-- capabilities, then execute a RIO action in that context.
module WebDriver.RIO.App
  ( runHttp,
    withHttpSession,
    WantWebDriverLogging (..),
    defaultEndpoint
  )
where

import RIO
import WebDriver.RIO.Env
  ( HasHttpRunner,
    HttpEnv (..),
    HttpSessionEnv (..),
    getHttpRunner,
    getLogger,
  )
import WebDriver.RIO.HTTP.Base.Actions (deleteSession, getTimeouts, newSession, setTimeouts, status)
import WebDriver.RIO.Logging (LoggerConfig (..), withLogging)
import WebDriverPreCore.Extended.Capabilities as EC
import WebDriverPreCore.HTTP.Protocol (Timeouts (..))
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), HttpRunner (..), mkHttpRunner)

data WantWebDriverLogging = WebDriverLogging | NoWebDriverLogging deriving (Eq, Show)

defaultEndpoint :: HttpEndpoint
defaultEndpoint = MkHttpEndpoint {host = "127.0.0.1", port = 4444}

-- | Run an HTTP action with logging and HTTP runner capabilities.
--
-- Sets up a logging context and HTTP runner, then executes the provided
-- RIO action in an environment constructed by the given function.
runHttp ::
  (MonadUnliftIO m) =>
  -- | Environment constructor function that builds the environment from LogFunc and HttpRunner
  (LogFunc -> HttpRunner m -> env) ->
  -- | Configuration for the logging subsystem
  LoggerConfig ->
  -- | HTTP endpoint (host and port) for the WebDriver server
  HttpEndpoint ->
  -- | Whether to enable verbose HTTP API logging
  WantWebDriverLogging ->
  -- | The RIO action to execute in the configured environment
  RIO env a ->
  m a
runHttp mkEnv loggerConfig httpEndpoint apiLogging httpAction =
  withLogging loggerConfig $ \lf ->
    let runnerLogger =
          case apiLogging of
            WebDriverLogging -> Just $ infoLogger lf
            NoWebDriverLogging -> Nothing

        httpRunner = mkHttpRunner httpEndpoint runnerLogger
        env = mkEnv lf httpRunner
     in runRIO env httpAction

infoLogger :: (MonadIO m) => LogFunc -> Text -> m ()
infoLogger lf = liftIO . runRIO lf . logInfo . display

myAction :: (HasLogFunc env, HasHttpRunner env) => RIO env ()
myAction = do
  logInfo "Running myAction"
  s <- status
  logInfo $ "Status: " <> displayShow s

a :: IO ()
a = runHttp MkHttpEnv Console defaultEndpoint WebDriverLogging myAction

mkHttpSession :: (HasLogFunc env, HasHttpRunner env) => EC.HttpCapabilities -> RIO env HttpSessionEnv
mkHttpSession caps = MkHttpSessionEnv <$> getLogger <*> getHttpRunner <*> newSession caps

-- Perform WebDriver commands using the HTTP runner from the environment

-- | Create a session, run an action with it, then delete the session.
--
-- Uses bracket to ensure the session is always cleaned up even if the action fails.
withHttpSession :: forall env a. 
  (HasHttpRunner env, HasLogFunc env) =>
  -- | Capabilities to request for the session
  EC.HttpCapabilities ->
  -- | Action to run with the session
  RIO HttpSessionEnv a ->
  RIO env a
withHttpSession caps action =
  bracket
    (mkHttpSession caps)
    (run deleteSession)
    (run action)
  where
    run = flip runRIO

