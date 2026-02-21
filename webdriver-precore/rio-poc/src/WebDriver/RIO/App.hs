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
    withHttpSessionEnv,
    WantWebDriverLogging (..),
    defaultEndpoint,
    pause,
  )
where

import RIO
import WebDriver.RIO.Env
  ( HasHttpRunner,
    HasHttpSession,
    HasPauseDuration (getPauseDuration),
    HttpSessionEnv (..),
    getHttpRunner,
    getLogger,
  )
import WebDriver.RIO.HTTP.Base.Actions (deleteSession, newSession)
import WebDriver.RIO.Logging (LoggerConfig (..), withLogging)
import WebDriverPreCore.Extended.Capabilities as EC
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), HttpRunner (..), mkHttpRunner)
import WebDriverPreCore.Utils.Timeout (Timeout (..))

data WantWebDriverLogging = WebDriverLogging | NoWebDriverLogging deriving (Eq, Show)

defaultEndpoint :: HttpEndpoint
defaultEndpoint = MkHttpEndpoint {host = "127.0.0.1", port = 4444}

-- | Run an HTTP action with logging and HTTP runner capabilities.
--
-- Sets up a logging context and HTTP runner, then executes the provided
-- RIO action in an environment constructed by the given function.
runHttp ::
  (MonadUnliftIO m) =>
  -- | Environment constructor function that builds the environment from LogFunc and HttpRunner IO
  (LogFunc -> HttpRunner IO -> env) ->
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
    let runnerLogger :: Maybe (Text -> IO ())
        runnerLogger =
          case apiLogging of
            WebDriverLogging -> Just $ \t -> runRIO lf (logInfo (display t))
            NoWebDriverLogging -> Nothing

        httpRunner = mkHttpRunner httpEndpoint runnerLogger
        env = mkEnv lf httpRunner
     in runRIO env httpAction

mkHttpSession :: (HasLogFunc env, HasHttpRunner env) => EC.HttpCapabilities -> Timeout -> RIO env HttpSessionEnv
mkHttpSession caps timeout' = do
  lf <- getLogger
  runner <- getHttpRunner
  session <- newSession caps
  pure MkHttpSessionEnv
    { logFunc = lf
    , httpRunner = runner
    , httpSession = session
    , pauseDuration = timeout'
    }

-- | Create a session, run an action with it, then delete the session.
--
-- Uses bracket to ensure the session is always cleaned up even if the action fails.
withHttpSession ::
  forall env senv a.
  (HasHttpRunner senv, HasHttpSession senv) =>
  (EC.HttpCapabilities -> Timeout -> RIO env senv) ->
  Timeout ->
  EC.HttpCapabilities ->
  -- | Action to run with the session
  RIO senv a ->
  RIO env a
withHttpSession mkEnv pauseDuration caps action =
  bracket
    (mkEnv caps pauseDuration)
    (run deleteSession)
    (run action)
  where
    run = flip runRIO

withHttpSessionEnv :: (HasLogFunc env, HasHttpRunner env) => Timeout -> EC.HttpCapabilities -> RIO HttpSessionEnv a -> RIO env a
withHttpSessionEnv = withHttpSession mkHttpSession

pause :: (HasPauseDuration env) => RIO env ()
pause = do
  getPauseDuration <$> ask >>= liftIO . threadDelay . (.microseconds)
