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
    defaultDriverInfo,
    pause,
  )
where

import RIO
import WebDriver.RIO.Env
  ( HasHttpDriverInfo,
    HasHttpSession,
    HasPauseDuration (getPauseDuration),
    HttpDriverInfo (..),
    HttpSessionEnv (..),
    getHttpDriverInfo,
    getLogger,
  )
import WebDriver.RIO.HTTP.Base.Actions (deleteSession, newSession)
import WebDriver.RIO.Logging (LoggerConfig (..), withLogging)
import WebDriverPreCore.Extended.Capabilities as EC
import WebDriverPreCore.HttpRunner (HttpEndpoint (..))
import WebDriverPreCore.Utils.Timeout (Timeout (..))

defaultDriverInfo :: HttpDriverInfo
defaultDriverInfo = MkHttpDriverInfo
  { httpEndpoint = MkHttpEndpoint {host = "127.0.0.1", port = 4444},
    driverLogging = False
  }

-- | Run an HTTP action with logging and HTTP driver info in the environment.
--
-- Sets up a logging context then constructs the environment from a 'LogFunc'
-- and 'HttpDriverInfo', before running the provided RIO action.
runHttp ::
  (MonadUnliftIO m) =>
  -- | Environment constructor from LogFunc and HttpDriverInfo
  (LogFunc -> HttpDriverInfo -> env) ->
  -- | Configuration for the logging subsystem
  LoggerConfig ->
  -- | HTTP driver info (endpoint + logging flag)
  HttpDriverInfo ->
  -- | The RIO action to execute in the configured environment
  RIO env a ->
  m a
runHttp mkEnv loggerConfig driverInfo httpAction =
  withLogging loggerConfig $ \lf ->
    runRIO (mkEnv lf driverInfo) httpAction

mkHttpSession :: (HasLogFunc env, HasHttpDriverInfo env) => EC.HttpCapabilities -> Timeout -> RIO env HttpSessionEnv
mkHttpSession caps timeout' = do
  lf <- getLogger
  driverInfo <- getHttpDriverInfo
  session <- newSession caps
  pure MkHttpSessionEnv
    { logFunc = lf
    , httpDriverInfo = driverInfo
    , httpSession = session
    , pauseDuration = timeout'
    }

-- | Create a session, run an action with it, then delete the session.
--
-- Uses bracket to ensure the session is always cleaned up even if the action fails.
withHttpSession ::
  forall env senv a.
  (HasLogFunc senv, HasHttpDriverInfo senv, HasHttpSession senv) =>
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

withHttpSessionEnv :: (HasLogFunc env, HasHttpDriverInfo env) => Timeout -> EC.HttpCapabilities -> RIO HttpSessionEnv a -> RIO env a
withHttpSessionEnv = withHttpSession mkHttpSession

pause :: (HasPauseDuration env) => RIO env ()
pause = do
  getPauseDuration <$> ask >>= liftIO . threadDelay . (.microseconds)
