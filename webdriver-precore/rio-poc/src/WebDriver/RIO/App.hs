{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides runner functions to set up environments with logging and
-- driver info, then execute RIO actions in those contexts.
module WebDriver.RIO.App
  ( runHttp,
    withHttpSession,
    withHttpSessionEnv,
    defaultDriverInfo,
    pause,
    -- * BiDi Runners
    runBiDi,
    withBiDiEnv,
    withBiDiSession,
  )
where

import RIO
import WebDriver.RIO.Env
  ( BiDiEnv (..),
    HttpSessionEnv (..),
  )
import WebDriver.RIO.HTTP.Core
  ( HasHttpDriverInfo,
    HasHttpSession,
    HasPauseDuration (getPauseDuration),
    HttpDriverInfo (..),
    getHttpDriverInfo,
    getLogger,
  )
import WebDriver.RIO.HTTP.Base.Actions (deleteSession, newSessionResponse)
import WebDriver.RIO.Logging (LoggerConfig (..), withLogging)
import WebDriverPreCore.Extended.Capabilities as EC
import WebDriverPreCore.HttpRunner (HttpEndpoint (..))
import WebDriverPreCore.Utils.Timeout (Timeout (..))
import WebDriverPreCore.BiDiRunner (BiDiUrl, parseBiDiUrl)
import WebDriverPreCore.BiDiRunner qualified as BiDiRunner

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
  resp <- newSessionResponse caps
  pure MkHttpSessionEnv
    { logFunc = lf
    , httpDriverInfo = driverInfo
    , httpSession = resp.session
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

-- ---------------------------------------------------------------------------
-- BiDi runners
-- ---------------------------------------------------------------------------

-- | Run a BiDi action with full logging setup.
--
-- Sets up a logging context, then opens a WebSocket connection to 'BiDiUrl'
-- and runs the provided 'RIO BiDiEnv' action inside a 'BiDiEnv'.
--
-- Use 'driverLogging' to enable verbose per-message logging via RIO.
runBiDi ::
  (MonadUnliftIO m) =>
  -- | Configuration for the logging subsystem
  LoggerConfig ->
  -- | Whether to enable driver-level message logging
  Bool ->
  -- | The BiDi WebSocket URL (obtained from a prior HTTP session response)
  BiDiUrl ->
  -- | The RIO action to execute in the BiDi environment
  RIO BiDiEnv a ->
  m a
runBiDi loggerConfig driverLogging bidiUrl action =
  withLogging loggerConfig $ \lf ->
    liftIO $ withBiDiEnvIO lf driverLogging bidiUrl action

-- | Run a BiDi action within an existing RIO environment.
--
-- Inherits the 'LogFunc' from the outer environment, opens a WebSocket
-- connection to 'BiDiUrl', builds a 'BiDiEnv', and runs @action@ in it.
withBiDiEnv ::
  (HasLogFunc env) =>
  -- | Whether to enable driver-level message logging
  Bool ->
  -- | The BiDi WebSocket URL (obtained from a prior HTTP session response)
  BiDiUrl ->
  -- | The RIO action to execute in the BiDi environment
  RIO BiDiEnv a ->
  RIO env a
withBiDiEnv driverLogging bidiUrl action = do
  lf <- view logFuncL
  liftIO $ withBiDiEnvIO lf driverLogging bidiUrl action

-- | Create an HTTP session with BiDi enabled, open the WebSocket, and run
-- a 'RIO BiDiEnv' action.  The HTTP session is deleted on exit (or error).
--
-- The provided 'EC.HttpCapabilities' must have @webSocketUrl = Just True@ so
-- that the driver returns a WebSocket URL in the session response.
withBiDiSession ::
  (HasLogFunc env, HasHttpDriverInfo env) =>
  -- | Whether to enable driver-level message logging
  Bool ->
  -- | Capabilities – must enable @webSocketUrl@
  EC.HttpCapabilities ->
  -- | Action to run in the BiDi environment
  RIO BiDiEnv a ->
  RIO env a
withBiDiSession driverLogging caps action = do
  lf <- view logFuncL
  driverInfo <- getHttpDriverInfo
  resp <- newSessionResponse caps
  let sessionEnv = MkHttpSessionEnv
        { logFunc = lf
        , httpDriverInfo = driverInfo
        , httpSession = resp.session
        , pauseDuration = MkTimeout 0
        }
  flip finally (runRIO sessionEnv deleteSession) $ do
    wsText <- case resp.websocketUrl of
      Nothing -> throwIO $ userError "withBiDiSession: driver did not return a WebSocket URL (is webSocketUrl=true in caps?)"
      Just t  -> pure t
    bidiUrl <- case parseBiDiUrl wsText of
      Nothing -> throwIO $ userError $ "withBiDiSession: could not parse WebSocket URL: " <> show wsText
      Just u  -> pure u
    withBiDiEnv driverLogging bidiUrl action

-- Internal helper: open a BiDi connection and run an action in BiDiEnv.
withBiDiEnvIO :: LogFunc -> Bool -> BiDiUrl -> RIO BiDiEnv a -> IO a
withBiDiEnvIO lf driverLogging bidiUrl action = do
  resultRef <- newIORef (Left (error "withBiDiEnvIO: result unset" :: SomeException))
  BiDiRunner.withBiDi mLogger bidiUrl $ \runner -> do
    r <- try (runRIO (MkBiDiEnv {logFunc = lf, biDiRunner = runner}) action)
    writeIORef resultRef r
  readIORef resultRef >>= either throwIO pure
  where
    mLogger
      | driverLogging = Just $ \t -> runRIO lf (logInfo (display t))
      | otherwise = Nothing
