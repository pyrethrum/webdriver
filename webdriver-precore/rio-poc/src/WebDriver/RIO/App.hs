{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides a runner function to set up a BaseEnv with logging and
-- capabilities, then execute a RIO action in that context.
module WebDriver.RIO.App
  ( runHttp,
  -- withHttpSession,
  -- newSession,
  -- deleteSession,
  )
where

import RIO
import RIO.Text (pack)
import WebDriver.RIO.Env (BiDiEnv (..), HasHttpRunner, HttpEnv (..))
import WebDriver.RIO.HTTP.Base.Actions (status)
import WebDriver.RIO.Logging (LoggerConfig (..), withLogging)
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), HttpRunner, mkHttpRunner)

data WantWebDriverLogging = WebDriverLogging | NoWebDriverLogging deriving (Eq, Show)

defaultEndpoint :: HttpEndpoint
defaultEndpoint = MkHttpEndpoint {host = "127.0.0.1", port = 4444}

-- | Run an HTTP action with logging and HTTP runner capabilities.
--
-- Sets up a logging context and HTTP runner, then executes the provided
-- RIO action in an environment constructed by the given function.
runHttp ::
  (MonadUnliftIO m) =>
  (LogFunc -> HttpRunner m -> env) ->
  -- ^ Environment constructor function that builds the environment from LogFunc and HttpRunner
  LoggerConfig ->
  -- ^ Configuration for the logging subsystem
  HttpEndpoint ->
  -- ^ HTTP endpoint (host and port) for the WebDriver server
  WantWebDriverLogging ->
  -- ^ Whether to enable verbose HTTP API logging
  RIO env a ->
  -- ^ The RIO action to execute in the configured environment
  m a
runHttp mkEnv loggerConfig httpEndpoint apiLogging httpAction =
  withLogging loggerConfig $ \lf ->
    let runnerLogger =
          case apiLogging of
            WebDriverLogging -> Just $ runRIO lf . logInfo . display . ("HTTP: " <>)
            NoWebDriverLogging -> Nothing

        httpRunner = mkHttpRunner httpEndpoint runnerLogger
        env = mkEnv lf httpRunner
     in runRIO env httpAction

myAction :: (HasLogFunc env, HasHttpRunner env) => RIO env ()
myAction = do
  logInfo "Running myAction"
  s <- status
  logInfo $ "Status: " <> displayShow s

a :: IO ()
a = runHttp MkHttpEnv Console defaultEndpoint WebDriverLogging myAction

-- Perform WebDriver commands using the HTTP runner from the environment

-- | Create a session, run an action with it, then delete the session.
--
-- Uses bracket to ensure the session is always cleaned up.
-- withHttpSession ::
--   (HasHttpRunner env, HasLogFunc env) =>
--   RIO HttpSessionEnv a ->
--   RIO env a
-- withHttpSession action = do
--   httpRunner <- getHttpRunner
--   logFunc <- getLogger
--   bracket
--     ((.sessionId) <$> newSession)
--     (deleteSession)
--     (\httpSessionId -> runRIO (MkHttpSessionEnv {logFunc, httpRunner, httpSessionId}) action)

-- withHttpSession
-- runBiDi :: LoggerConfig -> BiDiCapabilities -> RIO BiDiEnv a -> IO a
-- runBiDi loggerConfig bidiCaps bidiAction = undefined
--    withLogging loggerConfig $ \lf -> do
--     runRIO (MkLoggerEnv lf) $ do
--       let socketLogger = Just logInfo
--       bracket
--   let biDiEnv = MkBiDiEnv lf bidiCaps
--   runRIO biDiEnv action

-- | Run a BiDi session with typed commands
-- withBiDi
--   :: Maybe (Text -> IO ())  -- ^ Optional logger>
--   -> BiDiUrl
--   -> (BiDiRunner -> IO ())
--   -> IO ()
-- withBiDi mLogger bidiUrl action =
--   withBiDiBase mLogger bidiUrl $ \sa ->
--     action (mkBiDiRunner sa)

-- -- | Initialize a BaseEnv context and run a RIO action.
-- --
-- -- Takes logger config and capabilities, creates a BaseEnv, logs a debug
-- -- message, then executes the provided action.
-- runWebDriver :: LoggerConfig -> FullCapabilitiesRequest -> RIO BaseEnv a -> IO a
-- runWebDriver loggerConfig caps action =
--   withLogging loggerConfig $ \lf -> do
--     let env = MkBaseEnv lf caps Nothing
--     runRIO env $ do
--       logDebug "Running action"
--       action

-- -- | Run an action in BiDiEnv context with the given runner.
-- --
-- -- Extracts LogFunc and capabilities from the current environment, constructs
-- -- a BiDiEnv with the given runner, then executes the provided action.
-- runBiDi :: BiDiRunner -> RIO BiDiEnv a -> RIO BaseEnv a
-- runBiDi runner action = do
--   caps <- getCapabilities
--   mCapsResp <- getCapabilitiesResponse
--   lf <- getLogger
--   let biDiEnv = MkBiDiEnv lf caps mCapsResp runner
--   runRIO biDiEnv action

-- -- | Create a BiDiRunner from capabilities and run an action in BiDiEnv context.
--
-- Extracts logger and capabilities from BaseEnv, creates a BiDiRunner using
-- withBiDi from the BiDiRunner module, then executes the action in a BiDiEnv
-- context. The BiDi URL comes from the webSocketUrl in the capabilities response.
-- -- Uses finally to ensure cleanup happens evRIO BaseEnven if the action fails.

-- withBiDiRunner ::
--   (BiDiRunner -> IO ()) ->
--   RIO BiDiEnv () ->
--   RIO BaseEnv ()
-- withBiDiRunner cleanup action = do
--   caps <- getCapabilities
--   mCapsResp <- getCapabilitiesResponse
--   capsResp <- case mCapsResp of
--     Nothing -> error "No capabilities response available. Create a session first."
--     Just cr -> pure cr
--   bidiUrl <- getBiDiUrl capsResp
--   lf <- getLogger

--   -- Create logger function from RIO logInfo
--   let logger txt = runRIO (MkBaseEnv lf caps mCapsResp) $ logInfo $ display txt

--   liftIO $ BiDiRunner.withBiDi (Just logger) bidiUrl $ \runner -> do
--     finally
--       (runRIO (MkBaseEnv lf caps mCapsResp) $ runBiDi runner action)
--       (cleanup runner)

-- -- | Extract BiDi URL from capabilities response
-- getBiDiUrl :: (MonadIO m) => SessionResponse -> m BiDiUrl
-- getBiDiUrl capsResp = case capsResp of
--   BiDiSessionResponse {bidiCapabilities = MkBiDiCapabilities {bidiWebSocketUrl = Just wsUrl}} ->
--     case parseBiDiUrl wsUrl of
--       Just url -> pure url
--       Nothing -> error $ "Failed to parse WebSocket URL: " <> show wsUrl
--   BiDiSessionResponse {bidiCapabilities = MkBiDiCapabilities {bidiWebSocketUrl = Nothing}} ->
--     error "No WebSocket URL in BiDi capabilities response"
--   HttpSessionResponse {} ->
--     error "Cannot get BiDi URL from HTTP capabilities response"

-- getCapabilities :: HasCapabilities env => RIO env FullCapabilitiesRequest
-- getCapabilities = view capabilitiesL

-- getCapabilitiesResponse :: HasCapabilitiesResponse env => RIO env (Maybe SessionResponse)
-- getCapabilitiesResponse = view capabilitiesResponseL

-- getLogger :: HasLogFunc env => RIO env LogFunc
-- getLogger = view logFuncL