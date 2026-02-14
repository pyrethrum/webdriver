{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.App
-- Description: Runner to initialize BaseEnv and execute RIO actions
--
-- Provides a runner function to set up a BaseEnv with logging and
-- capabilities, then execute a RIO action in that context.
module WebDriver.RIO.App
  ( runHttp,
  )
where

import RIO
import WebDriver.RIO.Env
import WebDriver.RIO.Logging (LoggerConfig, withLogging)
import WebDriverPreCore.BiDiRunner (BiDiRunner, BiDiUrl, parseBiDiUrl)
import WebDriverPreCore.BiDiRunner qualified as BiDiRunner
import WebDriverPreCore.Extended.Capabilities
import WebDriverPreCore.HttpRunner (mkHttpRunner)

runHttp :: LoggerConfig -> Text -> Word16 -> RIO HttpEnv a -> IO a
runHttp loggerConfig host port httpAction =
  withLogging loggerConfig $ \lf ->
    let logger = runRIO lf . logInfo . display
        httpEnv = MkHttpEnv lf (mkHttpRunner host port (Just logger))
     in runRIO httpEnv $ do
          logInfo "Successfully started WebDriverRIO"
          httpAction

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
--   :: Maybe (Text -> IO ())  -- ^ Optional logger
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