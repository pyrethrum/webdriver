-- |
-- Module: WebDriver.Effectful.App
-- Description: Runner functions to initialize effects and execute Effectful WebDriver actions
--
-- Provides top-level runners that stack interpreter effects and execute
-- 'Eff' actions.
--
-- This mirrors 'WebDriver.Bluefin.App' but uses Effectful algebraic effects
-- instead of explicit Bluefin compound handles.  The key technique is
-- 'withSeqEffToIO', which provides a @runInIO :: forall r. Eff es r -> IO r@
-- function so resource-management brackets run in ordinary @IO@ while still
-- being able to call back into the outer effect stack.
module WebDriver.Effectful.App
  ( -- * HTTP Session Management
    acquireHttpSession,
    releaseHttpSession,
    runHttpSession,
    withHttpSession,

    -- * BiDi Session Management
    acquireBiDiSession,
    releaseBiDiSession,
    withBiDiSession,

    -- * Re-exports
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Effectful (Eff, IOE, (:>), withSeqEffToIO)
import UnliftIO (bracket, finally, throwIO)
import WebDriver.Effectful.HTTP.Core
  ( BiDiInfo (..),
    HttpSessionInfo (..),
    WebDriverBiDi,
    WebDriverHttp,
    mkSessionRunner,
    runWebDriverBiDi,
    runWebDriverHttp,
  )
import WebDriverPreCore.BiDiRunner (BiDiUrl, parseBiDiUrl, withBiDi)
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (HttpEndpoint, callWebDriver)
import WebDriverPreCore.HttpRunner qualified as R
import WebDriverPreCore.Utils.Timeout (Timeout)
import WebDriverPreCore.Error (parseFailToWDException)

-- ---------------------------------------------------------------------------
-- HTTP Session Management
-- ---------------------------------------------------------------------------

-- | Create an HTTP session and return the session info handle.
--
-- This is the acquire half of the acquire/release pair. Use with
-- 'releaseHttpSession' in test framework resource management (e.g.
-- @Test.Tasty.withResource@) or within your own brackets.
--
-- For convenience, 'withHttpSession' provides a bracket version.
acquireHttpSession ::
  HttpEndpoint ->
  (Text -> IO ()) ->
  EC.HttpCapabilities ->
  IO HttpSessionInfo
acquireHttpSession endpoint logger caps = do
  let runner = callWebDriverRunner endpoint logger
  sessionResponse <- EC.newHttpSession runner caps
  pure
    MkHttpSessionInfo
      { endpoint,
        logger,
        sessionResponse
      }

-- | Delete the HTTP session associated with an 'HttpSessionInfo' handle.
--
-- This is the release half of the acquire/release pair.
releaseHttpSession :: HttpSessionInfo -> IO ()
releaseHttpSession MkHttpSessionInfo {endpoint, logger, sessionResponse} =
  HA.deleteSession (callWebDriverRunner endpoint logger) sessionResponse.session

-- | Run an effectful action inside the 'WebDriverHttp' effect using an
-- existing 'HttpSessionInfo' handle.
runHttpSession :: forall es a. (IOE :> es) => HttpSessionInfo -> Eff (WebDriverHttp : es) a -> Eff es a
runHttpSession = runWebDriverHttp


-- | Create an HTTP session, run an action inside the 'WebDriverHttp' effect,
-- then delete the session on completion or error.
--
-- Uses 'withSeqEffToIO' so that 'releaseHttpSession' runs even when the
-- action throws.
--
-- This is a convenience function that combines 'acquireHttpSession',
-- 'runHttpSession', and 'releaseHttpSession'. For test framework resource
-- management, use the acquire/release functions directly.
withHttpSession ::
  (IOE :> es) =>
  HttpEndpoint ->
  (Text -> IO ()) ->
  EC.HttpCapabilities ->
  Eff (WebDriverHttp : es) a ->
  Eff es a
withHttpSession endpoint logger caps action =
  withSeqEffToIO $ \runInIO -> do
    bracket
      (acquireHttpSession endpoint logger caps)
      releaseHttpSession
      (runInIO . flip runHttpSession action)

-- ---------------------------------------------------------------------------
-- BiDi Session Management
-- ---------------------------------------------------------------------------

-- | Create an HTTP session with BiDi enabled, open the WebSocket, and return
-- both handles.
--
-- This is the acquire half of the acquire/release pair. Use with
-- 'releaseBiDiSession' in test framework resource management.
--
-- The capabilities must have @webSocketUrl = True@ enabled.
--
-- NOTE: Currently not implemented because 'withBiDi' from
-- WebDriverPreCore.BiDiRunner uses a bracket-style API that doesn't expose
-- separate acquire/release functions. This would require refactoring the
-- BiDiRunner module to support that pattern.
acquireBiDiSession ::
  HttpEndpoint ->
  (Text -> IO ()) ->
  Timeout ->
  EC.HttpCapabilities ->
  IO (HttpSessionInfo, BiDiInfo)
acquireBiDiSession endpoint logger pauseDuration caps = do
  httpInfo <- acquireHttpSession endpoint logger pauseDuration caps
  _bidiUrl <- parseBiDiUrlIO httpInfo.sessionResponse.websocketUrl
  -- Note: withBiDi creates the WebSocket connection but doesn't close it
  -- until the continuation returns. We need to refactor this to return
  -- the BiDiRunner directly or use a different approach.
  --
  -- For now, this is a placeholder that shows the intent.
  -- A proper implementation would require changes to WebDriverPreCore.BiDiRunner
  -- to expose an acquire/release style API.
  error "acquireBiDiSession: not yet implemented - requires BiDiRunner refactoring"

-- | Close the BiDi WebSocket and delete the HTTP session.
--
-- This is the release half of the acquire/release pair.
releaseBiDiSession :: (HttpSessionInfo, BiDiInfo) -> IO ()
releaseBiDiSession (httpInfo, _biDiInfo) = do
  -- TODO: close BiDi WebSocket connection
  releaseHttpSession httpInfo

-- | Create an HTTP session with BiDi enabled, open the WebSocket, and run an
-- action inside the 'WebDriverBiDi' effect.
--
-- * Creates an HTTP session (the capabilities must have @webSocketUrl = True@).
-- * Parses the WebSocket URL from the session response.
-- * Opens the WebSocket via 'withBiDi'.
-- * Deletes the HTTP session on exit (success or failure).
--
-- This is a convenience function. For test framework resource management,
-- you'll need to use the approach in the commented code below once
-- 'acquireBiDiSession' is properly implemented.
withBiDiSession ::
  (IOE :> es) =>
  HttpEndpoint ->
  (Text -> IO ()) ->
  Timeout ->
  EC.HttpCapabilities ->
  Eff (WebDriverBiDi : es) a ->
  Eff es a
withBiDiSession endpoint logger pauseDuration caps action =
  withSeqEffToIO $ \runInIO -> do
    let runner = mkRootRunner endpoint logger
    sessionResponse <- EC.newHttpSession runner caps
    let httpInfo =
          MkHttpSessionInfo
            { endpoint,
              logger,
              session = sessionResponse.session,
              pauseDuration,
              sessionResponse
            }
    bidiUrl <- parseBiDiUrlIO sessionResponse.websocketUrl
    finally
      ( withBiDi (Just logger) bidiUrl $ \ioRunner -> do
          let biDiInfo =
                MkBiDiInfo
                  { biDiRunner = ioRunner,
                    pauseDuration
                  }
          runInIO (runWebDriverBiDi biDiInfo action)
      )
      (releaseHttpSession httpInfo)

-- | Parse a BiDi WebSocket URL, throwing 'IOError' on failure.
parseBiDiUrlIO :: Maybe Text -> IO BiDiUrl
parseBiDiUrlIO = maybe
    (throwIO $
      userError
        "withBiDiSession: driver did not return a WebSocket URL \
        \(set webSocketUrl = True in capabilities)")
    \t ->
    case parseBiDiUrl t of
      Nothing -> throwIO $ userError $ "withBiDiSession: could not parse WebSocket URL: " <> show t
      Just u  -> pure u
