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
import UnliftIO (bracket, finally, throwIO, Exception)
import WebDriver.Effectful.HTTP.Core
  ( BiDiIORunner,
    HttpSessionInfo (..),
    WebDriverBiDi,
    WebDriverHttp,
    runWebDriverBiDi,
    runWebDriverHttp,
  )
import WebDriverPreCore.BiDiRunner (BiDiUrl, parseBiDiUrl, parseBiDiUrlProperty, withBiDi)
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (HttpEndpoint, callWebDriver, Command)
import WebDriverPreCore.HttpRunner qualified as R
import WebDriverPreCore.Utils.Timeout (Timeout)
import WebDriverPreCore.Error (parseFailToWDException)
import Control.Exception (throw)
import Control.Monad ((>=>))
import WebDriverPreCore.HTTP.Protocol (SessionResponse)

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
acquireHttpSession :: HttpEndpoint -> (Text -> IO ()) -> EC.HttpCapabilities -> IO HttpSessionInfo
acquireHttpSession endpoint logger caps = 
  MkHttpSessionInfo endpoint logger <$> EC.newHttpSession (httpIORunner endpoint logger) caps

httpIORunner :: forall a. HttpEndpoint -> (Text -> IO ()) -> Command a -> IO a
httpIORunner endpoint logger = callWebDriver endpoint logger >=> ioThrow

ioThrow :: Either l r -> IO r
ioThrow  =  either throwIO pure

-- | Delete the HTTP session associated with an 'HttpSessionInfo' handle.
--
-- This is the release half of the acquire/release pair.
releaseHttpSession :: HttpSessionInfo -> IO ()
releaseHttpSession MkHttpSessionInfo {endpoint, logger, sessionResponse} =
  HA.deleteSession (httpIORunner endpoint logger) sessionResponse.session

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

-- get a bidi session runner from an existing HTTP session
acquireBiDiSession :: HttpSessionInfo -> IO BiDiIORunner
acquireBiDiSession httpInfo = do
  bidiUrl <- ioThrow $ parseBiDiUrlProperty httpInfo.sessionResponse.websocketUrl
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
              sessionResponse
            }
    bidiUrl <- parseBiDiUrl sessionResponse.websocketUrl
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
