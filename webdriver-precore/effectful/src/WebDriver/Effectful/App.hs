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
  ( -- * opts

    -- * HTTP Runners
    releaseHttpSession,
    runHttpSession,
    withHttpSession,

    -- * BiDi Runners
    withBiDiSession,

    -- * Re-exports
    defaultDriverInfo,
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Effectful (Eff, IOE, Limit (..), Persistence (..), UnliftStrategy (..), (:>), withEffToIO, withSeqEffToIO)
import UnliftIO (bracket, finally, throwIO)
import WebDriver.Effectful.Logger (Logger, Severity (..), getLogFn)
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
import WebDriverPreCore.Error (parseFailToWDException)
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (callWebDriver, HttpEndpoint)
import WebDriverPreCore.Utils.Timeout (Timeout)
import WebDriverPreCore.Extended.Capabilities (HttpSessionResponse(..))


-- ---------------------------------------------------------------------------
-- opts
-- ---------------------------------------------------------------------------



-- | Run an effectful action inside the 'WebDriverHttp' effect using an
-- existing 'HttpSessionInfo' handle.
runHttpSession :: forall es a. (IOE :> es) => HttpSessionInfo -> Eff (WebDriverHttp : es) a -> Eff es a
runHttpSession = runWebDriverHttp


-- | Delete the HTTP session associated with an 'HttpSessionInfo' handle.
releaseHttpSession :: HttpSessionInfo -> IO ()
releaseHttpSession si =
  HA.deleteSession (mkSessionRunner si) si.session


-- | Create an HTTP session, run an action inside the 'WebDriverHttp' effect,
-- then delete the session on completion or error.
--
-- Uses 'withSeqEffToIO' so that 'releaseHttpSession' runs even when the
-- action throws.
--
-- Stack requirements on the outer @es@:
--
-- * @IOE :> es@    — implicit via 'withSeqEffToIO'
-- * @Logger :> es@ — to route driver-level logging through the existing
--   'Logger' effect (only relevant when @opts.driverLogging == True@)
--
-- Use 'acquireHttpSession' \/ 'releaseHttpSession' as an acquire\/release
-- pair (e.g. with 'Test.Tasty.withResource') and 'runHttpSession' to inject
-- the 'WebDriverHttp' effect into each test.
withHttpSession
  :: (IOE :> es)
  => Timeout
  -> HttpDriverInfo
  -> EC.HttpCapabilities
  -> Eff (WebDriverHttp : es) a
  -> Eff es a
withHttpSession pauseDuration driverInfo caps action =
  withSeqEffToIO $ \runInIO -> do
    bracket
      (acquireHttpSession driverInfo caps pauseDuration)
      releaseHttpSession
      (runInIO . flip runHttpSession action)

-- ---------------------------------------------------------------------------
-- BiDi Runners
-- ---------------------------------------------------------------------------

-- | Create an HTTP session with BiDi enabled, open the WebSocket, and run an
-- action inside the 'WebDriverBiDi' effect.
--
-- * Creates an HTTP session (the capabilities must have @webSocketUrl = True@).
-- * Parses the WebSocket URL from the session response.
-- * Opens the WebSocket via 'withBiDi'.
-- * Deletes the HTTP session on exit (success or failure).
--
-- Requires:
--
-- * @IOE :> es@    — for @IO@ operations
-- * @Logger :> es@ — to optionally pipe driver logging
withBiDiSession
  :: (IOE :> es)
  => Timeout
  -> HttpDriverInfo
  -> EC.HttpCapabilities
  -> Eff (WebDriverBiDi : es) a
  -> Eff es a
withBiDiSession pauseDuration driverInfo caps action =
  withEffToIO (ConcUnlift Persistent Unlimited) $ \runInIO -> do
    sessionResponse <- EC.newHttpSessionResponse (mkRootRunner driverInfo) caps
    let httpInfo =
          MkHttpSessionInfo
            { driverInfo    = driverInfo,
              session       = sessionResponse.session,
              pauseDuration = pauseDuration,
              sessionResponse
            }
    bidiUrl  <- parseBiDiUrlIO sessionResponse.websocketUrl
    finally
      ( withBiDi driverInfo.driverLogFn bidiUrl $ \ioRunner -> do
          let biDiInfo =
                MkBiDiInfo
                  { biDiRunner    = ioRunner,
                    pauseDuration = pauseDuration
                  }
          runInIO (runWebDriverBiDi biDiInfo action)
      )
      (HA.deleteSession (mkSessionRunner httpInfo) httpInfo.session)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Build an IO-level root (no-session) runner from 'HttpDriverInfo'.
--
-- Used to create a session via @newSession@ before an 'HttpSessionInfo'
-- object exists.  'mkSessionRunner' in "WebDriver.Effectful.HTTP.Core"
-- provides the session-scoped equivalent used by the interpreter.
mkRootRunner :: (FromJSON r) => HttpEndpoint -> (Text -> IO ()) -> HA.Runner IO r
mkRootRunner endpoint logger cmd =
  callWebDriver httpEndpoint logger cmd
    >>= either (throwIO . parseFailToWDException) pure


-- | Parse a BiDi WebSocket URL, throwing 'IOError' on failure.
parseBiDiUrlIO :: Maybe Text -> IO BiDiUrl
parseBiDiUrlIO Nothing =
  throwIO $
    userError
      "withBiDiSession: driver did not return a WebSocket URL \
      \(set webSocketUrl = True in capabilities)"
parseBiDiUrlIO (Just t) =
  case parseBiDiUrl t of
    Nothing -> throwIO $ userError $ "withBiDiSession: could not parse WebSocket URL: " <> show t
    Just u  -> pure u
