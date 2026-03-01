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
  ( -- * Behaviour
    InteractBehaviour (..),

    -- * HTTP Runners
    runHttp,
    withHttpSession,

    -- * BiDi Runners
    withBiDiSession,

    -- * Re-exports
    defaultDriverInfo,
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Effectful (Eff, IOE, (:>), runEff, withSeqEffToIO)
import UnliftIO (finally, throwIO)
import WebDriver.Effectful.Core
  ( Logger,
    Severity (..),
    getLogFn,
  )
import WebDriver.Effectful.HTTP.Core
  ( BiDiInfo (..),
    HttpDriverInfo (..),
    HttpSessionInfo (..),
    WebDriverBiDi,
    WebDriverHttp,
    defaultDriverInfo,
    mkSessionRunner,
    runWebDriverBiDi,
    runWebDriverHttp,
  )
import WebDriverPreCore.BiDiRunner (BiDiUrl, parseBiDiUrl, withBiDi)
import WebDriverPreCore.Error (parseFailToWDException)
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (callWebDriver)
import WebDriverPreCore.Utils.Timeout (Timeout)

-- ---------------------------------------------------------------------------
-- Behaviour
-- ---------------------------------------------------------------------------

-- | Bundles runtime behaviour parameters shared across HTTP and BiDi runners.
data InteractBehaviour = MkInteractBehaviour
  { -- | How long 'pause' sleeps between actions.
    pauseDuration :: Timeout,
    -- | When 'True' log each driver message via the 'Logger' effect.
    driverLogging :: Bool
  }

-- ---------------------------------------------------------------------------
-- HTTP Runners
-- ---------------------------------------------------------------------------

-- | Run an Effectful computation in pure @IO@.
--
-- Provides just 'IOE', so the supplied action can lift any @IO@ operation.
-- Use 'withHttpSession' or 'withBiDiSession' to add WebDriver effects, and
-- 'WebDriver.Effectful.Core.withLogger' \/ 'WebDriver.Effectful.Core.withLogPause'
-- for logging and pacing.
runHttp :: Eff '[IOE] a -> IO a
runHttp = runEff

-- | Create an HTTP session, run an action inside the 'WebDriverHttp' effect,
-- then delete the session on completion or error.
--
-- Uses 'withSeqEffToIO' so that 'deleteSession' runs even when the action
-- throws.
--
-- Stack requirements on the outer @es@:
--
-- * @IOE :> es@    — implicit via 'withSeqEffToIO'
-- * @Logger :> es@ — to route driver-level logging through the existing
--   'Logger' effect (only relevant when @behaviour.driverLogging == True@)
withHttpSession
  :: (IOE :> es, Logger :> es)
  => HttpDriverInfo
  -> InteractBehaviour
  -> EC.HttpCapabilities
  -> Eff (WebDriverHttp : es) a
  -> Eff es a
withHttpSession driverInfo behaviour caps action =
  withSeqEffToIO $ \runInIO -> do
    logFn     <- runInIO (resolveLogFn behaviour)
    let driverInfo' = driverInfo {driverLogFn = logFn}
    resp      <- EC.newHttpSessionResponse (mkRootRunner driverInfo') caps
    let sessionInfo =
          MkHttpSessionInfo
            { driverInfo    = driverInfo',
              session       = resp.session,
              pauseDuration = behaviour.pauseDuration
            }
    finally
      (runInIO (runWebDriverHttp sessionInfo action))
      (HA.deleteSession (mkSessionRunner sessionInfo) sessionInfo.session)

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
  :: (IOE :> es, Logger :> es)
  => HttpDriverInfo
  -> InteractBehaviour
  -> EC.HttpCapabilities
  -> Eff (WebDriverBiDi : es) a
  -> Eff es a
withBiDiSession driverInfo behaviour caps action =
  withSeqEffToIO $ \runInIO -> do
    logFn    <- runInIO (resolveLogFn behaviour)
    let driverInfo' = driverInfo {driverLogFn = logFn}
    resp     <- EC.newHttpSessionResponse (mkRootRunner driverInfo') caps
    let httpInfo =
          MkHttpSessionInfo
            { driverInfo    = driverInfo',
              session       = resp.session,
              pauseDuration = behaviour.pauseDuration
            }
    bidiUrl  <- parseBiDiUrlIO resp.websocketUrl
    finally
      ( withBiDi logFn bidiUrl $ \ioRunner -> do
          let biDiInfo =
                MkBiDiInfo
                  { biDiRunner    = ioRunner,
                    pauseDuration = behaviour.pauseDuration
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
mkRootRunner :: (FromJSON r) => HttpDriverInfo -> HA.Runner IO r
mkRootRunner info cmd =
  callWebDriver info.httpEndpoint info.driverLogFn cmd
    >>= either (throwIO . parseFailToWDException) pure

-- | Extract the driver @IO@ log function from the 'Logger' static effect
-- when @driverLogging@ is enabled.  Returns 'Nothing' otherwise.
resolveLogFn :: (Logger :> es) => InteractBehaviour -> Eff es (Maybe (Text -> IO ()))
resolveLogFn behaviour
  | behaviour.driverLogging = fmap (Just . ($ Info)) getLogFn
  | otherwise               = pure Nothing

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
