-- |
-- Module: WebDriver.Bluefin.App
-- Description: Runner functions to initialize environments and execute Bluefin WebDriver actions
--
-- Provides top-level runners that construct 'HttpEnv' and 'BiDiEnv'
-- handles and execute Bluefin actions in them.
--
-- This mirrors 'WebDriver.RIO.App' but uses Bluefin handles instead of RIO
-- typeclass environments and 'withEffToIO_' for cleanup on error.
module WebDriver.Bluefin.App
  ( -- * Behaviour
    InteractOpts (..),

    -- * HTTP Runners
    runHttp,
    withHttpSession,

    -- * BiDi Runners
    withBiDiSession,

    -- * Re-exports
    defaultDriverInfo,
  )
where

import Prelude hiding (log)

import Data.Text (Text)
import Bluefin.Eff (Eff, (:>), runEff_)
import Bluefin.IO (IOE, withEffToIO_)
import UnliftIO (finally, throwIO)
import WebDriver.Bluefin.Core (Logger (..))
import WebDriver.Bluefin.Logger (Severity (..))
import WebDriver.Bluefin.HTTP.Core
  ( BiDiEnv (..),
    HttpDriverInfo (..),
    HttpEnv (..),
    HttpSessionEnv (..),
    defaultDriverInfo,
  )
import WebDriver.Bluefin.HTTP.Base.Actions (deleteSession, newSessionResponse)
import WebDriverPreCore.BiDiRunner (BiDiUrl, parseBiDiUrl, withBiDi)
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Utils.Timeout (Timeout)

-- ---------------------------------------------------------------------------
-- Behaviour
-- ---------------------------------------------------------------------------

-- | Bundles runtime behaviour parameters shared across HTTP and BiDi runners.
--
-- Analogous to 'WebDriver.RIO.App.InteractOpts' but without a logging
-- config (Bluefin uses the 'IOE' handle for console logging directly).
data InteractOpts = MkInteractOpts
  { -- | How long 'pause' sleeps between actions
    pauseDuration :: Timeout,
    -- | When 'True' log each driver message to stdout
    driverLogging :: Bool
  }

-- ---------------------------------------------------------------------------
-- HTTP Runners
-- ---------------------------------------------------------------------------

-- | Build an 'HttpEnv' and run the action inside 'runEff_'.
--
-- This is the simplest entry point when you only need HTTP (no BiDi).
--
-- Example:
--
-- @
-- runHttp driverInfo $ \io http -> do
--   log io "Hello"
--   status http
-- @
runHttp
  :: HttpDriverInfo
  -> (forall e. IOE e -> HttpEnv e -> Eff e a)
  -> IO a
runHttp driverInfo action =
  runEff_ $ \io -> action io (MkHttpEnv driverInfo io)

-- | Create an HTTP session, run an action with it, then delete the session.
--
-- Uses 'withEffToIO_' so that 'deleteSession' is guaranteed to run even if
-- the action throws.
--
-- @
-- withHttpSession http behaviour caps $ \sess -> do
--   navigateTo sess myUrl
--   getTitle sess
-- @
withHttpSession
  :: (e :> es)
  => HttpEnv e
  -> InteractOpts
  -> Logger e
  -> EC.HttpCapabilities
  -> (HttpSessionEnv e -> Eff es a)
  -> Eff es a
withHttpSession http behaviour logger caps action =
  withEffToIO_ http.envIO $ \toIO -> do
    resp <- toIO $ newSessionResponse http caps
    let logFn
          | behaviour.driverLogging = Just (logger.logFunc Info)
          | otherwise               = Nothing
        info'      = http.httpDriverInfo { driverLogFn = logFn }
        sessionEnv = mkHttpSessionEnv info' behaviour resp http.envIO
    finally
      (toIO $ action sessionEnv)
      (toIO $ deleteSession sessionEnv)

-- ---------------------------------------------------------------------------
-- BiDi Runners
-- ---------------------------------------------------------------------------

-- | Create an HTTP session with BiDi enabled, open the WebSocket, and run a
-- Bluefin action in the resulting 'BiDiEnv'.
--
-- * Creates an HTTP session (the capabilities must have @webSocketUrl = True@).
-- * Parses the WebSocket URL from the session response.
-- * Opens the WebSocket via 'withBiDi'.
-- * Deletes the HTTP session on exit (success or failure).
--
-- The 'BiDiEnv' shares the same 'IOE' handle as 'HttpEnv'.
withBiDiSession
  :: (e :> es)
  => HttpEnv e
  -> InteractOpts
  -> Logger e
  -> EC.HttpCapabilities
  -> (BiDiEnv e -> Eff es a)
  -> Eff es a
withBiDiSession http behaviour logger caps action =
  withEffToIO_ http.envIO $ \toIO -> do
    resp        <- toIO $ newSessionResponse http caps
    let sessionEnv = mkHttpSessionEnv http.httpDriverInfo behaviour resp http.envIO
    let mLogger
          | behaviour.driverLogging = Just (logger.logFunc Info)
          | otherwise               = Nothing
    bidiUrl <- parseBiDiUrlIO resp.websocketUrl
    finally
      ( withBiDi mLogger bidiUrl $ \ioRunner ->
          toIO $ action (MkBiDiEnv ioRunner behaviour.pauseDuration http.envIO)
      )
      (toIO $ deleteSession sessionEnv)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

mkHttpSessionEnv :: HttpDriverInfo -> InteractOpts -> EC.HttpSessionResponse -> IOE e -> HttpSessionEnv e
mkHttpSessionEnv info behaviour resp io =
  MkHttpSessionEnv
    { httpDriverInfo = info,
      httpSession = resp.session,
      pauseDuration = behaviour.pauseDuration,
      envIO = io
    }

parseBiDiUrlIO :: Maybe Text -> IO BiDiUrl
parseBiDiUrlIO Nothing =
  throwIO $ userError "withBiDiSession: driver did not return a WebSocket URL (set webSocketUrl = True in caps)"
parseBiDiUrlIO (Just t) =
  case parseBiDiUrl t of
    Nothing -> throwIO $ userError $ "withBiDiSession: could not parse WebSocket URL: " <> show t
    Just u  -> pure u
