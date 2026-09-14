module WebDriver.Effectful.App
  ( -- * HTTP Session Management
    acquireHttpSession,
    releaseHttpSession,
    runHttpSession,
    withHttpSession,

    -- * BiDi Session Management
    withBiDiSession
  )
where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Effectful (Eff, IOE, withSeqEffToIO, (:>))
import UnliftIO (bracket)
import WebDriver.Effectful.HTTP.Core
  ( HttpSessionInfo (..),
    WebDriverBiDi,
    WebDriverHttp,
    runWebDriverBiDi,
    runWebDriverHttp,
  )
import WebDriver.Effectful.Logger (Logger, Severity (..), getLogFn)
import WebDriverPreCore.BiDiRunner qualified as BiDiRunner
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (Command, HttpEndpoint, callWebDriver)
import WebDriverPreCore.Utils.Utils (ioThrow)

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

httpIORunner :: forall a. (FromJSON a) => HttpEndpoint -> (Text -> IO ()) -> Command a -> IO a
httpIORunner endpoint logger = callWebDriver endpoint logger >=> ioThrow

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

withHttpSession ::
  (IOE :> es, Logger :> es) =>
  HttpEndpoint ->
  EC.HttpCapabilities ->
  Eff (WebDriverHttp : es) a ->
  Eff es a
withHttpSession endpoint caps action = do
  -- Extract the IO logger before entering withSeqEffToIO.
  logFn <- getLogFn
  let logger = logFn InfoS
  -- uses 'withSeqEffToIO' so that 'releaseHttpSession' runs even when the action throws.
  withSeqEffToIO $ \runInIO -> do
    bracket
      (acquireHttpSession endpoint logger caps)
      releaseHttpSession
      (runInIO . flip runHttpSession action)

-- ---------------------------------------------------------------------------
-- BiDi Session Management
-- ---------------------------------------------------------------------------

-- | Close the BiDi WebSocket and delete the HTTP session.
withBiDiSession ::
  (IOE :> es, Logger :> es) =>
  BiDiRunner.BiDiUrl ->
  Eff (WebDriverBiDi : es) a ->
  Eff es a
withBiDiSession bidiUrl action = do
  logFn <- getLogFn
  let logger = logFn InfoS
  withSeqEffToIO $ \runInIO ->
    BiDiRunner.withBiDi logger bidiUrl $
      \ioRunner -> runInIO (runWebDriverBiDi ioRunner action)
