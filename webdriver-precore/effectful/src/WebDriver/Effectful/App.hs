module WebDriver.Effectful.App
  ( -- * HTTP Session Management
    acquireHttpSession,
    -- releaseHttpSession,
    -- runHttpSession,
    -- withHttpSession,

    -- -- * BiDi Session Management
    -- withBiDiSession
  )
where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Effectful (Eff, IOE, withSeqEffToIO, (:>))
import UnliftIO (bracket)
import WebDriver.Effectful.HTTP.Core
  ( 
    WebDriverBiDi,
    WebDriverHttp,
    runWebDriverBiDi,
    runWebDriverHttp,
  )
import WebDriver.Effectful.Logger (Logger, logDebug)
import WebDriverPreCore.BiDiRunner qualified as BiDiRunner
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (Command, HttpEndpoint, callWebDriver, ParseFailure)
import WebDriverPreCore.Utils.Utils (throwLeft)
import WebDriver.Effectful.HTTP.Base.Interpreter (HttpParams(..))
import Effectful.Error.Static (Error, throwError)
import WebDriverPreCore.Extended.Capabilities (HttpSessionResponse(..))

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
acquireHttpSession :: forall es. (IOE :> es, Logger :> es, Error ParseFailure :> es) => HttpEndpoint -> EC.HttpCapabilities -> Eff es HttpSessionResponse
acquireHttpSession endpoint caps =
   EC.newHttpSession (httpRunner endpoint) caps

httpRunner :: forall es a. (IOE :> es, Logger :> es, Error ParseFailure :> es, FromJSON a) => HttpEndpoint -> Command a -> Eff es a
httpRunner endpoint = callWebDriver endpoint logDebug >=> throwLeft throwError



-- | Delete the HTTP session associated with an 'HttpSessionInfo' handle.
--
-- This is the release half of the acquire/release pair.
releaseHttpSession :: HttpSessionInfo -> IO ()
releaseHttpSession MkHttpSessionInfo {endpoint, logger, sessionResponse} =
  HA.deleteSession (httpIORunner endpoint logger) sessionResponse.session

{-
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
withHttpSession endpoint caps action =
  withSeqEffToIO $ \runInIO -> do
    let logger txt = runInIO (logInfo txt)
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
withBiDiSession bidiUrl action =
  withSeqEffToIO $ \runInIO -> do
    let logger txt = runInIO (logInfo txt)
    BiDiRunner.withBiDi logger bidiUrl $
      \ioRunner -> runInIO (runWebDriverBiDi ioRunner action)

      -}
