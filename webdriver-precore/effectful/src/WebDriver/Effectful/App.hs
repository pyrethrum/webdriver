module WebDriver.Effectful.App
  ( -- * HTTP Session Management
    acquireHttpSession,
    releaseHttpSession,
    withHttpSession,
    -- releaseHttpSession,
    -- runHttpSession,
    -- withHttpSession,

    -- -- * BiDi Session Management
    -- withBiDiSession
  )
where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON)
import Effectful (Eff, IOE, Limit (..), Persistence (..), UnliftStrategy (..), withUnliftStrategy, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (bracket)
import WebDriver.Effectful.HTTP.Base.Interpreter (HttpParams (..))
import WebDriver.Effectful.HTTP.Core
  ( WebDriverBiDi,
    WebDriverHttp,
    runWebDriverBiDi,
    runWebDriverHttp,
  )
import WebDriver.Effectful.Logger (Logger, logDebug)
import WebDriverPreCore.BiDiRunner qualified as BiDiRunner
import WebDriverPreCore.Extended.Capabilities (HttpSessionResponse (..))
import WebDriverPreCore.Extended.Capabilities qualified as EC
import WebDriverPreCore.Extended.HTTP.Base.Actions qualified as HA
import WebDriverPreCore.HttpRunner (Command, HttpEndpoint, ParseFailure, callWebDriver)
import WebDriverPreCore.Utils.Utils (throwLeft)

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
releaseHttpSession :: forall es. (IOE :> es, Logger :> es, Error ParseFailure :> es) => HttpParams -> Eff es ()
releaseHttpSession MkHttpParams {endpoint, session} = HA.deleteSession (httpRunner endpoint) session

withHttpSession ::
  (IOE :> es, Logger :> es, Error ParseFailure :> es) =>
  HttpEndpoint ->
  EC.HttpCapabilities ->
  Eff (WebDriverHttp : es) a ->
  Eff es a
withHttpSession endpoint caps action =
  bracket
    (flip MkHttpParams endpoint . (.session) <$> acquireHttpSession endpoint caps)
    releaseHttpSession
    (flip runWebDriverHttp action)



-- | Create an HTTP session, run an action inside the 'WebDriverHttp' effect,
-- then delete the session on completion or error.
--
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
  withUnliftStrategy (ConcUnlift Persistent Unlimited) $
    BiDiRunner.withBiDi logDebug bidiUrl $
      \runner -> runWebDriverBiDi runner action
