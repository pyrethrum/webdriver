module HTTP.Runner
  ( withHttp,
    testUrl,
    BaseHTTPEffs,
    HttpTestEff,
    WDSession (..),
    -- TODO: Clean this up
    runHttpTest,
    runHttp,
  )
where

import Common.SessionInit (WDSession (..), closeWDSession, getWDSession, testUrl)
import Data.Text (Text, unpack)
import Effectful (Eff, IOE, runEff, (:>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import UnliftIO (finally)
import WebDriver.Effectful
  ( WaitPrimative,
    WebDriverHttp,
    runWaitPrimative,
    runWebDriverHttp,
  )
import WebDriver.Effectful.HTTP.Base.Interpreter (HttpParams (..))
import WebDriver.Effectful.Logger
  ( Logger,
    runLogger,
  )

withHttp :: (forall es. (IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverHttp :> es) => Eff es ()) -> IO ()
withHttp action = do
  sess <- getWDSession False
  runHttp sess action `finally` closeWDSession sess

-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

-- | Run a 'BaseHTTPAction' with shared session and logger resources.
--
-- Retrieves the 'WDSession' from the Tasty resource getter, then runs the
-- action with 'IOE', 'Pause', 'Logger', and 'WebDriverHttp' in scope.
-- Intended for use inside a 'withResource' group via 'baseLocateTests'.
runHttpTest :: IO WDSession -> Text -> HttpTestEff () -> TestTree
runHttpTest sessPrms name action =
  testCase (unpack name) $
    sessPrms >>= flip runHttp action

-- runWDSessionTest :: WDSession -> Text -> BaseHTTPAction -> TestTree
runHttp :: forall a. WDSession -> HttpTestEff a -> IO a
runHttp MkWDSession {loggerEnv, session, endpoint} action =
  runEff
    $ runWaitPrimative
    $ runLogger loggerEnv
    $ runWebDriverHttp (MkHttpParams {session = session, endpoint = endpoint}) action

type BaseHTTPEffs a = forall es. (IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverHttp :> es) => Eff es a

type HttpTestEff = Eff '[WebDriverHttp, Logger, WaitPrimative, IOE]
