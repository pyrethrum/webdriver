module HTTP.Runner (
  withHttp,
  testUrl,
  BaseHTTPEffs,
  HttpTestEff,
  WDSession (..),
  -- TODO: Clean this up
  runHttpTest,
  runHttp,
) where

-- TODO: Update to use new acquire/release pattern
-- import Common.Runner (runSetup, testUrl, WDSession (..), mkHttpCaps)
import Common.Runner (testUrl, WDSession (..), mkHttpCaps)
import Data.Text (Text, unpack)
import Effectful (Eff, IOE, liftIO, (:>), runEff)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import UnliftIO (finally)
import WebDriver.Effectful
  ( HttpCapabilities,
    HttpSessionInfo (..),
    WaitPrimative,
    WebDriverHttp,
    acquireHttpSession,
    fromHttpCapability,
    FullCapabilities (..),
    releaseHttpSession,
    runHttpSession,
    runWaitPrimative,
    withHttpSession,
  )
import WebDriver.Effectful.Logger
  ( LoggerHandle,
    Logger,
    acquireLogger,
    releaseLogger,
    runLogger,
    withLogger,
  )
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..))


withHttp :: (forall es. ( IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverHttp :> es) => Eff es ()) -> IO ()
withHttp action =
  runSetup $ \driverInfo opts config ->
    runPause opts.pauseDuration $
         withLogger "eval.log" $
           withHttpSession driverInfo opts (mkHttpCaps config) action

-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------


-- | Run a 'BaseHTTPAction' with shared session and logger resources.
--
-- Retrieves the 'WDSession' from the Tasty resource getter, then runs the
-- action with 'IOE', 'Pause', 'Logger', and 'WebDriverHttp' in scope.
-- Intended for use inside a 'withResource' group via 'baseLocateTests'.
runHttpTest :: IO WDSession -> Text -> HttpTestEff () -> TestTree
runHttpTest getRes name action = 
  testCase (unpack name) $ 
    getRes >>= \r -> runHttp r action
 

-- runWDSessionTest :: WDSession -> Text -> BaseHTTPAction -> TestTree
runHttp :: forall a. WDSession -> HttpTestEff a -> IO a
runHttp MkWDSession {loggerHandle, sessionInfo} action = 
    runEff 
      $ runWaitPrimative
      $ runLogger loggerHandle 
      $ runHttpSession sessionInfo action

type BaseHTTPEffs a =  forall es. (IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverHttp :> es) => Eff es a
type  HttpTestEff = Eff '[WebDriverHttp, Logger, WaitPrimative, IOE]
