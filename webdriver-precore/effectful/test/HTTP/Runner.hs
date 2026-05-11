module HTTP.Runner (
  withHttp,
  testUrl,
  mkHttpCaps,
  BaseHTTPAction,
  WDSession (..),
  getWDSession,
  closeWDSession,
  -- TODO: Clean this up
  runHttpTest,
  runHttp,
) where

import Common.Runner (runSetup, testUrl)
import Data.Text (Text, unpack)
import Effectful (Effect, Eff, IOE, liftIO, (:>), runEff)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import WebDriver.Effectful
  ( HttpCapabilities,
    HttpSessionInfo (..),
    InteractOpts (..),
    Pause,
    WebDriverHttp,
    acquireHttpSession,
    fromHttpCapability,
    FullCapabilities (..),
    releaseHttpSession,
    runHttpSession,
    runPause,
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
import WebDriver.Effectful.HTTP.Base.Actions (navigateTo)
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..))
import WebDriverPreCore.Test.TestData (megaformaUrl)


mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch  = []
    }

withHttp
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , Pause :> es
        , WebDriverHttp :> es
        )
     => Eff es ()
     )
  -> IO ()
withHttp action =
  runSetup $ \driverInfo opts config ->
    runPause opts.pauseDuration $
      withLogger "eval.log" $
        withHttpSession driverInfo opts (mkHttpCaps config) action


-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

data WDSession = MkWDSession
  { loggerHandle :: LoggerHandle,
    sessionInfo :: HttpSessionInfo
  }

getWDSession :: IO WDSession
getWDSession = 
  runSetup $ 
   \driverInfo opts config -> 
      liftIO $ do
        loggerHandle <- acquireLogger "eval.log"
        sessionInfo <- acquireHttpSession driverInfo (mkHttpCaps config) opts.pauseDuration
        pure MkWDSession {loggerHandle, sessionInfo}

closeWDSession :: WDSession -> IO ()
closeWDSession MkWDSession {loggerHandle, sessionInfo} = do
  releaseHttpSession sessionInfo
  releaseLogger loggerHandle

-- | Run a 'BaseHTTPAction' with shared session and logger resources.
--
-- Retrieves the 'WDSession' from the Tasty resource getter, then runs the
-- action with 'IOE', 'Pause', 'Logger', and 'WebDriverHttp' in scope.
-- Intended for use inside a 'withResource' group via 'baseLocateTests'.
runHttpTest :: IO WDSession -> Text -> BaseHTTPAction -> TestTree
runHttpTest getRes name action = 
  testCase (unpack name) $ 
    getRes >>= \r -> runHttp r action
 

-- runWDSessionTest :: WDSession -> Text -> BaseHTTPAction -> TestTree
runHttp :: WDSession -> BaseHTTPAction -> IO ()
runHttp MkWDSession {loggerHandle, sessionInfo} action = 
    runEff $
      runPause sessionInfo.pauseDuration $
        runLogger loggerHandle $
          runHttpSession sessionInfo action






type BaseHTTPAction = forall (es :: [Effect]). (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es ()
