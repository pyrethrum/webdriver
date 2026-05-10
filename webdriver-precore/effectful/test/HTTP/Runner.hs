module HTTP.Runner (
  runHttpTest,
  testUrl,
  mkHttpCaps,
  BaseHTTPAction,
  WDResources (..),
  acquireResources,
  releaseResources,
  runWDTest,
  runMegaformaTest,
) where

import Common.Runner (runSetup, testUrl)
import Data.Text (Text, unpack)
import Effectful (Effect, Eff, IOE, liftIO, (:>))
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
    runHttp,
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

runHttpTest
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , Pause :> es
        , WebDriverHttp :> es
        )
     => Eff es ()
     )
  -> IO ()
runHttpTest action =
  runSetup $ \driverInfo opts config ->
    runPause opts.pauseDuration $
      withLogger "eval.log" $
        withHttpSession driverInfo opts (mkHttpCaps config) action


-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

data WDResources = MkWDResources
  { loggerHandle :: LoggerHandle,
    sessionInfo :: HttpSessionInfo
  }

acquireResources :: IO WDResources
acquireResources = 
  runSetup $ 
   \driverInfo opts config -> 
      liftIO $ do
        loggerHandle <- acquireLogger "eval.log"
        sessionInfo <- acquireHttpSession driverInfo (mkHttpCaps config) opts.pauseDuration

        pure MkWDResources {loggerHandle, sessionInfo}

releaseResources :: WDResources -> IO ()
releaseResources MkWDResources {loggerHandle, sessionInfo} = do
  releaseHttpSession sessionInfo
  releaseLogger loggerHandle

-- | Run a 'BaseHTTPAction' with shared session and logger resources.
--
-- Retrieves the 'WDResources' from the Tasty resource getter, then runs the
-- action with 'IOE', 'Pause', 'Logger', and 'WebDriverHttp' in scope.
-- Intended for use inside a 'withResource' group via 'baseLocateTests'.
runWDTest :: IO WDResources -> Text -> BaseHTTPAction -> TestTree
runWDTest getRes name action = 
  testCase (unpack name) $ do
    MkWDResources {loggerHandle, sessionInfo} <- getRes
    runHttp $
      runPause sessionInfo.pauseDuration $
        runLogger loggerHandle $
          runHttpSession sessionInfo action

runMegaformaTest :: IO WDResources -> Text -> BaseHTTPAction -> TestTree
runMegaformaTest getRes name action =
  runWDTest getRes name $ do
    url <- testUrl megaformaUrl
    navigateTo url
    action



type BaseHTTPAction = forall (es :: [Effect]). (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es ()
