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

import Common.Runner (runSetup, testUrl, WDSession (..), mkHttpCaps)
import Data.Text (Text, unpack)
import Effectful (Eff, IOE, liftIO, (:>), runEff)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import UnliftIO (finally)
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
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..))


withHttp :: (forall es. ( IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es ()) -> IO ()
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
      $ runPause sessionInfo.pauseDuration 
      $ runLogger loggerHandle 
      $ runHttpSession sessionInfo action



type BaseHTTPEffs a =  forall es. (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es a
type  HttpTestEff = Eff '[WebDriverHttp, Logger, Pause, IOE]




-- -- from pyrethrum these will probably be split off and go into core or another library
-- -- module later
-- type Action = Eff ApEffs

-- type HasLog es = Out NodeLog :> es

-- type LogEffs a = forall es. (Out NodeLog :> es) => Eff es a

-- type ApEffs = '[RunConfigReader, FileSystem, WebUI, Out NodeLog, IOE]
-- -- type ApEffs = '[FileSystem, WebUI, Out NodeLog, IOE]

-- -- Define a labeled Reader effect for RunConfig
-- type RunConfigReader = Labeled "runConfig" (LR.Reader RunConfig) 

-- -- type ApConstraints es = (FileSystem :> es, Out NodeLog :> es, Error FSException :> es, IOE :> es)
-- -- type AppEffs a = forall es. (FileSystem :> es, Out NodeLog :> es, Error FSException :> es, IOE :> es) => Eff es a

-- type SuiteRunner = Suite 
--   -> Filters RunConfig FixtureConfig 
--   -> RunConfig 
--   -> ThreadCount 
--   -> L.LogActions (L.Log L.ExePath AE.NodeLog)
--   -> IO ()

-- ioInterpreter :: RunConfig -> AE.LogSink -> Action a -> IO a
-- ioInterpreter rc sink ap =
--   ap
--     & LR.runReader @"runConfig" rc
--     & FIO.runFileSystem
--     & WDIO.runWebDriver
--     & runOut sink
--     & runEff
