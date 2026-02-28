{-# LANGUAGE CPP #-}

{-|
Test suite for webdriver-precore-http-runner library

This module contains HTTP demos migrated from the main test suite,
excluding BiDi tests and unit tests.
-}
module Main where

import Control.Exception (SomeException, catch)
import Data.Text (Text, unpack)
import ErrorDemo qualified
import FallbackDemo qualified
import HttpDemo qualified
import HttpDemoUtils (HttpDemo (..), runDemoWithConfig)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import WebDriverPreCore.Test.Config (Config (..), DemoBrowser (..))
import WebDriverPreCore.Test.ConfigLoader (loadConfig)

main :: IO ()
main = do
  testCfg <- loadConfig
  -- defaultMain $ localOption (NumThreads 1) $ tests testCfg
  defaultMain $ tests testCfg

tests :: Config -> TestTree
tests cfg =
  testGroup
    "HTTP Runner Tests"
    [ httpDemos cfg
    ]

-- Single isolated HTTP demo for CI debugging 
httpDemoSingleIsolated :: Config -> TestTree
httpDemoSingleIsolated cfg =
  testGroup
    "HTTP Demos"
    $ fromHttpDemo cfg
      <$> [ 
            HttpDemo.demoForwardBackRefresh
          ]

httpDemos :: Config -> TestTree
httpDemos cfg =
  let thisBrowser = cfg.browser
      expectHttpFail = httpError thisBrowser
   in testGroup
        "HTTP Demos"
        $ fromHttpDemo cfg
          <$> [ HttpDemo.newSessionDemo,
                -- W3C spec requires status.ready=false when sessions exist. Chrome diverges from spec.
                expectHttpFail [Chrome'] "status.ready expected to be False"
                  HttpDemo.driverStatusDemo,
                HttpDemo.demoSendKeysClear,
                HttpDemo.demoForwardBackRefresh,
                HttpDemo.demoWindowHandles,
                HttpDemo.demoWindowSizes,
                HttpDemo.demoElementPageProps,
                HttpDemo.demoTimeouts,
                HttpDemo.demoWindowRecs,
                HttpDemo.demoWindowFindElement,
                HttpDemo.demoFrames,
                HttpDemo.demoShadowDom,
                HttpDemo.demoIsElementSelected,
                HttpDemo.demoGetPageSourceScreenShot,
                HttpDemo.demoPrintPage,
                HttpDemo.demoExecuteScript,
                HttpDemo.demoCookies,
                HttpDemo.demoAlerts,
                HttpDemo.demoPointerNoneActions,
                HttpDemo.demoKeyAndReleaseActions,
                HttpDemo.demoWheelActions,
                HttpDemo.demoError,
                ErrorDemo.errorDemo
-- fallback commands not implemented for legacy
#ifndef LEGACY_TEST
                , FallbackDemo.demoFallbackActions
                , FallbackDemo.demoFallbackCoercions
                , FallbackDemo.demoExtendPost
#endif
          ]

httpTest :: Config -> Text -> [HttpDemo] -> TestTree
httpTest cfg title = testGroup (unpack title) . fmap (fromHttpDemo cfg)

fromHttpDemo :: Config -> HttpDemo -> TestTree
fromHttpDemo cfg demo' = testCase (unpack demo'.name) $ runDemoWithConfig cfg demo'

-- | Browser type for expected failure checking
data BrowserType = Firefox' | Chrome' deriving (Eq, Show)

fromBrowser :: DemoBrowser -> BrowserType
fromBrowser = \case 
  Firefox {} -> Firefox'
  Chrome {} -> Chrome'

expectFailure :: DemoBrowser -> [BrowserType] -> Bool
expectFailure actualBrowser failBrowsers = 
  fromBrowser actualBrowser `elem` failBrowsers

httpError :: DemoBrowser -> [BrowserType] -> Text -> HttpDemo -> HttpDemo
httpError actualBrowser failBrowsers errorFragment demo' =
  if expectFailure actualBrowser failBrowsers then
    case demo' of
      Demo {name, action} ->
        Demo
          { name = name <> " - EXPECTED ERROR: " <> errorFragment,
            action = \demoActions httpActions ->
              catch
                ( do
                    action demoActions httpActions
                    error $ "Expected test to fail with error containing: " <> unpack errorFragment
                )
                (\(_ :: SomeException) -> pure ())
          }
      SessionDemo {name, sessionAction} ->
        SessionDemo
          { name = name <> " - EXPECTED ERROR: " <> errorFragment,
            sessionAction = \session demoActions httpActions ->
              catch
                ( do
                    sessionAction session demoActions httpActions
                    error $ "Expected test to fail with error containing: " <> unpack errorFragment
                )
                (\(_ :: SomeException) -> pure ())
          }
  else demo'
