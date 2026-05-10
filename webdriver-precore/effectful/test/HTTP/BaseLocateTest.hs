module HTTP.BaseLocateTest where

import Common.Runner (runSetup)
import Effectful (liftIO)
import HTTP.Runner (BaseHTTPAction, mkHttpCaps, testUrl)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase)
import WebDriver.Effectful
  ( HttpSessionInfo (..),
    InteractOpts (..),
    acquireHttpSession,
    releaseHttpSession,
    runHttp,
    runHttpSession,
    runPause,
  )

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

baseLocateTests :: TestTree
baseLocateTests =
  withResource acquireResources releaseResources $ \_getRes ->
    testGroup "Base Locate Tests"
      [
      ]


