module HTTP.BaseLocateTest where

import HTTP.Runner (acquireResources, releaseResources)
import Test.Tasty (TestTree, testGroup, withResource)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

baseLocateTests :: TestTree
baseLocateTests =
  withResource acquireResources releaseResources $ \_getRes ->
    testGroup "Base Locate Tests"
      [
      ]


