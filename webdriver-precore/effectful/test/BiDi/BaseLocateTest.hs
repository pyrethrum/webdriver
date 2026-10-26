module BiDi.BaseLocateTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    BiDiResources,
    interpBiDiLocate,
    runBiDiLocate,
    withBiDiResources,
  )
import Common.BaseLocateTest
  ( testAttributeLocatorVariants,
    testBasicLocatorTypes,
    testClassLocatorVariants,
    testCombinedLocators,
    testExtendedRoleMatchingAlways,
    testExtendedRoleMatchingMiss,
    testExtendedRoleMatchingNever,
    testExtendedRoleMatchingRoleType,
    testLandmarkAndRole,
    testLocateFromElement,
    testMiscAriaRoleTypes,
    testMiscAriaRoleTypesNever,
    testRoleNameAndRole,
    testVisibilityAlways,
    testVisibilityDisambiguate,
    testVisibilityDisambiguateVsNever,
    testVisibilityNever,
  )
import Common.TastyEval qualified as TE
import Common.WebDriver.Effect (WebDriver)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Test.Tasty (TestTree, inOrderTestGroup)
import WebDriverPreCore.BiDi.Protocol (NodeRemoteValue)
import Prelude

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  withBiDiResources runSessionTests
  where
    runSessionTests :: IO BiDiResources -> TestTree
    runSessionTests res =
      inOrderTestGroup
        "Base Locate Tests"
        [ runInprt testLandmarkAndRole,
          runInprt testExtendedRoleMatchingNever,
          runInprt testExtendedRoleMatchingAlways,
          runInprt testExtendedRoleMatchingMiss,
          runInprt testVisibilityAlways,
          runInprt testVisibilityNever,
          runInprt testVisibilityDisambiguate,
          runInprt testBasicLocatorTypes,
          runInprt testClassLocatorVariants,
          runInprt testAttributeLocatorVariants,
          runInprt testRoleNameAndRole,
          runInprt testLocateFromElement,
          runInprt testCombinedLocators,
          runInprt testMiscAriaRoleTypes,
          runInprt testMiscAriaRoleTypesNever,
          testExtendedRoleMatchingRoleType run interp interp,
          testVisibilityDisambiguateVsNever run interp interp
        ]
      where
        runInprt ::
          ( forall elm es.
            (Eq elm, Show elm, Error Text :> es) =>
            (forall a. Eff es a -> IO a) ->
            (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
            TestTree
          ) ->
          TestTree
        runInprt test = test run interp

        run :: forall a. Eff BiDiLocateEffs a -> IO a
        run = runBiDiLocate res

        interp :: forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
        interp = interpBiDiLocate



arggh :: IO TE.TastyResult
arggh = TE.tastyEvalStdOut (Just "/Landmark and Role Tests/&&/Locate by ID/") tests

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "Landmark and Role Tests"

-- Run it all
--- >>> _eval Nothing tests

--- >>> _eval _pattern tests
-- *** Exception: ProgressCancelledException
