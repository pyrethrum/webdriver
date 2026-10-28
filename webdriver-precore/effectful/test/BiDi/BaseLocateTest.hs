module BiDi.BaseLocateTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    BiDiResources,
    defBiDiOpts,
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
    testExtendedRoleMatchingRoleType,
    testLandmarkAndRole,
    testLocateFromElement,
    testMiscAriaRoleTypes,
    testMiscAriaRoleTypesNever,
    testRoleNameAndRole,
    testViewportDisplayCheck,
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
import WebDriverPreCore.Extended.BiDi.Locate qualified as BL
import Prelude

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  inOrderTestGroup
    "Base Locate Test Suite"
    [ withBiDiResources viewportTests,
      withBiDiResources runSessionTests
    ]
  where
    viewportTests :: IO BiDiResources -> TestTree
    viewportTests res =
      testViewportDisplayCheck (runBiDiLocate res) (interpBiDiLocate defBiDiOpts)

    runSessionTests :: IO BiDiResources -> TestTree
    runSessionTests res =
      inOrderTestGroup
        "Base Locate Tests"
        [ runInprt testLandmarkAndRole,
          runInprt testExtendedRoleMatchingAlways,
          runInprt $ testExtendedRoleMatchingMiss False,
          testVisibilityAlways run (interpWith defBiDiOpts),
          testVisibilityNever
            run
            (interpWith (defBiDiOpts {BL.jsRecheckDisplayed = BL.DisplayedCheckNever})),
          testVisibilityDisambiguate
            run
            (interpWith (defBiDiOpts {BL.jsRecheckDisplayed = BL.DisplayedCheckDisambiguateUnique})),
          runInprt testBasicLocatorTypes,
          runInprt testClassLocatorVariants,
          runInprt testAttributeLocatorVariants,
          runInprt testRoleNameAndRole,
          runInprt testLocateFromElement,
          runInprt testCombinedLocators,
          runInprt testMiscAriaRoleTypes,
          testMiscAriaRoleTypesNever
            run
            (interpWith (defBiDiOpts {BL.jsRecheckDisplayed = BL.DisplayedCheckNever})),
          testExtendedRoleMatchingRoleType run (interpWith defBiDiOpts) (interpWith defBiDiOpts),
          testVisibilityDisambiguateVsNever
            run
            (interpWith (defBiDiOpts {BL.jsRecheckDisplayed = BL.DisplayedCheckDisambiguateUnique}))
            (interpWith (defBiDiOpts {BL.jsRecheckDisplayed = BL.DisplayedCheckNever}))
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
        runInprt test = test run (interpWith defBiDiOpts)

        run :: forall a. Eff BiDiLocateEffs a -> IO a
        run = runBiDiLocate res

        interpWith :: BL.BiDiLocateOpts -> forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
        interpWith = interpBiDiLocate



arggh :: IO TE.TastyResult
arggh = TE.tastyEvalStdOut (Just "/Landmark and Role Tests/&&/Img - div with explicit role override/") tests

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOutOnFailure

_pattern :: Maybe Text
-- _pattern = Just "/Landmark and Role Tests/"
_pattern = Nothing



--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess
