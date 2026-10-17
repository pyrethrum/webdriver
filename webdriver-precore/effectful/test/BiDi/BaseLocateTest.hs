module BiDi.BaseLocateTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    BiDiResources,
    interpBiDiLocate,
    runBiDiLocate,
    withBiDiResources,
  )
import Common.BaseLocateTest qualified as Common
import Common.TastyEval qualified as TE
import Common.WebDriver.Effect (WebDriver)
import Data.Text (Text)
import Effectful (Eff)
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
      inOrderTestGroup "Base Locate Tests"
        [ Common.testLandmarkAndRole run interp,
          Common.testExtendedRoleMatchingNever run interp,
          Common.testExtendedRoleMatchingAlways run interp,
          Common.testExtendedRoleMatchingMiss run interp,
          Common.testExtendedRoleMatchingRoleType run interp interp,
          Common.testVisibilityAlways run interp,
          Common.testVisibilityNever run interp,
          Common.testVisibilityDisambiguate run interp,
          Common.testVisibilityDisambiguateVsNever run interp interp,
          Common.testBasicLocatorTypes run interp,
          Common.testClassLocatorVariants run interp,
          Common.testAttributeLocatorVariants run interp,
          Common.testRoleNameAndRole run interp,
          Common.testLocateFromElement run interp,
          Common.testCombinedLocators run interp,
          Common.testMiscAriaRoleTypes run interp,
          Common.testMiscAriaRoleTypesNever run interp
        ]
      where
        run :: forall a. Eff BiDiLocateEffs a -> IO a
        run = runBiDiLocate res

        interp :: forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
        interp = interpBiDiLocate

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "Landmark and Role Tests"

-- Specific test
--- >>> _eval _pattern tests