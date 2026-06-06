module HTTP.LocateCombinatorTest where

import Common.Utils
  ( autoId,
    chkAttributeEq,
    chkElms,
    chkElmsM,
    chkEmpty,
    chkEq,
    chkLocException,
    chkSingleton,
    defOpts,
    locateAllFromElementHttp,
    locateAllHttp,
    locateFromElementHttp,
    locateHttp,
  )
import Common.Utils qualified as CU
import Data.Text (Text, unpack)
import Effectful
import HTTP.Runner (BaseHTTPEffs, WDSession, closeWDSession, getWDSession, runHttp, runHttpTest, testUrl)
import Prelude
import System.Environment (withArgs)
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Predicate (expect, satisfies, (.$))
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, defaultMain, inOrderTestGroup, testGroup, withResource)
import Test.Tasty.Falsify (ExpectFailure (DontExpectFailure), TestOptions (..), Verbose (..), gen, info, testPropertyWith)
import Test.Tasty.Falsify qualified as F
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId, URL)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locators.Internal (CaseSensitivity (..))
import WebDriverPreCore.Test.TestData

-- >>> _eval tests
-- *** Exception: ExitSuccess
tests :: TestTree
tests =
  withResource getWDSession closeWDSession runSessionTests
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests ses =
    inOrderTestGroup "Locate Combinator Tests"
      [ testGroup "TODO: Add tests here" []
      ]
