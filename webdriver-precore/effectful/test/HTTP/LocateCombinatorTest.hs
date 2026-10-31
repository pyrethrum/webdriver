module HTTP.LocateCombinatorTest where

import Common.LocateCombinatorTest qualified as Common
import Common.SessionInit (closeWDSession, getWDSession)
import Common.TastyEval qualified as TE
import Common.Utils (defHttpOpts)
import Common.WebDriver.Effect (WebDriver)
import Common.WebDriver.InterpreterHttp qualified as Interp
import Data.Text (Text)
import Effectful (Eff)
import HTTP.Runner (HttpBaseEffs, runWith)
import Prelude
import Test.Tasty (TestTree, withResource)
import WebDriverPreCore.HTTP.Protocol ( ElementId )
import WebDriverPreCore.Extended.Locate qualified as L

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  withResource (getWDSession False) closeWDSession $ \ses ->
    Common.tests (runWith ses) (interpHttp opts) 
  where
    opts :: L.HttpLocateOpts
    opts = defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever}

    interpHttp :: L.HttpLocateOpts -> forall a. Eff (WebDriver ElementId : HttpBaseEffs) a -> Eff HttpBaseEffs a
    interpHttp = Interp.runWebDriverHttp

-- locators mixture of css and xpath

_pattern :: Maybe Text
-- _pattern = Just "OR with contains under"
_pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEval

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess
