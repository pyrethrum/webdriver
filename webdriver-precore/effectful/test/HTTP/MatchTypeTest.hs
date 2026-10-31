module HTTP.MatchTypeTest where

import Common.MatchTypeTest qualified as Common
import Common.SessionInit (closeWDSession, getWDSession)
import Common.TastyEval qualified as TE
import Common.Utils (defHttpOpts)
import Common.WebDriver.InterpreterHttp qualified as Interp
import Data.Text (Text)
import HTTP.Runner (runWith)
import Prelude
import Test.Tasty (TestTree, withResource)

tests :: TestTree
tests =
  withResource (getWDSession False) closeWDSession $ \ses ->
    Common.tests (runWith ses) (Interp.runWebDriverHttp defHttpOpts)

_pattern :: Maybe Text
-- _pattern = Just "Contains Match - Partial MatchType"
_pattern = Nothing

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEval

--- >>> _eval _pattern tests
-- *** Exception: ExitSuccess
