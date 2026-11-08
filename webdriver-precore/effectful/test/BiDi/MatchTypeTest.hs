module BiDi.MatchTypeTest where

import BiDi.Runner (defBiDiOpts, runBiDiLocate, withBiDiResources)
import Common.MatchTypeTest qualified as Common
import Prelude
import Test.Tasty (TestTree)
import Common.WebDriver.InterpreterBiDi (runWebDriverWithRootContext)

tests :: TestTree
tests =
  withBiDiResources $ \res ->
    Common.tests (runBiDiLocate res) (runWebDriverWithRootContext defBiDiOpts)
