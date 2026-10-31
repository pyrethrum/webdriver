module BiDi.MatchTypeTest where

import BiDi.Runner (defBiDiOpts, interpBiDiLocate, runBiDiLocate, withBiDiResources)
import Common.MatchTypeTest qualified as Common
import Prelude
import Test.Tasty (TestTree)

tests :: TestTree
tests =
  withBiDiResources $ \res ->
    Common.tests (runBiDiLocate res) (interpBiDiLocate defBiDiOpts)
