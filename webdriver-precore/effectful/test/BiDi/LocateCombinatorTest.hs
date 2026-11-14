module BiDi.LocateCombinatorTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    defBiDiOpts,
    runBiDiLocate,
    withBiDiResources,
  )
import Common.LocateCombinatorTest qualified as Common
import Common.WebDriver.Effect (WebDriver)
import Effectful (Eff)
import Prelude
import Test.Tasty (TestTree)
import WebDriverPreCore.BiDi.Protocol (NodeRemoteValue)
import WebDriverPreCore.Extended.BiDi.Locate qualified as BL
import Common.WebDriver.InterpreterBiDi (runWebDriverWithRootContext)
import WebDriverPreCore.Extended.Common.Locate (DisplayedCheck(..))

tests :: TestTree
tests =
  withBiDiResources $ \res ->
    Common.tests (runBiDiLocate res) (interpWith opts)
  where
    opts :: BL.BiDiLocateOpts
    opts = defBiDiOpts {BL.jsRecheckDisplayed = DisplayedCheckNever}

    interpWith :: BL.BiDiLocateOpts -> forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
    interpWith = runWebDriverWithRootContext
