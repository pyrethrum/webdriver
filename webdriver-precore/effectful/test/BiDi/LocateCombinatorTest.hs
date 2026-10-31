module BiDi.LocateCombinatorTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    defBiDiOpts,
    interpBiDiLocate,
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

tests :: TestTree
tests =
  withBiDiResources $ \res ->
    Common.tests (runBiDiLocate res) (interpWith opts)
  where
    opts :: BL.BiDiLocateOpts
    opts = defBiDiOpts {BL.jsRecheckDisplayed = BL.DisplayedCheckNever}

    interpWith :: BL.BiDiLocateOpts -> forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
    interpWith = interpBiDiLocate
