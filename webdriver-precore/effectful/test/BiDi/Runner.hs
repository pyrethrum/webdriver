module BiDi.Runner where

import Common.SessionInit (WDSession (..), closeWDSession, getWDSession)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff, (:>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import UnliftIO (finally)
import WebDriver.Effectful
import WebDriverPreCore.BiDi.Protocol
  ( KeySourceAction (..),
    PointerCommonProperties (..),
  )
import WebDriver.Effectful.Logger (Logger, runLogger)
import WebDriverPreCore.Utils.Utils (ioThrow)

-- | Acquire a BiDi-enabled session, run the action, then release resources.
withBidi :: (forall es. (IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverBiDi :> es) => Eff es ()) -> IO ()
withBidi action = do
  sess <- getWDSession True
  runBiDi sess action `finally` closeWDSession sess

-- | Run a 'BiDiTestEff' action as a Tasty test using a session supplied by the
-- caller (typically via 'Test.Tasty.withResource').
runBiDiTest :: IO WDSession -> Text -> BiDiTestEff () -> TestTree
runBiDiTest sessPrms name action =
  testCase (unpack name) $
    sessPrms >>= flip runBiDi action

-- | Run a 'WebDriverBiDi' action against an existing 'WDSession'.
--
-- The session must have been acquired with 'getWDSession True' so its
-- 'websocketUrl' is populated. This is the resource-free counterpart used by
-- 'runBiDiTest' inside a Tasty 'withResource' group.
runBiDi :: forall a. WDSession -> BiDiTestEff a -> IO a
runBiDi MkWDSession {loggerEnv, websocketUrl} action = do
  bidiUrl <- ioThrow (parseBiDiUrlProperty websocketUrl)
  runEff
    $ runWaitPrimative
    $ runLogger loggerEnv
    $ withBiDiSession bidiUrl
    $ action

type BiDiTestEff = Eff '[WebDriverBiDi, Logger, WaitPrimative, IOE]

-- | Minimal pointer properties with all optional fields set to 'Nothing'.
defaultPointerProps :: PointerCommonProperties
defaultPointerProps =
  MkPointerCommonProperties
    { width              = Nothing,
      height             = Nothing,
      pressure           = Nothing,
      tangentialPressure = Nothing,
      twist              = Nothing,
      altitudeAngle      = Nothing,
      azimuthAngle       = Nothing
    }

-- | Convert a 'Char' to a pair of keyDown\/keyUp 'KeySourceAction's.
charToKeys :: Char -> [KeySourceAction]
charToKeys c = [KeyDown {value = T.singleton c}, KeyUp {value = T.singleton c}]
