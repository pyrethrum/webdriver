module BiDi.Runner where

-- TODO: Update to use new acquire/release pattern
import Common.SessionInit (runSetup)
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import WebDriver.Effectful
import WebDriverPreCore.BiDi.Protocol
  ( KeySourceAction (..),
    PointerCommonProperties (..),
  )
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..))
import WebDriver.Effectful.Logger (withLogger, Logger)
import WebDriverPreCore.Utils.Timeout as T (Timeout(..)) 
import WebDriver.Effectful.WaitPrimative (runWaitPrimative)

mkBiDiCaps :: Config -> HttpCapabilities
mkBiDiCaps config =
  MkFullCapabilities
    { alwaysMatch = Just cap {httpWebSocketUrl = True},
      firstMatch  = []
    }
  where
    cap = fromHttpCapability $ httpCapabilities config

runBiDiTest
  ::  ( forall es
      . ( IOE :> es
        , Logger :> es
        , WaitPrimative :> es
        , WebDriverBiDi :> es
        )
     => Eff es ()
     )
  -> IO ()
runBiDiTest action =
  runSetup $ \driverInfo config ->
    withLogger "eval.log" $
      withBiDiSession driverInfo (mkBiDiCaps config) $
        runWaitPrimative action

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
