module BiDi.Runner where

import Common.SessionInit (WDSession (..), closeWDSession, getWDSession)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff, (:>))
import UnliftIO (finally)
import WebDriver.Effectful
import WebDriverPreCore.BiDi.Protocol
  ( KeySourceAction (..),
    PointerCommonProperties (..),
  )
import WebDriver.Effectful.Logger (Logger, runLogger)
import WebDriverPreCore.Utils.Utils (ioThrow)

withBidi
  ::  ( forall es
      . ( IOE :> es
        , Logger :> es
        , WaitPrimative :> es
        , WebDriverBiDi :> es
        )
     => Eff es ()
     )
  -> IO ()
withBidi action = do
  session@MkWDSession {loggerEnv, websocketUrl} <- getWDSession True
  bidiUrl <- ioThrow (parseBiDiUrlProperty websocketUrl)
  ( runEff
      $ runWaitPrimative
      $ runLogger loggerEnv
      $ withBiDiSession bidiUrl
      $ action
    )
    `finally` closeWDSession session

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
