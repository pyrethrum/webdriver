module HTTP.Runner where

import Common.Runner (mkInteractBehaviour, runSetup)
import Effectful (Eff, IOE, (:>))
import WebDriver.Effectful
  ( HttpCapabilities,
    InteractBehaviour (..),
    Logger,
    Pause,
    WebDriverHttp,
    fromHttpCapability,
    FullCapabilities (..),
    withHttpSession,
    withLogger,
    withPause,
  )
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..))

mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch  = []
    }

runHttpTest
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , Pause :> es
        , WebDriverHttp :> es
        )
     => Eff es ()
     )
  -> IO ()
runHttpTest action =
  runSetup $ \driverInfo behaviour config ->
    withLogger "eval.log" $
      withHttpSession driverInfo behaviour (mkHttpCaps config) $
        withPause behaviour.pauseDuration action
