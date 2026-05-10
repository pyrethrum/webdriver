module HTTP.Runner (
  runHttpTest,
  testUrl
) where

import Common.Runner (runSetup, testUrl)
import Effectful (Effect, Eff, IOE, (:>))
import WebDriver.Effectful
  ( HttpCapabilities,
    InteractOpts (..),
    Pause,
    WebDriverHttp,
    fromHttpCapability,
    FullCapabilities (..),
    withHttpSession,
    runPause
  )
import WebDriver.Effectful.Logger (withLogger, Logger)
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
  runSetup $ \driverInfo opts config ->
    runPause opts.pauseDuration $
      withLogger "eval.log" $
        withHttpSession driverInfo opts (mkHttpCaps config) action


type BaseHTTPAction = forall (es :: [Effect]). (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es ()
