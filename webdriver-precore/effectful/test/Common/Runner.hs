module Common.Runner where

import Effectful (Eff, IOE, liftIO, (:>), MonadIO)
import WebDriver.Effectful
  ( HttpDriverInfo (..),
    HttpEndpoint (..),
    InteractOpts (..),
    runHttp,
  )
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Utils.Timeout (milliseconds)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)

runSetup :: (forall es. (IOE :> es) => HttpDriverInfo -> InteractOpts -> Config -> Eff es a) -> IO a
runSetup action = runHttp $ do
  config <- liftIO loadConfig
  let opts = mkInteractOpts config
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
            driverLogFn  = Nothing
          }
  action driverInfo opts config

mkInteractOpts :: Config -> InteractOpts
mkInteractOpts config =
  MkInteractOpts
    { pauseDuration = fromIntegral config.pauseMS * milliseconds,
      driverLogging = config.logging
    }

testUrl :: MonadIO m => IO URL -> m URL 
testUrl = liftIO
