module Common.Runner where

import Effectful (Eff, IOE, liftIO, (:>), MonadIO, runEff)
import WebDriver.Effectful
  ( HttpDriverInfo (..),
    HttpEndpoint (..),
    InteractOpts (..)
  )
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Utils.Timeout (milliseconds)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)

runSetup :: forall a. (forall es. (IOE :> es) => HttpDriverInfo -> InteractOpts -> Config -> Eff es a) -> IO a
runSetup action = runEff $ do
  (config :: Config) <- liftIO loadConfig
  let 
      opts :: InteractOpts
      opts = mkInteractOpts config
      
      driverInfo :: HttpDriverInfo
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

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9