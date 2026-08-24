module Common.Runner where

import Effectful (Eff, IOE, liftIO, (:>), MonadIO, runEff)
import UnliftIO (finally)
import WebDriver.Effectful.Logger (LoggerHandle, acquireLogger, releaseLogger)

import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Utils.Timeout as T (Timeout(..)) 
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import Effectful.Timeout
import WebDriver.Effectful.HTTP.Base.Effect
import qualified WebDriverPreCore.Extended.Capabilities as EC
import WebDriver.Effectful (HttpEndpoint(..), HttpCapabilities, FullCapabilities (..))
import WebDriver.Effectful.App
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Extended.Capabilities (fromHttpCapability)
-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------


mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch  = []
    }

data WDSession = MkWDSession
  { loggerHandle :: Maybe LoggerHandle,
    sessionInfo :: HttpSessionInfo
  }

getWDSession :: T.Timeout -> Bool -> IO WDSession
getWDSession pauseDuration wantLogging = 
  runSetup $ 
   \driverInfo config -> 
      liftIO $ do
        loggerHandle <- if wantLogging
                          then Just <$> acquireLogger "eval.log"
                          else pure Nothing
        sessionInfo <- acquireHttpSession driverInfo (mkHttpCaps config) pauseDuration
        pure MkWDSession {loggerHandle, sessionInfo}

closeWDSession :: WDSession -> IO ()
closeWDSession MkWDSession {loggerHandle, sessionInfo} =
  releaseHttpSession sessionInfo
    `finally` maybe (pure ()) releaseLogger loggerHandle


runSetup :: forall a. (forall es. (IOE :> es) => HttpDriverInfo -> Config -> Eff es a) -> IO a
runSetup action = runEff runAction
  where
    runAction :: Eff '[IOE] a
    runAction = do
      (config :: Config) <- liftIO loadConfig
      let
          driverInfo :: HttpDriverInfo
          driverInfo =
            MkHttpDriverInfo
              { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
                driverLogFn  = Nothing
              }
      action driverInfo config
  

testUrl :: MonadIO m => IO URL -> m URL 
testUrl = liftIO

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9