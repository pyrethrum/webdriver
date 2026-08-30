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


mkHttpCaps :: Boolean -> Config -> HttpCapabilities
mkHttpCaps bidiSocket config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config {httpWebSocketUrl = bidiSocket},
      firstMatch  = []
    }

data WDSession = MkWDSession
  { loggerHandle :: Maybe LoggerHandle,
    sessionInfo :: HttpSessionInfo
  }
data CfgLoaded = MkCfgLoaded
  { logger :: Text -> IO (),
    loggerHandle :: Maybe LoggerHandle,
    httpEndpoint :: HttpEndpoint,
    httpCapabilities :: HttpCapabilities,
    pauseDuration :: Timeout
  }

getConfigData :: Bool -> IO CfgLoaded
getConfigData bidiSocket = do
  cfg@MkConfig{httpUrl = host, httpPort = port, wantLogging} <- loadConfig
  loggerHandle <- if wantLogging
                    then Just <$> acquireLogger "eval.log"
                    else pure Nothing
  let 
    endpoint = MkHttpEndpoint {host, port}
    logger = loggerHandle & 
                maybe 
                  (const $ pure ()) 
                  (\lh -> \t -> liftIO $ logToHandle lh t) 
  pure MkCfgLoaded
    { logger,
      loggerHandle,
      httpEndpoint = endpoint,
      httpCapabilities = mkHttpCaps bidiSocket cfg,
      pauseDuration = cfg.pauseDuration
    }

-- | Create a new WebDriver session based on config
getWDSession :: Bool -> IO WDSession
getWDSession bidiSocket = do
  MkCfgLoaded{logger, loggerHandle, httpEndpoint = endpoint, httpCapabilities, pauseDuration} <- getConfigData bidiSocket
  let caps = if bidiSocket then EC.addBidiFlag httpCapabilities else httpCapabilities
  sessionResponse@MkHttpSessionResponse{session} <- EC.newHttpSession caps endpoint  
  let sessionInfo = MkHttpSessionInfo
        { endpoint,
          logger,
          session,
          pauseDuration,
          sessionResponse
        }
  pure MkWDSession {loggerHandle, sessionInfo}

closeWDSession :: WDSession -> IO ()
closeWDSession MkWDSession {loggerHandle, sessionInfo} =
  releaseHttpSession sessionInfo
    `finally` maybe (pure ()) releaseLogger loggerHandle
  

testUrl :: MonadIO m => IO URL -> m URL 
testUrl = liftIO

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9