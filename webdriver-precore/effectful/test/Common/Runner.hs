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

-- | Create a new WebDriver session based on config
-- TODO:: ADD BIDI Flag
getWDSession :: IO WDSession
getWDSession = do
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
  sessionResponse@MkHttpSessionResponse{session} <- EC.newHttpSession (mkHttpCaps cfg) endpoint  
  let sessionInfo = MkHttpSessionInfo
        { endpoint = endpoint,
          logger = logger,
          session = session,
          pauseDuration = cfg.pauseDuration,
          sessionResponse = sessionResponse
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