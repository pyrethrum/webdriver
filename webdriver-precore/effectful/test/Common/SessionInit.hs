module Common.SessionInit (

) where

import Data.Text (Text)
import Effectful (liftIO, MonadIO)
import UnliftIO (finally)
import WebDriver.Effectful.Logger ( acquireLogger, releaseLogger)

import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Utils.Timeout as T (Timeout(..)) 
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriver.Effectful (HttpEndpoint(..), HttpCapabilities, FullCapabilities (..))
import WebDriver.Effectful.App
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Extended.Capabilities (fromHttpCapability, HttpSessionResponse)
import WebDriverPreCore.HTTP.Protocol (Capabilities(..))


mkHttpCaps :: Bool -> Config -> HttpCapabilities
mkHttpCaps bidiSocket config =
  let baseCaps = httpCapabilities config
      updatedCaps = baseCaps {webSocketUrl = if bidiSocket then Just True else Nothing}
  in MkFullCapabilities
       { alwaysMatch = Just (fromHttpCapability updatedCaps),
         firstMatch  = []
       }

data CfgLoaded = MkCfgLoaded
  {
    httpEndpoint :: HttpEndpoint,
    httpCapabilities :: HttpCapabilities,
    wantLogging :: Bool
  }

getConfigData :: Bool -> IO CfgLoaded
getConfigData wantBiDiSocket = do
  cfg@MkConfig{httpUrl = host, httpPort = port, logging} <- loadConfig
  loggerHandle <- if logging
                    then Just <$> acquireLogger "eval.log"
                    else pure Nothing
  let 
    endpoint = MkHttpEndpoint {host, port}
    -- TODO: Need to add a function to Logger module to convert LoggerHandle to (Text -> IO ())

  pure MkCfgLoaded
    {
      httpEndpoint = endpoint,
      httpCapabilities = mkHttpCaps wantBiDiSocket cfg,
      wantLogging = logging
    }

-- | Create a new WebDriver session based on config
getWDSession :: Bool -> IO WDSession
getWDSession wantBiDiSocket = do
  MkCfgLoaded{logger, loggerHandle, httpEndpoint = endpoint, httpCapabilities = caps} <- getConfigData wantBiDiSocket
  sessionInfo <- acquireHttpSession endpoint logger caps
  pure MkWDSession {loggerHandle, sessionInfo}

closeWDSession :: WDSession -> IO ()
closeWDSession MkWDSession {loggerHandle, sessionInfo} =
  releaseHttpSession sessionInfo
    `finally` maybe (pure ()) releaseLogger loggerHandle
  

testUrl :: MonadIO m => IO URL -> m URL 
testUrl = liftIO

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9