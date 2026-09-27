module Common.SessionInit
  ( testUrl,
  )
where

import Data.Text (Text)
import Effectful (MonadIO, liftIO, runEff)
import Effectful.Error.Static (runError)
import UnliftIO (finally)
import WebDriver.Effectful (FullCapabilities (..), HttpCapabilities, HttpEndpoint (..), runWebDriverHttp)
import WebDriver.Effectful.App
import WebDriver.Effectful.Logger (acquireLogger, releaseLogger, LogEnv)
import WebDriver.Effectful.Logger.KatipInterpreter (runLogger)
import WebDriverPreCore.Extended.Capabilities (HttpSessionResponse, fromHttpCapability)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriverPreCore.HTTP.Protocol (Capabilities (..), ParseFailure)
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Utils.Timeout as T (Timeout (..))
import WebDriverPreCore.Utils.Utils (ioThrow, throwLeft)

mkHttpCaps :: Bool -> Config -> HttpCapabilities
mkHttpCaps bidiSocket config =
  let baseCaps = httpCapabilities config
      updatedCaps = baseCaps {webSocketUrl = if bidiSocket then Just True else Nothing}
   in MkFullCapabilities
        { alwaysMatch = Just (fromHttpCapability updatedCaps),
          firstMatch = []
        }

data CfgLoaded = MkCfgLoaded
  { httpEndpoint :: HttpEndpoint,
    httpCapabilities :: HttpCapabilities,
    wantLogging :: Bool
  }

getConfigData :: Bool -> IO CfgLoaded
getConfigData wantBiDiSocket = do
  cfg@MkConfig {httpUrl = host, httpPort = port, logging} <- loadConfig
  loggerHandle <-
    if logging
      then Just <$> acquireLogger "eval.log"
      else pure Nothing
  let endpoint = MkHttpEndpoint {host, port}
  -- TODO: Need to add a function to Logger module to convert LoggerHandle to (Text -> IO ())

  pure
    MkCfgLoaded
      { httpEndpoint = endpoint,
        httpCapabilities = mkHttpCaps wantBiDiSocket cfg,
        wantLogging = logging
      }

-- | Create a new WebDriver session based on config
-- getWDSession :: Bool -> IO HttpSessionResponse
getWDSession :: LogEnv -> Bool -> IO HttpSessionResponse
getWDSession logEnv wantBiDiSocket = do
  MkCfgLoaded {httpEndpoint = endpoint, httpCapabilities = caps} <- getConfigData wantBiDiSocket
  ( runEff
      . runError @ParseFailure
      . runLogger logEnv
      $ acquireHttpSession endpoint caps
    )
    >>= ioThrow

-- closeWDSession :: WDSession -> IO ()
-- closeWDSession MkWDSession {loggerHandle, sessionInfo} =
--   releaseHttpSession sessionInfo
--     `finally` maybe (pure ()) releaseLogger loggerHandle

testUrl :: (MonadIO m) => IO URL -> m URL
testUrl = liftIO

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9