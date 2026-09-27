module Common.SessionInit
  ( WDSession (..),
    mkHttpCaps,
    getWDSession,
    closeWDSession,
    testUrl,
  )
where

import Effectful (MonadIO, liftIO, runEff)
import Effectful.Error.Static (runErrorNoCallStack, HasCallStack)
import UnliftIO (finally)
import WebDriver.Effectful (FullCapabilities (..), HttpCapabilities, HttpEndpoint (..))
import WebDriver.Effectful.App (acquireHttpSession, releaseHttpSession)
import WebDriver.Effectful.HTTP.Base.Interpreter (HttpParams (..))
import WebDriver.Effectful.Logger
  ( LogEnv,
    LoggerData (..),
    acquireLogger,
    acquireNoOpLogger,
    releaseLogger,
    runLogger,
  )
import WebDriverPreCore.Extended.Capabilities (HttpSessionResponse (..), fromHttpCapability)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (URL)
import WebDriverPreCore.HTTP.Protocol (Capabilities (..), ParseFailure)
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Utils.Utils (ioThrow)

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
    loggerData :: LoggerData
  }

getConfigData :: Bool -> IO CfgLoaded
getConfigData wantBiDiSocket = do
  cfg@MkConfig {httpUrl = host, httpPort = port, logging} <- loadConfig
  loggerData <-
    if logging
      then acquireLogger "eval.log"
      else acquireNoOpLogger
  let endpoint = MkHttpEndpoint {host, port}

  pure
    MkCfgLoaded
      { httpEndpoint = endpoint,
        httpCapabilities = mkHttpCaps wantBiDiSocket cfg,
        loggerData = loggerData
      }

-- | A WebDriver session together with the resources needed to run and
-- release it: a logger environment, the underlying logger data (for
-- releasing scribes/file handles), and the HTTP session parameters.
data WDSession = MkWDSession
  { loggerHandle :: LogEnv,
    loggerData :: LoggerData,
    sessionInfo :: HttpParams
  }

-- | Create a new WebDriver session based on config
getWDSession :: (HasCallStack) => Bool -> IO WDSession
getWDSession wantBiDiSocket = do
  MkCfgLoaded {httpEndpoint = endpoint, httpCapabilities = caps, loggerData = loggerData} <-
    getConfigData wantBiDiSocket
  MkHttpSessionResponse {session = sessionId} <-
    ( runEff
        $ runErrorNoCallStack @ParseFailure
        $ runLogger loggerData.loggerEnv
        $ acquireHttpSession endpoint caps
      )
      >>= ioThrow
  pure
    MkWDSession
      { loggerHandle = loggerData.loggerEnv,
        loggerData = loggerData,
        sessionInfo = MkHttpParams {session = sessionId, endpoint}
      }

closeWDSession :: (HasCallStack) => WDSession -> IO ()
closeWDSession MkWDSession {loggerHandle, loggerData, sessionInfo} = do
  result <-
    runEff
      $ runErrorNoCallStack @ParseFailure
      $ runLogger loggerHandle
      $ releaseHttpSession sessionInfo
  ioThrow result `finally` releaseLogger loggerData

testUrl :: (MonadIO m) => IO URL -> m URL
testUrl = liftIO

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9