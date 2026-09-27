module Common.SessionInit
  ( WDSession (..),
    mkHttpCaps,
    getWDSession,
    closeWDSession,
    testUrl,
  )
where

import Data.Text (Text)
import Effectful (MonadIO, liftIO, runEff)
import Effectful.Error.Static (HasCallStack, runErrorNoCallStack)
import System.IO (Handle)
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
import WebDriverPreCore.HTTP.Protocol (Capabilities (..), ParseFailure, Session)
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
  pure
    MkCfgLoaded
      { httpEndpoint = MkHttpEndpoint {host, port},
        httpCapabilities = mkHttpCaps wantBiDiSocket cfg,
        loggerData
      }

-- | A WebDriver session together with the resources needed to run and
-- release it: a logger environment, the underlying logger data (for
-- releasing scribes/file handles), and the HTTP session parameters.
data WDSession = MkWDSession
  { session :: Session,
    endpoint :: HttpEndpoint,
    websocketUrl :: Maybe Text,
    loggerEnv :: LogEnv,
    loggerFileHandle :: Maybe Handle
  }

-- | Create a new WebDriver session based on config
getWDSession :: (HasCallStack) => Bool -> IO WDSession
getWDSession wantBiDiSocket = do
  MkCfgLoaded
    { httpEndpoint = endpoint,
      httpCapabilities = caps,
      loggerData = MkLoggerData {fileHandle = loggerFileHandle, loggerEnv}
    } <-
    getConfigData wantBiDiSocket
  MkHttpSessionResponse {session, websocketUrl} <-
    ( runEff
        $ runErrorNoCallStack @ParseFailure
        $ runLogger loggerEnv
        $ acquireHttpSession endpoint caps
    )
      >>= ioThrow
  pure
    MkWDSession
      { session,
        endpoint,
        loggerEnv,
        websocketUrl,
        loggerFileHandle
      }

closeWDSession :: (HasCallStack) => WDSession -> IO ()
closeWDSession MkWDSession {loggerEnv, loggerFileHandle = fileHandle, session, endpoint} = do
  result <-
    runEff
      $ runErrorNoCallStack @ParseFailure
      $ runLogger loggerEnv
      $ releaseHttpSession (MkHttpParams {session, endpoint})
  ioThrow result `finally` releaseLogger (MkLoggerData {fileHandle, loggerEnv})

testUrl :: (MonadIO m) => IO URL -> m URL
testUrl = liftIO

-- ghc/ghc#27214
-- https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=created_date&state=opened&search=expectJust&first_page_size=20&show=eyJpaWQiOiIyNzIxNCIsImZ1bGxfcGF0aCI6ImdoYy9naGMiLCJpZCI6MjgzMzJ9