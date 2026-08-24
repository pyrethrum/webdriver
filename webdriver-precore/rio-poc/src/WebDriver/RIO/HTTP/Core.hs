-- |
-- Module: WebDriver.RIO.HTTP.Core
-- Description: Core typeclasses and abstract functions for RIO WebDriver
--
-- Provides:
--
-- * 'HttpDriverInfo' — HTTP connection configuration
-- * Typeclasses: 'HasHttpDriverInfo', 'HasBiDiRunner', 'HasHttpSession',
--   'HasBiDiSession', 'HasPauseDuration'
-- * Abstract helpers: 'getLogger', 'getHttpEndpoint', 'runHttpCommand',
--   'getBiDiRunner'
module WebDriver.RIO.HTTP.Core
  ( 
    -- * Driver Info Typeclass
    HasHttpEndpoint (..),
    HasDriverLogging (..),

    -- * Other Runner Typeclasses
    HasBiDiRunner (..),

    -- * Session Typeclasses
    HasHttpSession (..),
    HasBiDiSession (..),
    HasPauseDuration (..),

    -- * Abstract helpers
    getLogger,
    getHttpEndpoint,
    runHttpCommand,
    getBiDiRunner,
    log
  )
where

import Prelude hiding (log)
import Data.Aeson (FromJSON)
import RIO
  ( HasLogFunc (..),
    Lens',
    LogFunc,
    RIO,
    Text,
    display,
    liftIO,
    logInfo,
    runRIO,
    throwIO,
    view,
  )
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.Error (parseFailToWDException)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (Session (..))
import WebDriverPreCore.Extended.HTTP.Protocol (Command (..))
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), callWebDriver)
import WebDriverPreCore.Utils.Timeout (Timeout)

-- | Configuration for an HTTP WebDriver connection.

-- | Env has a 'BiDiRunner' available.
-- | Env has an 'HttpDriverInfo' available.
class HasHttpEndpoint env where
  httpDriverInfoL :: Lens' env HttpEndpoint

-- | Env has driver logging configuration available.
class HasDriverLogging env where
  driverLoggingL :: Lens' env Bool

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env (BiDiRunner (RIO env))

-- | Env has an HTTP session id available.
class HasHttpSession env where
  getHttpSession :: env -> Session

-- | Env has a BiDi session id available.
class HasBiDiSession env where
  getBiDiSession :: env -> Session

-- | Env has a pause duration available.
class HasPauseDuration env where
  getPauseDuration :: env -> Timeout

-- ---------------------------------------------------------------------------
-- Abstract helpers
-- ---------------------------------------------------------------------------

getLogger :: (HasLogFunc env) => RIO env LogFunc
getLogger = view logFuncL

log :: (HasLogFunc env) => Text -> RIO env ()
log = logInfo . display

getHttpEndpoint :: (HasHttpEndpoint env) => RIO env HttpEndpoint
getHttpEndpoint = view httpDriverInfoL

getDriverLogging :: (HasDriverLogging env) => RIO env Bool
getDriverLogging = view driverLoggingL

-- | Run a WebDriver 'Command' in the RIO environment, using the stored
-- driver info to build an HTTP runner on each call.
-- Logging is enabled only when 'driverLogging' is 'True'.
runHttpCommand :: forall env r. (HasHttpEndpoint env, HasLogFunc env, FromJSON r, HasDriverLogging env) => Command r -> RIO env r
runHttpCommand cmd = do
  httpEndpoint <- getHttpEndpoint
  driverLogging <- getDriverLogging
  lf <- view logFuncL
  let logger :: Text -> IO ()
      logger
        | driverLogging = \t -> runRIO lf (logInfo (display t))
        | otherwise = const $ pure ()
  liftIO $
    callWebDriver httpEndpoint logger cmd
      >>= either (throwIO . parseFailToWDException) pure

getBiDiRunner :: (HasBiDiRunner env) => RIO env (BiDiRunner (RIO env))
getBiDiRunner = view biDiRunnerL
