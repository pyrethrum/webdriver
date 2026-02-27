-- |
-- Module: WebDriver.RIO.HTTP.Core
-- Description: Core typeclasses and abstract functions for RIO WebDriver
--
-- Provides:
--
-- * 'HttpDriverInfo' — HTTP connection configuration
-- * Typeclasses: 'HasHttpDriverInfo', 'HasBiDiRunner', 'HasHttpSession',
--   'HasBiDiSession', 'HasPauseDuration'
-- * Abstract helpers: 'getLogger', 'getHttpDriverInfo', 'runHttpCommand',
--   'getBiDiRunner'
module WebDriver.RIO.HTTP.Core
  ( -- * Driver Info
    HttpDriverInfo (..),

    -- * Driver Info Typeclass
    HasHttpDriverInfo (..),

    -- * Other Runner Typeclasses
    HasBiDiRunner (..),

    -- * Session Typeclasses
    HasHttpSession (..),
    HasBiDiSession (..),
    HasPauseDuration (..),

    -- * Abstract helpers
    getLogger,
    getHttpDriverInfo,
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
import WebDriverPreCore.Extended.HTTP.Base.Protocol (Command (..), Session (..))
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), callWebDriver)
import WebDriverPreCore.Utils.Timeout (Timeout)

-- | Configuration for an HTTP WebDriver connection.
data HttpDriverInfo = MkHttpDriverInfo
  { httpEndpoint :: HttpEndpoint,
    driverLogging :: Bool
  }

-- | Env has an 'HttpDriverInfo' available.
class HasHttpDriverInfo env where
  httpDriverInfoL :: Lens' env HttpDriverInfo

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner m env where
  biDiRunnerL :: Lens' env (BiDiRunner m)

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

getHttpDriverInfo :: (HasHttpDriverInfo env) => RIO env HttpDriverInfo
getHttpDriverInfo = view httpDriverInfoL

-- | Run a WebDriver 'Command' in the RIO environment, using the stored
-- driver info to build an HTTP runner on each call.
-- Logging is enabled only when 'driverLogging' is 'True'.
runHttpCommand :: forall env r. (HasHttpDriverInfo env, HasLogFunc env, FromJSON r) => Command r -> RIO env r
runHttpCommand cmd = do
  MkHttpDriverInfo {httpEndpoint, driverLogging} <- getHttpDriverInfo
  lf <- view logFuncL
  let mLogger :: Maybe (Text -> IO ())
      mLogger
        | driverLogging = Just $ \t -> runRIO lf (logInfo (display t))
        | otherwise = Nothing
  liftIO $
    callWebDriver httpEndpoint mLogger cmd
      >>= either (throwIO . parseFailToWDException) pure

getBiDiRunner :: (HasBiDiRunner m env) => RIO env (BiDiRunner m)
getBiDiRunner = view biDiRunnerL
