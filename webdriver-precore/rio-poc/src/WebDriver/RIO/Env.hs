-- |
-- Module: WebDriver.RIO.Env
-- Description: RIO environment types for outer and inner WebDriver layers
--
-- Defines environment data types:
--
-- * 'HttpEnv'        — logging + HTTP driver info
-- * 'HttpSessionEnv' — 'HttpEnv' + session id
--
-- All implement 'HasLogFunc'; runner envs implement 'HasHttpDriverInfo';
-- session envs add 'HasHttpSession'.
module WebDriver.RIO.Env
  ( BaseEnv (..),

    -- * Driver Info
    HttpDriverInfo (..),

    -- * Runner Envs
    HttpEnv (..),
    mkHttpEnv,

    -- * Session Envs
    HttpSessionEnv (..),

    -- * Driver Info Typeclass
    HasHttpDriverInfo (..),

    -- * Other Runner Typeclasses
    HasBiDiRunner (..),

    -- * Session Typeclasses
    HasHttpSession (..),
    HasBiDiSession (..),
    HasPauseDuration (..),
    getLogger,
    getHttpDriverInfo,
    runCommand,
    getBiDiRunner,
  )
where

import Data.Aeson (FromJSON)
import RIO (HasLogFunc (..), Lens', LogFunc, RIO, Text, display, lens, liftIO, logInfo, runRIO, throwIO, view)
import WebDriverPreCore.BiDiRunner (BiDiRunner)
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
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env BiDiRunner

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
-- HTTP runner env
-- ---------------------------------------------------------------------------

-- | HTTP-runner environment.
data HttpEnv = MkHttpEnv
  { logFunc :: LogFunc,
    httpDriverInfo :: HttpDriverInfo
  }

-- | Construct an 'HttpEnv' from a 'LogFunc' and 'HttpDriverInfo'.
mkHttpEnv :: LogFunc -> HttpDriverInfo -> HttpEnv
mkHttpEnv = MkHttpEnv

instance HasLogFunc HttpEnv where
  logFuncL :: Lens' HttpEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpEnv {..} l -> MkHttpEnv {logFunc = l, ..}

instance HasHttpDriverInfo HttpEnv where
  httpDriverInfoL :: Lens' HttpEnv HttpDriverInfo
  httpDriverInfoL = lens (.httpDriverInfo) \MkHttpEnv {..} i -> MkHttpEnv {httpDriverInfo = i, ..}

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

getLogger :: (HasLogFunc env) => RIO env LogFunc
getLogger = view logFuncL

getHttpDriverInfo :: (HasHttpDriverInfo env) => RIO env HttpDriverInfo
getHttpDriverInfo = view httpDriverInfoL

-- | Run a WebDriver 'Command' in the RIO environment, using the stored
-- driver info to build an HTTP runner on each call.
-- Logging is enabled only when 'driverLogging' is 'True'.
runCommand :: forall env r. (HasHttpDriverInfo env, HasLogFunc env, FromJSON r) => Command r -> RIO env r
runCommand cmd = do
  MkHttpDriverInfo {httpEndpoint, driverLogging} <- getHttpDriverInfo
  lf <- view logFuncL
  let mLogger :: Maybe (Text -> IO ())
      mLogger
        | driverLogging = Just $ \t -> runRIO lf (logInfo (display t))
        | otherwise     = Nothing
  liftIO $ callWebDriver httpEndpoint mLogger cmd >>= either (throwIO . parseFailToWDException) pure

getBiDiRunner :: (HasBiDiRunner env) => RIO env BiDiRunner
getBiDiRunner = view biDiRunnerL

data BaseEnv = MkBaseEnv
  { logFunc :: LogFunc
  }

instance HasLogFunc BaseEnv where
  logFuncL :: Lens' BaseEnv LogFunc
  logFuncL = lens (.logFunc) \MkBaseEnv {} l -> MkBaseEnv {logFunc = l}

-- ---------------------------------------------------------------------------
-- HTTP session env
-- ---------------------------------------------------------------------------

-- | HTTP environment extended with a session id.
data HttpSessionEnv = MkHttpSessionEnv
  { logFunc :: LogFunc,
    httpDriverInfo :: HttpDriverInfo,
    httpSession :: Session,
    pauseDuration :: Timeout
  }

instance HasLogFunc HttpSessionEnv where
  logFuncL :: Lens' HttpSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpSessionEnv {..} l -> MkHttpSessionEnv {logFunc = l, ..}

instance HasHttpDriverInfo HttpSessionEnv where
  httpDriverInfoL :: Lens' HttpSessionEnv HttpDriverInfo
  httpDriverInfoL = lens (.httpDriverInfo) \MkHttpSessionEnv {..} i -> MkHttpSessionEnv {httpDriverInfo = i, ..}

instance HasHttpSession HttpSessionEnv where
  getHttpSession :: HttpSessionEnv -> Session
  getHttpSession = (.httpSession)

instance HasPauseDuration HttpSessionEnv where
  getPauseDuration :: HttpSessionEnv -> Timeout
  getPauseDuration = (.pauseDuration)

{- DO NOT DELETE
-- ---------------------------------------------------------------------------
-- BiDi only
-- ---------------------------------------------------------------------------

data BiDiEnv = MkBiDiEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc BiDiEnv where
  logFuncL :: Lens' BiDiEnv LogFunc
  logFuncL = lens (.logFunc) \MkBiDiEnv {..} l -> MkBiDiEnv {logFunc = l, ..}

instance HasBiDiRunner BiDiEnv where
  biDiRunnerL :: Lens' BiDiEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkBiDiEnv {..} r -> MkBiDiEnv {biDiRunner = r, ..}

-- ---------------------------------------------------------------------------
-- Dual (HTTP + BiDi)
-- ---------------------------------------------------------------------------

-- | Inner environment carrying both HTTP and BiDi runners.
data DualEnv = MkDualEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner,
    biDiRunner :: BiDiRunner
  }

httpRunnerGetter :: DualEnv -> HttpRunner
httpRunnerGetter MkDualEnv {httpRunner = r} = r

instance HasLogFunc DualEnv where
  logFuncL :: Lens' DualEnv LogFunc
  logFuncL = lens (.logFunc) \MkDualEnv {..} l -> MkDualEnv {logFunc = l, ..}

instance HasHttpRunner DualEnv where
  httpRunnerL :: Lens' DualEnv HttpRunner
  httpRunnerL = lens httpRunnerGetter \MkDualEnv {..} h -> MkDualEnv {httpRunner = h, ..}

instance HasBiDiRunner DualEnv where
  biDiRunnerL :: Lens' DualEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkDualEnv {..} b -> MkDualEnv {biDiRunner = b, ..}

-- ---------------------------------------------------------------------------
-- BiDi Session (BiDi runner + session id)
-- ---------------------------------------------------------------------------

-- | BiDi environment extended with a session id.
data BiDiSessionEnv = MkBiDiSessionEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner,
    biDiSession :: Session,
    pauseDuration :: Timeout
  }

instance HasLogFunc BiDiSessionEnv where
  logFuncL :: Lens' BiDiSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkBiDiSessionEnv {..} l -> MkBiDiSessionEnv {logFunc = l, ..}

instance HasBiDiRunner BiDiSessionEnv where
  biDiRunnerL :: Lens' BiDiSessionEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkBiDiSessionEnv {..} r -> MkBiDiSessionEnv {biDiRunner = r, ..}

instance HasBiDiSession BiDiSessionEnv where
  getBiDiSession :: BiDiSessionEnv -> Session
  getBiDiSession = (.biDiSession)

instance HasPauseDuration BiDiSessionEnv where
  getPauseDuration :: BiDiSessionEnv -> Timeout
  getPauseDuration = (.pauseDuration)

-- ---------------------------------------------------------------------------
-- Dual Session (HTTP + BiDi runners + session id)
-- ---------------------------------------------------------------------------

-- | Dual environment extended with a session id.
data DualSessionEnv = MkDualSessionEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner,
    biDiRunner :: BiDiRunner,
    httpSession :: Session,
    biDiSession :: Session,
    pauseDuration :: Timeout
  }

dualSessionEnvHttpRunnerExtract :: DualSessionEnv -> HttpRunner
dualSessionEnvHttpRunnerExtract MkDualSessionEnv {httpRunner = r} = r

instance HasLogFunc DualSessionEnv where
  logFuncL :: Lens' DualSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkDualSessionEnv {..} l -> MkDualSessionEnv {logFunc = l, ..}

instance HasHttpRunner DualSessionEnv where
  httpRunnerL :: Lens' DualSessionEnv HttpRunner
  httpRunnerL = lens dualSessionEnvHttpRunnerExtract \MkDualSessionEnv {..} h -> MkDualSessionEnv {httpRunner = h, ..}

instance HasBiDiRunner DualSessionEnv where
  biDiRunnerL :: Lens' DualSessionEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkDualSessionEnv {..} b -> MkDualSessionEnv {biDiRunner = b, ..}

instance HasHttpSession DualSessionEnv where
  getHttpSession :: DualSessionEnv -> Session
  getHttpSession = (.httpSession)

instance HasBiDiSession DualSessionEnv where
  getBiDiSession :: DualSessionEnv -> Session
  getBiDiSession = (.biDiSession)

instance HasPauseDuration DualSessionEnv where
  getPauseDuration :: DualSessionEnv -> Timeout
  getPauseDuration = (.pauseDuration)

-}