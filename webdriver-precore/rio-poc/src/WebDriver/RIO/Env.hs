-- |
-- Module: WebDriver.RIO.Env
-- Description: RIO environment types for outer and inner WebDriver layers
--
-- Defines environment data types:
--
-- * 'HttpEnv'        — logging + IO HTTP runner
-- * 'HttpSessionEnv' — 'HttpEnv' + session id
--
-- All implement 'HasLogFunc'; runner envs implement 'HasHttpRunner';
-- session envs add 'HasHttpSession'.
module WebDriver.RIO.Env
  ( BaseEnv (..),

    -- * Runner Envs
    HttpEnv (..),
    mkHttpEnv,

    -- * Session Envs
    HttpSessionEnv (..),

    -- * Runner Typeclasses
    HasHttpRunner (..),
    HasBiDiRunner (..),

    -- * Session Typeclasses
    HasHttpSession (..),
    HasBiDiSession (..),
    HasPauseDuration (..),
    getLogger,
    getHttpRunner,
    getHttpCommandRunner,
    getBiDiRunner,
  )
where

import Data.Aeson (FromJSON)
import RIO (HasLogFunc (..), Lens', LogFunc, RIO, lens, view)
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (Command (..), Session (..))
import WebDriverPreCore.HttpRunner (HttpRunner (..))
import WebDriverPreCore.Utils.Timeout (Timeout)


-- | Env has an IO 'HttpRunner' available.
class HasHttpRunner env where
  httpRunnerL :: Lens' env (HttpRunner IO)

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
    httpRunner :: HttpRunner IO
  }

-- | Construct an 'HttpEnv' from a 'LogFunc' and an IO 'HttpRunner'.
mkHttpEnv :: LogFunc -> HttpRunner IO -> HttpEnv
mkHttpEnv = MkHttpEnv

instance HasLogFunc HttpEnv where
  logFuncL :: Lens' HttpEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpEnv {..} l -> MkHttpEnv {logFunc = l, ..}

instance HasHttpRunner HttpEnv where
  httpRunnerL :: Lens' HttpEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkHttpEnv {..} r -> MkHttpEnv {httpRunner = r, ..}

-- ---------------------------------------------------------------------------
-- Shared helpers
-- ---------------------------------------------------------------------------

getLogger :: (HasLogFunc env) => RIO env LogFunc
getLogger = view logFuncL

getHttpRunner :: (HasHttpRunner env) => RIO env (HttpRunner IO)
getHttpRunner = view httpRunnerL

getHttpCommandRunner :: forall env a. (HasHttpRunner env, FromJSON a) => RIO env (Command a -> IO a)
getHttpCommandRunner = do
  MkHttpRunner {run} <- getHttpRunner
  pure run

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
    httpRunner :: HttpRunner IO,
    httpSession :: Session,
    pauseDuration :: Timeout
  }

instance HasLogFunc HttpSessionEnv where
  logFuncL :: Lens' HttpSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpSessionEnv {..} l -> MkHttpSessionEnv {logFunc = l, ..}

instance HasHttpRunner HttpSessionEnv where
  httpRunnerL :: Lens' HttpSessionEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkHttpSessionEnv {..} r -> MkHttpSessionEnv {httpRunner = r, ..}

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