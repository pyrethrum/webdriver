-- |
-- Module: WebDriver.RIO.Env
-- Description: RIO environment types for outer and inner WebDriver layers
--
-- Defines environment data types:
--
-- * 'HttpEnv'        — logging + HTTP driver info
-- * 'HttpSessionEnv' — 'HttpEnv' + session id
-- * 'BiDiEnv'        — logging + BiDi runner
-- * 'BiDiSessionEnv' — 'BiDiEnv' + session id + pause duration
--
-- All implement 'HasLogFunc'; runner envs implement the appropriate runner
-- typeclass; session envs add the matching session typeclass.
--
-- Typeclasses and abstract helpers live in "WebDriver.RIO.HTTP.Core".
module WebDriver.RIO.Env
  ( BaseEnv (..),

    -- * Runner Envs
    HttpEnv (..),
    mkHttpEnv,

    -- * Session Envs
    HttpSessionEnv (..),

    -- * BiDi Runner Env
    BiDiEnv (..),

    -- * BiDi Session Env
    BiDiSessionEnv (..),
  )
where

import RIO
  ( HasLogFunc (..),
    Lens',
    LogFunc,
    lens,
  )
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriver.RIO.HTTP.Core
  ( HasBiDiRunner (..),
    HasBiDiSession (..),
    HasHttpDriverInfo (..),
    HasHttpSession (..),
    HasPauseDuration (..),
    HttpDriverInfo (..),
  )

import WebDriverPreCore.Extended.HTTP.Base.Protocol (Session (..))
import WebDriverPreCore.Utils.Timeout (Timeout)

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

-- ---------------------------------------------------------------------------
-- BiDi only
-- ---------------------------------------------------------------------------

-- | BiDi-runner environment: logging + 'BiDiRunner'.
data BiDiEnv m = MkBiDiEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner m
  }

instance HasLogFunc (BiDiEnv m) where
  logFuncL :: Lens' (BiDiEnv m) LogFunc
  logFuncL = lens (.logFunc) \MkBiDiEnv {..} l -> MkBiDiEnv {logFunc = l, ..}

instance HasBiDiRunner (BiDiEnv m) where
  biDiRunnerL :: Lens' (BiDiEnv m) (BiDiRunner m)
  biDiRunnerL = lens (.biDiRunner) \MkBiDiEnv {..} r -> MkBiDiEnv {biDiRunner = r, ..}

-- ---------------------------------------------------------------------------
-- BiDi Session (BiDi runner + session id)
-- ---------------------------------------------------------------------------

-- | BiDi environment extended with a session id and pause duration.
data BiDiSessionEnv m = MkBiDiSessionEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner m,
    biDiSession :: Session,
    pauseDuration :: Timeout
  }

instance HasLogFunc (BiDiSessionEnv m) where
  logFuncL :: Lens' (BiDiSessionEnv m) LogFunc
  logFuncL = lens (.logFunc) \MkBiDiSessionEnv {..} l -> MkBiDiSessionEnv {logFunc = l, ..}

instance HasBiDiRunner (BiDiSessionEnv m) where
  biDiRunnerL :: Lens' (BiDiSessionEnv m) (BiDiRunner m)
  biDiRunnerL = lens (.biDiRunner) \MkBiDiSessionEnv {..} r -> MkBiDiSessionEnv {biDiRunner = r, ..}

instance HasBiDiSession (BiDiSessionEnv m) where
  getBiDiSession :: BiDiSessionEnv m -> Session
  getBiDiSession = (.biDiSession)

instance HasPauseDuration (BiDiSessionEnv m) where
  getPauseDuration :: BiDiSessionEnv m -> Timeout
  getPauseDuration = (.pauseDuration)

{- DO NOT DELETE
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