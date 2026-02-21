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

{-
-- ---------------------------------------------------------------------------
-- BiDi Session (BiDi runner + session id)
-- ---------------------------------------------------------------------------
-}