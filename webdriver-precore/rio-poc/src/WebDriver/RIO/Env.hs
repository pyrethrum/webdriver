{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.Env
-- Description: RIO environment types for outer and inner WebDriver layers
--
-- Defines environment data types:
--
-- * 'LoggerEnv'         — logging only
-- * 'HttpEnv'           — logging + HTTP runner
-- * 'BiDiEnv'           — logging + BiDi runner
-- * 'DualEnv'           — logging + HTTP + BiDi runners
-- * 'HttpSessionEnv'    — 'HttpEnv' + session id
-- * 'BiDiSessionEnv'    — 'BiDiEnv' + session id
-- * 'DualSessionEnv'    — 'DualEnv' + session id
--
-- All implement 'HasLogFunc'; runner envs additionally implement the
-- appropriate runner typeclasses; session envs add 'HasHttpSession' or
-- 'HasBiDiSession'.
module WebDriver.RIO.Env
  ( 
    LoggerEnv (..),

    -- * Runner Envs
    HttpEnv (..),
    BiDiEnv (..),
    DualEnv (..),

    -- * Session Envs
    HttpSessionEnv (..),
    BiDiSessionEnv (..),
    DualSessionEnv (..),

    -- * Runner Typeclasses
    HasHttpRunner (..),
    HasBiDiRunner (..),

    -- * Session Typeclasses
    HasHttpSession (..),
    HasBiDiSession (..),

    getLogger,
    getHttpRunner,
    getBiDiRunner,
    getHttpSessionId,
    getBiDiSessionId
  )
where

import RIO (HasLogFunc (..), Lens', LogFunc, RIO, Text, lens, view)
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.HttpRunner (HttpRunner)

-- | Env has an 'HttpRunner' available.
class HasHttpRunner env where
  httpRunnerL :: Lens' env (HttpRunner IO)

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env BiDiRunner

-- | Env has an HTTP session id available.
class HasHttpSession env where
  getHttpSessionId :: env -> Text

-- | Env has a BiDi session id available.
class HasBiDiSession env where
  getBiDiSessionId :: env -> Text

-- | HTTP runner.
data HttpEnv = MkHttpEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner IO
  }

instance HasLogFunc HttpEnv where
  logFuncL :: Lens' HttpEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpEnv{..} l -> MkHttpEnv { logFunc = l, .. }

getLogger :: HasLogFunc env => RIO env LogFunc
getLogger = view logFuncL

getHttpRunner :: HasHttpRunner env => RIO env (HttpRunner IO)
getHttpRunner = view httpRunnerL

getBiDiRunner :: HasBiDiRunner env => RIO env BiDiRunner
getBiDiRunner = view biDiRunnerL

instance HasHttpRunner HttpEnv where
  httpRunnerL :: Lens' HttpEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkHttpEnv{..} r -> MkHttpEnv { httpRunner = r, .. }

data LoggerEnv = MkLoggerEnv
  { logFunc :: LogFunc
  }

instance HasLogFunc LoggerEnv where
  logFuncL :: Lens' LoggerEnv LogFunc
  logFuncL = lens (.logFunc) \MkLoggerEnv{} l -> MkLoggerEnv { logFunc = l}


-- ---------------------------------------------------------------------------
-- BiDi only
-- ---------------------------------------------------------------------------

data BiDiEnv = MkBiDiEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc BiDiEnv where
  logFuncL :: Lens' BiDiEnv LogFunc
  logFuncL = lens (.logFunc) \MkBiDiEnv{..} l -> MkBiDiEnv { logFunc = l, .. }

instance HasBiDiRunner BiDiEnv where
  biDiRunnerL :: Lens' BiDiEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkBiDiEnv{..} r -> MkBiDiEnv { biDiRunner = r, .. }

-- ---------------------------------------------------------------------------
-- Dual (HTTP + BiDi)
-- ---------------------------------------------------------------------------

-- | Inner environment carrying both HTTP and BiDi runners.
data DualEnv = MkDualEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner IO,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc DualEnv where
  logFuncL :: Lens' DualEnv LogFunc
  logFuncL = lens (.logFunc) \MkDualEnv{..} l -> MkDualEnv { logFunc = l, .. }

instance HasHttpRunner DualEnv where
  httpRunnerL :: Lens' DualEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkDualEnv{..} h -> MkDualEnv { httpRunner = h, .. }

instance HasBiDiRunner DualEnv where
  biDiRunnerL :: Lens' DualEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkDualEnv{..} b -> MkDualEnv { biDiRunner = b, .. }

-- ---------------------------------------------------------------------------
-- HTTP Session (HTTP runner + session id)
-- ---------------------------------------------------------------------------

-- | HTTP environment extended with a session id.
data HttpSessionEnv = MkHttpSessionEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner IO,
    httpSessionId :: Text
  }

instance HasLogFunc HttpSessionEnv where
  logFuncL :: Lens' HttpSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpSessionEnv{..} l -> MkHttpSessionEnv { logFunc = l, .. }

instance HasHttpRunner HttpSessionEnv where
  httpRunnerL :: Lens' HttpSessionEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkHttpSessionEnv{..} r -> MkHttpSessionEnv { httpRunner = r, .. }

instance HasHttpSession HttpSessionEnv where
  getHttpSessionId :: HttpSessionEnv -> Text
  getHttpSessionId = (.httpSessionId)

-- ---------------------------------------------------------------------------
-- BiDi Session (BiDi runner + session id)
-- ---------------------------------------------------------------------------

-- | BiDi environment extended with a session id.
data BiDiSessionEnv = MkBiDiSessionEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner,
    biDiSessionId :: Text
  }

instance HasLogFunc BiDiSessionEnv where
  logFuncL :: Lens' BiDiSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkBiDiSessionEnv{..} l -> MkBiDiSessionEnv { logFunc = l, .. }

instance HasBiDiRunner BiDiSessionEnv where
  biDiRunnerL :: Lens' BiDiSessionEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkBiDiSessionEnv{..} r -> MkBiDiSessionEnv { biDiRunner = r, .. }

instance HasBiDiSession BiDiSessionEnv where
  getBiDiSessionId :: BiDiSessionEnv -> Text
  getBiDiSessionId = (.biDiSessionId)

-- ---------------------------------------------------------------------------
-- Dual Session (HTTP + BiDi runners + session id)
-- ---------------------------------------------------------------------------

-- | Dual environment extended with a session id.
data DualSessionEnv = MkDualSessionEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner IO,
    biDiRunner :: BiDiRunner,
    httpSessionId :: Text,
    biDiSessionId :: Text
  }

instance HasLogFunc DualSessionEnv where
  logFuncL :: Lens' DualSessionEnv LogFunc
  logFuncL = lens (.logFunc) \MkDualSessionEnv{..} l -> MkDualSessionEnv { logFunc = l, .. }

instance HasHttpRunner DualSessionEnv where
  httpRunnerL :: Lens' DualSessionEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkDualSessionEnv{..} h -> MkDualSessionEnv { httpRunner = h, .. }

instance HasBiDiRunner DualSessionEnv where
  biDiRunnerL :: Lens' DualSessionEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \MkDualSessionEnv{..} b -> MkDualSessionEnv { biDiRunner = b, .. }

instance HasHttpSession DualSessionEnv where
  getHttpSessionId :: DualSessionEnv -> Text
  getHttpSessionId = (.httpSessionId)

instance HasBiDiSession DualSessionEnv where
  getBiDiSessionId :: DualSessionEnv -> Text
  getBiDiSessionId = (.biDiSessionId)
