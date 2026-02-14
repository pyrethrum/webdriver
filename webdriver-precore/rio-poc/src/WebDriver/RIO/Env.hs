{-# LANGUAGE DataKinds #-}

-- |
-- Module: WebDriver.RIO.Env
-- Description: RIO environment types for outer and inner WebDriver layers
--
-- Defines four environment data types:
--
-- * 'BaseEnv'   — pre-runner layer with logging + capabilities
-- * 'HttpEnv'   — inner layer with HTTP runner
-- * 'BiDiEnv'   — inner layer with BiDi runner
-- * 'DualEnv'   — inner layer with both HTTP and BiDi runners
--
-- All implement 'HasLogFunc' and 'HasCapabilities'; inner envs additionally
-- implement the appropriate runner typeclasses.
module WebDriver.RIO.Env
  ( 
    LoggerEnv (..),
    -- * Inner Layer
    HttpEnv (..),
    BiDiEnv (..),
    DualEnv (..),

    -- * Runner Typeclasses
    HasHttpRunner (..),
    HasBiDiRunner (..),
  )
where

import RIO (HasLogFunc (..), Lens', LogFunc, lens)
import WebDriverPreCore.Extended.Capabilities 
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.HttpRunner (HttpRunner)

-- | Env has an 'HttpRunner' available.
class HasHttpRunner env where
  httpRunnerL :: Lens' env (HttpRunner IO)

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env BiDiRunner

-- | HTTP runner.
data HttpEnv = MkHttpEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner IO
  }

instance HasLogFunc HttpEnv where
  logFuncL :: Lens' HttpEnv LogFunc
  logFuncL = lens (.logFunc) \MkHttpEnv{logFunc, ..} l -> MkHttpEnv { logFunc = l, .. }

instance HasHttpRunner HttpEnv where
  httpRunnerL :: Lens' HttpEnv (HttpRunner IO)
  httpRunnerL = lens (.httpRunner) \MkHttpEnv{..} r -> MkHttpEnv { httpRunner = r, .. }



data LoggerEnv = MkLoggerEnv
  { logFunc :: LogFunc
  }

instance HasLogFunc LoggerEnv where
  logFuncL :: Lens' LoggerEnv LogFunc
  logFuncL = lens (.logFunc) \MkLoggerEnv{..} l -> MkLoggerEnv { logFunc = l, .. }


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
