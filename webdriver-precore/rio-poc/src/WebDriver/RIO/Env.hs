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
  ( -- * Base Layer
    BaseEnv (..),

    -- * Inner Layer
    HttpEnv (..),
    BiDiEnv (..),
    DualEnv (..),

    -- * Runner Typeclasses
    HasHttpRunner (..),
    HasBiDiRunner (..),

    -- * Capabilities Typeclass
    HasCapabilities (..),
    HasCapabilitiesResponse (..),
  )
where

import RIO (HasLogFunc (..), Lens', LogFunc, lens)
import WebDriverPreCore.Extended.Capabilities 
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.HttpRunner (HttpRunner)

-- | Env has an 'HttpRunner' available.
class HasHttpRunner env where
  httpRunnerL :: Lens' env HttpRunner

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env BiDiRunner

-- | HTTP runner.
data HttpEnv = MkHttpEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner
  }

instance HasLogFunc HttpEnv where
  logFuncL :: Lens' HttpEnv LogFunc
  logFuncL = lens (.logFunc) \(MkHttpEnv _ c r) l -> MkHttpEnv l c r

instance HasHttpRunner HttpEnv where
  httpRunnerL :: Lens' HttpEnv HttpRunner
  httpRunnerL = lens (.httpRunner) \(MkHttpEnv l c _) r -> MkHttpEnv l c r

-- ---------------------------------------------------------------------------
-- BiDi only
-- ---------------------------------------------------------------------------


data BiDiEnv = MkBiDiEnv
  { logFunc :: LogFunc,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc BiDiEnv where
  logFuncL :: Lens' BiDiEnv LogFunc
  logFuncL = lens (.logFunc) \(MkBiDiEnv _ c cr r) l -> MkBiDiEnv l c cr r

instance HasBiDiRunner BiDiEnv where
  biDiRunnerL :: Lens' BiDiEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \(MkBiDiEnv l c cr _) r -> MkBiDiEnv l c cr r

-- ---------------------------------------------------------------------------
-- Dual (HTTP + BiDi)
-- ---------------------------------------------------------------------------

-- | Inner environment carrying both HTTP and BiDi runners.
data DualEnv = MkDualEnv
  { logFunc :: LogFunc,
    httpRunner :: HttpRunner,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc DualEnv where
  logFuncL :: Lens' DualEnv LogFunc
  logFuncL = lens (.logFunc) \(MkDualEnv _ c cr h b) l -> MkDualEnv l c cr h b

instance HasHttpRunner DualEnv where
  httpRunnerL :: Lens' DualEnv HttpRunner
  httpRunnerL = lens (.httpRunner) \(MkDualEnv l c cr _ b) h -> MkDualEnv l c cr h b

instance HasBiDiRunner DualEnv where
  biDiRunnerL :: Lens' DualEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \(MkDualEnv l c cr h _) b -> MkDualEnv l c cr h b
