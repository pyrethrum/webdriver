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
  )
where

import RIO (HasLogFunc (..), Lens', LogFunc, lens, view, (^.))
import WebDriverPreCore.Extended.Capabilities (FullCapabilitiesRequest)

import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.HttpRunner (HttpRunner)

-- | Env has an 'HttpRunner' available.
class HasHttpRunner env where
  httpRunnerL :: Lens' env HttpRunner

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env BiDiRunner

-- | Env carries 'FullCapabilitiesRequest'.
class HasCapabilities env where
  capabilitiesL :: Lens' env FullCapabilitiesRequest
  
-- ---------------------------------------------------------------------------
-- Base layer
-- ---------------------------------------------------------------------------

-- | Environment used before a runner is established.
data BaseEnv = MkBaseEnv
  { logFunc :: LogFunc,
    capabilities :: FullCapabilitiesRequest
  }

instance HasLogFunc BaseEnv where
  logFuncL :: Lens' BaseEnv LogFunc
  logFuncL = lens (.logFunc) \(MkBaseEnv _ c) l -> MkBaseEnv l c

instance HasCapabilities BaseEnv where
  capabilitiesL :: Lens' BaseEnv FullCapabilitiesRequest
  capabilitiesL = lens (.capabilities) \(MkBaseEnv l _) c -> MkBaseEnv l c


-- ---------------------------------------------------------------------------
-- Inner layer — HTTP only
-- ---------------------------------------------------------------------------

-- | Inner environment carrying an HTTP runner.
data HttpEnv = MkHttpEnv
  { logFunc :: LogFunc,
    capabilities :: FullCapabilitiesRequest,
    httpRunner :: HttpRunner
  }

instance HasLogFunc HttpEnv where
  logFuncL :: Lens' HttpEnv LogFunc
  logFuncL = lens (.logFunc) \(MkHttpEnv _ c r) l -> MkHttpEnv l c r

instance HasCapabilities HttpEnv where
  capabilitiesL :: Lens' HttpEnv FullCapabilitiesRequest
  capabilitiesL = lens (.capabilities) \(MkHttpEnv l _ r) c -> MkHttpEnv l c r

instance HasHttpRunner HttpEnv where
  httpRunnerL :: Lens' HttpEnv HttpRunner
  httpRunnerL = lens (.httpRunner) \(MkHttpEnv l c _) r -> MkHttpEnv l c r

-- ---------------------------------------------------------------------------
-- Inner layer — BiDi only
-- ---------------------------------------------------------------------------

-- | Inner environment carrying a BiDi runner.
data BiDiEnv = MkBiDiEnv
  { logFunc :: LogFunc,
    capabilities :: FullCapabilitiesRequest,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc BiDiEnv where
  logFuncL :: Lens' BiDiEnv LogFunc
  logFuncL = lens (.logFunc) \(MkBiDiEnv _ c r) l -> MkBiDiEnv l c r

instance HasCapabilities BiDiEnv where
  capabilitiesL :: Lens' BiDiEnv FullCapabilitiesRequest
  capabilitiesL = lens (.capabilities) \(MkBiDiEnv l _ r) c -> MkBiDiEnv l c r

instance HasBiDiRunner BiDiEnv where
  biDiRunnerL :: Lens' BiDiEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \(MkBiDiEnv l c _) r -> MkBiDiEnv l c r

-- ---------------------------------------------------------------------------
-- Inner layer — Dual (HTTP + BiDi)
-- ---------------------------------------------------------------------------

-- | Inner environment carrying both HTTP and BiDi runners.
data DualEnv = MkDualEnv
  { logFunc :: LogFunc,
    capabilities :: FullCapabilitiesRequest,
    httpRunner :: HttpRunner,
    biDiRunner :: BiDiRunner
  }

instance HasLogFunc DualEnv where
  logFuncL :: Lens' DualEnv LogFunc
  logFuncL = lens (.logFunc) \(MkDualEnv _ c h b) l -> MkDualEnv l c h b

instance HasCapabilities DualEnv where
  capabilitiesL :: Lens' DualEnv FullCapabilitiesRequest
  capabilitiesL = lens (.capabilities) \(MkDualEnv l _ h b) c -> MkDualEnv l c h b

instance HasHttpRunner DualEnv where
  httpRunnerL :: Lens' DualEnv HttpRunner
  httpRunnerL = lens (.httpRunner) \(MkDualEnv l c _ b) h -> MkDualEnv l c h b

instance HasBiDiRunner DualEnv where
  biDiRunnerL :: Lens' DualEnv BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \(MkDualEnv l c h _) b -> MkDualEnv l c h b
