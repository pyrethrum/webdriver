{-|
Module: WebDriver.RIO.Env
Description: RIO environment types for outer and inner WebDriver layers

Defines four environment data types:

* 'OuterEnv'  — pre-runner layer with logging + capabilities
* 'HttpEnv'   — inner layer with HTTP runner
* 'BiDiEnv'   — inner layer with BiDi runner
* 'DualEnv'   — inner layer with both HTTP and BiDi runners

All implement 'HasLogFunc' and 'HasCapabilities'; inner envs additionally
implement the appropriate runner typeclasses.
-}
module WebDriver.RIO.Env
  ( -- * Outer Layer
    OuterEnv (..),

    -- * Inner Layer
    HttpEnv (..),
    BiDiEnv (..),
    DualEnv (..),
  )
where

import RIO (HasLogFunc (..), LogFunc, lens, Lens')
import WebDriver.RIO.Capabilities (FullCapabilities)
import WebDriver.RIO.Runner
  ( HasBiDiRunner (..),
    HasCapabilities (..),
    HasHttpRunner (..),
  )
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.HttpRunner (HttpRunner)

-- ---------------------------------------------------------------------------
-- Outer layer
-- ---------------------------------------------------------------------------

-- | Environment used before a runner is established.
--   Parameterised by capability type so callers lock in HTTP or BiDi
--   at the type level.
data OuterEnv cap = MkOuterEnv
  { logFunc :: LogFunc
  , capabilities :: FullCapabilities cap
  }

instance HasLogFunc (OuterEnv cap) where
  logFuncL :: Lens' (OuterEnv cap) LogFunc
  logFuncL = lens (.logFunc) \(MkOuterEnv _ c) l -> MkOuterEnv l c

instance HasCapabilities (OuterEnv cap) cap where
  capabilitiesL :: Lens' (OuterEnv cap) (FullCapabilities cap)
  capabilitiesL = lens (.capabilities) \(MkOuterEnv l _) c -> MkOuterEnv l c

-- ---------------------------------------------------------------------------
-- Inner layer — HTTP only
-- ---------------------------------------------------------------------------

-- | Inner environment carrying an HTTP runner.
data HttpEnv cap = MkHttpEnv
  { logFunc :: LogFunc
  , capabilities :: FullCapabilities cap
  , httpRunner :: HttpRunner
  }

instance HasLogFunc (HttpEnv cap) where
  logFuncL :: Lens' (HttpEnv cap) LogFunc
  logFuncL = lens (.logFunc) \(MkHttpEnv _ c r) l -> MkHttpEnv l c r

instance HasCapabilities (HttpEnv cap) cap where
  capabilitiesL :: Lens' (HttpEnv cap) (FullCapabilities cap)
  capabilitiesL = lens (.capabilities) \(MkHttpEnv l _ r) c -> MkHttpEnv l c r

instance HasHttpRunner (HttpEnv cap) where
  httpRunnerL :: Lens' (HttpEnv cap) HttpRunner
  httpRunnerL = lens (.httpRunner) \(MkHttpEnv l c _) r -> MkHttpEnv l c r

-- ---------------------------------------------------------------------------
-- Inner layer — BiDi only
-- ---------------------------------------------------------------------------

-- | Inner environment carrying a BiDi runner.
data BiDiEnv cap = MkBiDiEnv
  { logFunc :: LogFunc
  , capabilities :: FullCapabilities cap
  , biDiRunner :: BiDiRunner
  }

instance HasLogFunc (BiDiEnv cap) where
  logFuncL :: Lens' (BiDiEnv cap) LogFunc
  logFuncL = lens (.logFunc) \(MkBiDiEnv _ c r) l -> MkBiDiEnv l c r

instance HasCapabilities (BiDiEnv cap) cap where
  capabilitiesL :: Lens' (BiDiEnv cap) (FullCapabilities cap)
  capabilitiesL = lens (.capabilities) \(MkBiDiEnv l _ r) c -> MkBiDiEnv l c r

instance HasBiDiRunner (BiDiEnv cap) where
  biDiRunnerL :: Lens' (BiDiEnv cap) BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \(MkBiDiEnv l c _) r -> MkBiDiEnv l c r

-- ---------------------------------------------------------------------------
-- Inner layer — Dual (HTTP + BiDi)
-- ---------------------------------------------------------------------------

-- | Inner environment carrying both HTTP and BiDi runners.
data DualEnv cap = MkDualEnv
  { logFunc :: LogFunc
  , capabilities :: FullCapabilities cap
  , httpRunner :: HttpRunner
  , biDiRunner :: BiDiRunner
  }

instance HasLogFunc (DualEnv cap) where
  logFuncL :: Lens' (DualEnv cap) LogFunc
  logFuncL = lens (.logFunc) \(MkDualEnv _ c h b) l -> MkDualEnv l c h b

instance HasCapabilities (DualEnv cap) cap where
  capabilitiesL :: Lens' (DualEnv cap) (FullCapabilities cap)
  capabilitiesL = lens (.capabilities) \(MkDualEnv l _ h b) c -> MkDualEnv l c h b

instance HasHttpRunner (DualEnv cap) where
  httpRunnerL :: Lens' (DualEnv cap) HttpRunner
  httpRunnerL = lens (.httpRunner) \(MkDualEnv l c _ b) h -> MkDualEnv l c h b

instance HasBiDiRunner (DualEnv cap) where
  biDiRunnerL :: Lens' (DualEnv cap) BiDiRunner
  biDiRunnerL = lens (.biDiRunner) \(MkDualEnv l c h _) b -> MkDualEnv l c h b
