{-|
Module: WebDriver.RIO.Runner
Description: Has-style typeclasses for runner and capabilities access

Defines lens-based typeclasses following RIO's @Has*@ pattern so that
actions can be polymorphic over which env (and therefore which runners)
they operate in. No runner construction functions or actions — just the
typeclass definitions.
-}
module WebDriver.RIO.Runner
  ( -- * Runner Access
    HasHttpRunner (..),
    HasBiDiRunner (..),

    -- * Capabilities Access
    HasCapabilities (..),
  )
where

import RIO (Lens')
import WebDriver.RIO.Capabilities (FullCapabilities)
import WebDriverPreCore.BiDiRunner (BiDiRunner)
import WebDriverPreCore.HttpRunner (HttpRunner)

-- | Env has an 'HttpRunner' available.
class HasHttpRunner env where
  httpRunnerL :: Lens' env HttpRunner

-- | Env has a 'BiDiRunner' available.
class HasBiDiRunner env where
  biDiRunnerL :: Lens' env BiDiRunner

-- | Env carries 'FullCapabilities' parameterised by @cap@.
--
-- The functional dependency @env -> cap@ ensures the capability type is
-- determined by the environment, preventing ambiguous type variable errors
-- at call sites.
class HasCapabilities env cap | env -> cap where
  capabilitiesL :: Lens' env (FullCapabilities cap)
