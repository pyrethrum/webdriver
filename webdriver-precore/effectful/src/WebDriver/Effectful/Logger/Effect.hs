-- |
-- Module: WebDriver.Effectful.Logger.Effect
-- Description: Generic dynamic Logger effect
--
-- Defines the 'Logger' algebraic effect and its associated 'Severity' type.
-- The effect has a single constructor, 'LogAtSev', from which all the
-- convenience helpers ('log', 'logInfo', …) are derived.
--
-- This module is backend-agnostic.  Wire in a concrete interpreter
-- (e.g. "WebDriver.Effectful.Logger.KatipInterpreter.runLogger")
-- to discharge the effect.
module WebDriver.Effectful.Logger.Effect
  ( -- * Severity
    Severity (..),

    -- * Logger effect
    Logger (..),

    -- * Logger operations
    logAtSev,
    log,
    logDebug,
    logInfo,
    logWarn,
    logError,
  )
where

import Data.Text (Text)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Prelude hiding (log)

-- ---------------------------------------------------------------------------
-- Severity
-- ---------------------------------------------------------------------------

-- | Log severity levels, ordered from least to most severe.
--
-- Mirrors Katip's @Severity@ constructors without the trailing @S@ suffix.
data Severity
  = Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency
  deriving (Eq, Ord, Show, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Logger effect
-- ---------------------------------------------------------------------------

-- | Generic dynamic logger effect.
--
-- The single constructor 'LogAtSev' is the only primitive operation.
-- Convenience helpers ('logInfo', 'logDebug', etc.) are plain functions
-- built on top of it.
--
-- Introduce with an interpreter such as
-- 'WebDriver.Effectful.Logger.KatipInterpreter.runLogger'.
data Logger :: Effect where
  LogAtSev :: Severity -> Text -> Logger m ()

type instance DispatchOf Logger = Dynamic

-- ---------------------------------------------------------------------------
-- Logger operations
-- ---------------------------------------------------------------------------

-- | Emit a log message at the specified 'Severity'.
logAtSev :: (Logger :> es) => Severity -> Text -> Eff es ()
logAtSev sev txt = send $ LogAtSev sev txt

-- | Emit a message at 'Info' severity.  Alias for 'logInfo'.
log :: (Logger :> es) => Text -> Eff es ()
log = logInfo

-- | Emit a message at 'Debug' severity.
logDebug :: (Logger :> es) => Text -> Eff es ()
logDebug = logAtSev Debug

-- | Emit a message at 'Info' severity.
logInfo :: (Logger :> es) => Text -> Eff es ()
logInfo = logAtSev Info

-- | Emit a message at 'Warning' severity.
logWarn :: (Logger :> es) => Text -> Eff es ()
logWarn = logAtSev Warning

-- | Emit a message at 'Error' severity.
logError :: (Logger :> es) => Text -> Eff es ()
logError = logAtSev Error
