{-# LANGUAGE DerivingVia #-}

-- |
-- Module: WebDriver.Bluefin.Core
-- Description: LogPause effect — a Bluefin handle for logging and pausing
--
-- Provides a first-class Bluefin effect that bundles console logging and
-- configurable pause delays into a single handle.  Using 'withLogPause' to
-- introduce the handle lets call-sites write:
--
-- @
-- withLogPause io behaviour.pauseDuration $ \lp -> do
--   log lp "=== step ==="
--   -- … actions …
--   pause lp
-- @
--
-- instead of threading 'IOE' and 'HttpSessionEnv' / 'BiDiEnv' through every
-- helper just to sleep or print.
module WebDriver.Bluefin.Core
  ( -- * Handle
    LogPause (..),

    -- * Introducer
    withLogPause,

    -- * Effects
    log,
    pause,
  )
where

import Prelude hiding (log)

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text qualified as T
import Bluefin.Compound (Handle, OneWayCoercible (..), OneWayCoercibleHandle (..), gOneWayCoercible)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE, effIO)
import GHC.Generics (Generic)
import WebDriverPreCore.Utils.Timeout (Timeout (..))

-- ---------------------------------------------------------------------------
-- Handle
-- ---------------------------------------------------------------------------

-- | Bluefin handle that carries an 'IOE' handle and a pause duration.
--
-- Introduce it with 'withLogPause'; use 'log' and 'pause' to consume it.
data LogPause e = MkLogPause
  { lpIO           :: IOE e,
    lpPauseDuration :: Timeout
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle LogPause

instance (e :> es) => OneWayCoercible (LogPause e) (LogPause es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- ---------------------------------------------------------------------------
-- Introducer
-- ---------------------------------------------------------------------------

-- | Run an action with a 'LogPause' handle built from an existing 'IOE' and
-- a pause duration.
--
-- @
-- withLogPause io (5 * seconds) $ \lp -> do
--   log lp "Starting…"
--   pause lp
-- @
withLogPause
  :: IOE e
  -> Timeout
  -> (LogPause e -> Eff es a)
  -> Eff es a
withLogPause io dur action = action MkLogPause {lpIO = io, lpPauseDuration = dur}

-- ---------------------------------------------------------------------------
-- Effects
-- ---------------------------------------------------------------------------

-- | Print a labelled info message to stdout.
--
-- Produces output of the form @[INFO] \<message\>@.
log :: (e :> es) => LogPause e -> Text -> Eff es ()
log lp t = effIO lp.lpIO $ putStrLn ("[INFO] " <> T.unpack t)

-- | Sleep for the 'lpPauseDuration' stored in the 'LogPause' handle.
pause :: (e :> es) => LogPause e -> Eff es ()
pause lp = effIO lp.lpIO $ threadDelay (let MkTimeout us = lp.lpPauseDuration in us)
