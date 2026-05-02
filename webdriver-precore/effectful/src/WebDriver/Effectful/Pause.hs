-- |
-- Module: WebDriver.Effectful.Pause
-- Description: Pause effect for pacing WebDriver actions
--
-- Provides the 'Pause' dynamic effect and 'pause', which sleeps for a
-- configurable duration between driver actions.
--
-- Typical usage:
--
-- @
-- runPause (100 * milliseconds) $ do
--   pause
--   navigate myUrl
-- @
module WebDriver.Effectful.Pause
  ( -- * Pause effect
    Pause,

    -- * Pause runners
    runPause,
    runNoPause,

    -- * Pause operations
    pause,
    pauseAtLeast,
    sleep,
  )
where

import Control.Concurrent (threadDelay)
import Effectful (Effect, Dispatch (..), DispatchOf, Eff, IOE, (:>), liftIO)
import Effectful.Dispatch.Dynamic (interpret, send)
import WebDriverPreCore.Utils.Timeout (Timeout (..))

-- ---------------------------------------------------------------------------
-- Pause effect
-- ---------------------------------------------------------------------------

-- | Dynamic effect for configurable pausing between driver actions.
data Pause :: Effect where
  Pause        :: Pause m ()
  PauseAtLeast :: Timeout -> Pause m ()
  Sleep        :: Timeout -> Pause m ()

type instance DispatchOf Pause = Dynamic

-- ---------------------------------------------------------------------------
-- Pause runners
-- ---------------------------------------------------------------------------

-- | Run with the 'Pause' effect, sleeping for the given 'Timeout' on each 'pause'.
--
--   * 'pause'        — sleeps for @defaultPause@
--   * 'pauseAtLeast' — sleeps for @max t defaultPause@
--   * 'sleep'        — sleeps for exactly @t@ regardless of the default
runPause :: IOE :> es => Timeout -> Eff (Pause : es) a -> Eff es a
runPause defaultPause = interpret $ \_ op -> 
  liftIO $  
    case op of
      Pause          -> threadDelay defaultPause.microseconds
      PauseAtLeast t -> threadDelay $ max t.microseconds defaultPause.microseconds
      Sleep t        -> threadDelay t.microseconds

-- | Run with the 'Pause' effect where pauses are no-ops; 'sleep' still waits.
--
--   * 'pause'        — no-op
--   * 'pauseAtLeast' — no-op
--   * 'sleep'        — sleeps for exactly @t@
runNoPause :: IOE :> es => Eff (Pause : es) a -> Eff es a
runNoPause = interpret $ \_ op -> case op of
  Pause          -> pure ()
  PauseAtLeast _ -> pure ()
  Sleep t        -> liftIO $ threadDelay t.microseconds

-- ---------------------------------------------------------------------------
-- Pause operations
-- ---------------------------------------------------------------------------

-- | Sleep for the default duration configured in the runner, or do nothing with 'runNoPause'.
pause :: Pause :> es => Eff es ()
pause = send Pause

-- | Sleep for at least the given duration (or the runner default, whichever is larger).
--   No-op under 'runNoPause'.
pauseAtLeast :: Pause :> es => Timeout -> Eff es ()
pauseAtLeast = send . PauseAtLeast

-- | Sleep for exactly the given duration in both runners.
sleep :: Pause :> es => Timeout -> Eff es ()
sleep = send . Sleep