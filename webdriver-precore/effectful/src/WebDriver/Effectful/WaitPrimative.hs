-- |
-- Module: WebDriver.Effectful.WiatPrimative
-- Description: Pause effect for pacing WebDriver actions
--
-- Provides the 'Pause' dynamic effect and 'pause', which sleeps for a
-- configurable duration between driver actions.
--
-- Typical usage:
--
-- @
-- runWaitPrimative (100 * milliseconds) $ do
--   pause
--   navigate myUrl
-- @
module WebDriver.Effectful.WaitPrimative
  ( -- * Pause effect
    WaitPrimative,

    -- * Pause runners
    runWaitPrimative,
    runWaitPrimativeNoOp,

    -- * Pause operations
    sleep,
  )
where

import Control.Concurrent (threadDelay)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import WebDriverPreCore.Utils.Timeout (Timeout (..))

-- ---------------------------------------------------------------------------
-- Pause effect
-- ---------------------------------------------------------------------------

-- | Dynamic effect for configurable pausing between driver actions.
data WaitPrimative :: Effect where
  Sleep :: Timeout -> WaitPrimative m ()

type instance DispatchOf WaitPrimative = Dynamic

-- ---------------------------------------------------------------------------
-- Pause runners
-- ---------------------------------------------------------------------------

-- | Run with the 'Pause' effect, sleeping for the given 'Timeout' on each 'pause'.
--
--   * 'sleep'        — sleeps for exactly @t@ regardless of the default
runWaitPrimative :: (IOE :> es) => Eff (WaitPrimative : es) a -> Eff es a
runWaitPrimative = interpret $ \_ op ->
  liftIO $
    case op of
      Sleep t -> threadDelay t.microseconds

-- | Run with the 'Pause' effect where pauses are no-ops; 'sleep' still waits.
--
--   * 'sleep'        — sleeps for exactly @t@
runWaitPrimativeNoOp :: Eff (WaitPrimative : es) a -> Eff es a
runWaitPrimativeNoOp = interpret $ \_ op -> case op of
  Sleep _t -> pure ()

-- ---------------------------------------------------------------------------
-- Pause operations
-- ---------------------------------------------------------------------------

-- | Sleep for exactly the given duration in both runners.
sleep :: (WaitPrimative :> es) => Timeout -> Eff es ()
sleep = send . Sleep