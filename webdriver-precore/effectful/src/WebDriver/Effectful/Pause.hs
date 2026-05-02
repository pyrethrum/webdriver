-- |
-- Module: WebDriver.Effectful.Pause
-- Description: LogPause effect for pacing WebDriver actions
--
-- Provides the 'LogPause' static effect and 'pause', which sleeps for a
-- configurable duration between driver actions.
--
-- Typical usage:
--
-- @
-- withLogPause (100 * milliseconds) $ do
--   pause
--   navigate myUrl
-- @
module WebDriver.Effectful.Pause
  ( -- * Pause effect
    Pause,

    -- * Pause introducer
    withPause,

    -- * LogPause operation
    pause,
  )
where

import Control.Concurrent (threadDelay)
import Effectful (Effect, Dispatch (..), DispatchOf, Eff, IOE, (:>), liftIO)
import Effectful.Dispatch.Static
  ( StaticRep,
    SideEffects (..),
    evalStaticRep,
    getStaticRep,
  )
import WebDriverPreCore.Utils.Timeout (Timeout (..))

-- ---------------------------------------------------------------------------
-- LogPause effect
-- ---------------------------------------------------------------------------

-- | Effectful static effect carrying a configurable pause duration.
--
-- Introduce with 'withLogPause'; use 'pause' to sleep between actions.
data Pause :: Effect

type instance DispatchOf Pause = Static NoSideEffects

-- | The static rep holds the pause 'Timeout'.
newtype instance StaticRep Pause = LogPauseRep Timeout

-- ---------------------------------------------------------------------------
-- LogPause introducer
-- ---------------------------------------------------------------------------

-- | Run an action with a 'Pause' effect providing the given 'Timeout'.
withPause :: Timeout -> Eff (Pause : es) a -> Eff es a
withPause = evalStaticRep . LogPauseRep

-- ---------------------------------------------------------------------------
-- Pause operation
-- ---------------------------------------------------------------------------

-- | Sleep for the duration stored in the 'LogPause' effect.
pause :: (Pause :> es, IOE :> es) => Eff es ()
pause = do
  LogPauseRep d <- getStaticRep @Pause
  liftIO $ threadDelay d.microseconds
