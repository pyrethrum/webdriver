{-# LANGUAGE DerivingVia #-}

-- |
-- Module: WebDriver.Bluefin.HTTP.Core
-- Description: Core handle types and helpers for Bluefin WebDriver
--
-- Defines the environment handle types used throughout the Bluefin POC:
--
-- * 'HttpEnv'          — IOE handle + HTTP endpoint + logging; used for root HTTP methods
-- * 'HttpSessionEnv'   — 'HttpEnv' fields + session + pause duration
-- * 'BiDiEnv'          — IOE handle + BiDiRunner IO + pause duration
--
-- All handle types are proper Bluefin compound effects: they derive 'Handle'
-- via 'OneWayCoercibleHandle' so they can be used with 'mapHandle',
-- 'useImplIn', etc.  Functions receive handles explicitly rather than
-- implicitly via typeclasses.
module WebDriver.Bluefin.HTTP.Core
  ( -- * Handles
    HttpEnv (..),
    HttpSessionEnv (..),
    BiDiEnv (..),

    -- * IO runner builders (for use by Actions modules)
    mkEnvRunner,
    mkSessionRunner,

    -- * Higher-level command runners
    runHttpCommand,
    runBiDiCommand,
    getBiDiRunner,
  )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Bluefin.Compound (Handle, OneWayCoercible (..), OneWayCoercibleHandle (..), gOneWayCoercible)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE, effIO)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDiRunner (BiDiRunner (..))
import WebDriverPreCore.BiDi.Protocol (Command)
import WebDriverPreCore.Error (parseFailToWDException)
import WebDriverPreCore.Extended.HTTP.Base.Actions (Runner)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (Session)
import WebDriverPreCore.HTTP.Command qualified as HC
import WebDriverPreCore.HttpRunner (HttpEndpoint (..), callWebDriver)
import WebDriverPreCore.Utils.Timeout (Timeout (..))
import UnliftIO (throwIO)

-- ---------------------------------------------------------------------------
-- Handles
-- ---------------------------------------------------------------------------

-- | Environment handle for HTTP runner operations (no session required).
--
-- A proper Bluefin compound handle wrapping 'IOE' and driver config.
data HttpEnv e = MkHttpEnv
  { httpEndpoint :: HttpEndpoint,
    -- | Each driver request/response is logged via this function.
    -- Set from a Katip 'Logger' by 'WebDriver.Bluefin.App.withHttpSession' /
    -- 'WebDriver.Bluefin.App.withBiDiSession'.
    driverLogFn :: Text -> IO (),
    envIO :: IOE e
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle HttpEnv

instance (e :> es) => OneWayCoercible (HttpEnv e) (HttpEnv es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- | Environment handle for session-scoped HTTP operations.
--
-- A proper Bluefin compound handle wrapping 'IOE', driver config, session
-- and pause duration.
data HttpSessionEnv e = MkHttpSessionEnv
  { httpEndpoint :: HttpEndpoint,
    -- | Each driver request/response is logged via this function.
    driverLogFn :: Text -> IO (),
    httpSession :: Session,
    pauseDuration :: Timeout,
    envIO :: IOE e
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle HttpSessionEnv

instance (e :> es) => OneWayCoercible (HttpSessionEnv e) (HttpSessionEnv es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- | Environment handle for BiDi operations.
--
-- A proper Bluefin compound handle wrapping 'IOE', a 'BiDiRunner' (kept in
-- @IO@; commands are lifted into 'Eff' via 'effIO') and pause duration.
data BiDiEnv e = MkBiDiEnv
  { biDiRunner :: BiDiRunner IO,
    pauseDuration :: Timeout,
    biDiIO :: IOE e
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle BiDiEnv

instance (e :> es) => OneWayCoercible (BiDiEnv e) (BiDiEnv es) where
  oneWayCoercibleImpl = gOneWayCoercible

-- ---------------------------------------------------------------------------
-- IO runner builders
-- ---------------------------------------------------------------------------

-- | Build a @Command a -> IO a@ runner from an 'HttpEnv'.
--
-- The returned runner is polymorphic in @a@ (constrained by 'FromJSON').
mkEnvRunner :: (FromJSON a) => HttpEnv e -> Runner IO a
mkEnvRunner env cmd =
  callWebDriver env.httpEndpoint env.driverLogFn cmd
    >>= either (throwIO . parseFailToWDException) pure

-- | Build a @Command a -> IO a@ runner from an 'HttpSessionEnv'.
mkSessionRunner :: (FromJSON a) => HttpSessionEnv e -> Runner IO a
mkSessionRunner sess cmd =
  callWebDriver sess.httpEndpoint sess.driverLogFn cmd
    >>= either (throwIO . parseFailToWDException) pure

-- ---------------------------------------------------------------------------
-- Higher-level command runners
-- ---------------------------------------------------------------------------

-- | Run a typed HTTP WebDriver 'Command' via an 'HttpEnv' handle.
runHttpCommand :: (e :> es, FromJSON r) => HttpEnv e -> HC.Command r -> Eff es r
runHttpCommand env cmd = effIO env.envIO (mkEnvRunner env cmd)

-- | Run a typed BiDi 'Command' via a 'BiDiEnv' handle.
--
-- The underlying 'BiDiRunner' is @IO@-based; the result is lifted into 'Eff'.
runBiDiCommand :: (e :> es, FromJSON r) => BiDiEnv e -> Command r -> Eff es r
runBiDiCommand MkBiDiEnv {biDiRunner = MkBiDiRunner {run = r}, biDiIO} cmd =
  effIO biDiIO (r cmd)

-- | Extract the 'BiDiRunner IO' from a 'BiDiEnv' handle.
getBiDiRunner :: BiDiEnv e -> BiDiRunner IO
getBiDiRunner = (.biDiRunner)


