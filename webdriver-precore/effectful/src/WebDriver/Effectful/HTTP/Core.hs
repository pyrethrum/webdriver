-- |
-- Module: WebDriver.Effectful.HTTP.Core
-- Description: Core types and effects for Effectful WebDriver
--
-- Defines the types and algebraic effects used throughout the Effectful POC:
--
-- * 'HttpDriverInfo'   — HTTP connection configuration
-- * 'HttpSessionInfo'  — driver info + session + pause duration
-- * 'BiDiInfo'         — BiDi runner + pause duration
-- * 'WebDriverHttp'    — Dynamic effect encoding all HTTP session operations
-- * 'WebDriverBiDi'    — Dynamic effect encoding all BiDi commands + subscriptions
--
-- The effects are dispatched dynamically: 'runWebDriverHttp' and
-- 'runWebDriverBiDi' provide the @IO@-backed interpreters.  The separation
-- of "what" (effect algebra) from "how" (interpreter) means you can add
-- alternative interpreters (e.g. pure test doubles) without changing
-- call-site code.
--
-- This mirrors "WebDriver.Bluefin.HTTP.Core" but uses Effectful algebraic
-- effects instead of explicit Bluefin compound handles.
module WebDriver.Effectful.HTTP.Core
  ( -- * Types
    HttpDriverInfo (..),
    HttpSessionInfo (..),
    BiDiInfo (..),
    defaultDriverInfo,

    -- * HTTP Effect
    WebDriverHttp (..),

    -- * BiDi Effect
    WebDriverBiDi (..),

    -- * HTTP Interpreter
    runWebDriverHttp,

    -- * BiDi Interpreter
    runWebDriverBiDi,

    -- * Internal helpers (re-exported for App module)
    mkSessionRunner,
  )
where

import WebDriver.Effectful.BiDi.Base.Effect
  ( BiDiInfo (..),
    WebDriverBiDi (..),
  )
import WebDriver.Effectful.BiDi.Base.Interpreter (runWebDriverBiDi)
import WebDriver.Effectful.HTTP.Base.Effect
  ( HttpDriverInfo (..),
    HttpSessionInfo (..),
    WebDriverHttp (..),
    defaultDriverInfo,
    mkSessionRunner,
  )
import WebDriver.Effectful.HTTP.Base.Interpreter (runWebDriverHttp)
