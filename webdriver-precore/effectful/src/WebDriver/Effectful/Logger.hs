-- |
-- Module: WebDriver.Effectful.Logger
-- Description: Logger effect and Katip interpreter for Effectful WebDriver
--
-- Re-exports the generic 'Logger' dynamic effect from
-- "WebDriver.Effectful.Logger.Effect" and the Katip-backed interpreter from
-- "WebDriver.Effectful.Logger.KatipInterpreter".
--
-- Typical usage:
--
-- @
-- withLogger "eval.log" $ do
--   log "session started"
-- @
module WebDriver.Effectful.Logger
  ( module WebDriver.Effectful.Logger.Effect,
    module WebDriver.Effectful.Logger.KatipInterpreter,
  )
where

import WebDriver.Effectful.Logger.Effect
import WebDriver.Effectful.Logger.KatipInterpreter
import Prelude hiding (log)
