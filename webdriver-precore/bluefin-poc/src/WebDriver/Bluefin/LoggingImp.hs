-- |
-- Module: WebDriver.Bluefin.LoggingImp
-- Description: Katip-based logging implementation for Bluefin WebDriver tests
--
-- Re-exports 'Severity' and 'withKatipLogFunc' from the shared
-- @webdriver-katip-logging@ private library.
--
-- See "WebDriver.KatipLogging" for full documentation.
module WebDriver.Bluefin.LoggingImp
  ( -- * Re-exports from WebDriver.KatipLogging
    Severity (..),
    withKatipLogFunc,
  )
where

import WebDriver.KatipLogging (Severity (..), withKatipLogFunc)
