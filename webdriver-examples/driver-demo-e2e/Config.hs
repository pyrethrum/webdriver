{-# LANGUAGE OverloadedStrings #-}

module Config where

import Prelude
import Data.Text


wantConsoleLogging :: Bool
wantConsoleLogging = True

-- set to False for Chrome
useFirefox :: Bool
useFirefox = True

-- see readme
customFirefoxProfilePath :: Maybe Text
customFirefoxProfilePath = Nothing

-- customFirefoxProfilePath = Just "./webdriver-examples/driver-demo-e2e/.profile/WebDriverProfile"