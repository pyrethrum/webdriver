{-# LANGUAGE OverloadedStrings #-}

module Config where

import Prelude
import Data.Text

wantConsoleLogging :: Bool
wantConsoleLogging = True

-- set to False for Chrome
useFirefox :: Bool
useFirefox = True

-- very boring to watch if set to True
firefoxHeadless :: Bool
firefoxHeadless = False

-- see readme
customFirefoxProfilePath :: Maybe Text
customFirefoxProfilePath = Just "/home/john-walker/snap/firefox/common/.cache/mozilla/firefox/2c77yj8o.Another"
-- customFirefoxProfilePath = Just "./webdriver-examples/driver-demo-e2e/.profile/WebDriverProfile"
-- customFirefoxProfilePath = Nothing
