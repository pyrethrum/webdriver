{-# LANGUAGE CPP #-}

module WebDriverPreCore.Test.ConfigLoader
  ( loadConfig,
    module WebDriverPreCore.Test.Config
  )
where

import WebDriverPreCore.Test.Config
  ( Config (..),
    DemoBrowser (..),
  )
import WebDriverPreCore.Test.IOUtils (findWebDriverRoot)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Dhall (auto, input)
import System.Directory (getCurrentDirectory)

loadConfig :: IO Config
loadConfig = do
  webDriverRoot <- getCurrentDirectory >>= pure . fromMaybe (error "Could not find webdriver root") . findWebDriverRoot
  let 
    configPath = webDriverRoot <> "/dev/config-ci.dhall"
#ifdef DEBUG_LOCAL_CONFIG
  putStrLn $ "Loading config from (DEBUG local): " <> configPath
#endif
  input auto $ pack configPath
