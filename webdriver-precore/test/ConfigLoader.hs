{-# LANGUAGE CPP #-}

module ConfigLoader
  ( Config (..),
    DemoBrowser (..),
    isFirefox,
    loadConfig,
  )
where

import Config ( Config (..),
    DemoBrowser (..),
    isFirefox
  )

import Data.Text.IO qualified as T

#ifdef DEBUG_LOCAL_CONFIG
import DebugConfig (debugConfig)
#else
import Control.Monad (unless)
import Data.Text as T (Text, pack, unlines)
import Dhall (auto, input)
import IOUtils (findWebDriverRoot)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (combine, (</>))
#endif

#ifndef DEBUG_LOCAL_CONFIG
configDir :: IO FilePath
configDir = do
  currentDir <- getCurrentDirectory
  case findWebDriverRoot currentDir of
    Just root -> pure $ root </> testSubDir </> ".config"
    Nothing ->
      error $
        "Could not find 'webdriver' root directory from: "
          <> currentDir
          <> "\n tests are expected to be run from the 'webdriver' directory or "
          <> testSubDir
  where
    testSubDir = "webdriver-precore" </> "test"

initialiseTestConfig :: IO ()
initialiseTestConfig = do
  userPath' <- userPath
  exists <- doesFileExist userPath'
  unless exists $ do
    putStrLn $ "Saving default config to: " <> userPath'
    T.writeFile userPath' configText

{-
Generating in code is more principled but produces a less readable file.

import Dhall.Pretty qualified as P
let expr = embed (inject @Config) defaultConfig
     doc = pack (show (P.prettyCharacterSet P.ASCII expr))
 -}
configText :: Text
configText =
  T.unlines
    [ "-- Config types",
      "let Browser = ",
      "      < Chrome : { headless : Bool }",
      "      | Firefox : ",
      "          { headless : Bool",
      "          , profilePath : Optional Text ",
      "          }",
      "      >",
      "",
      "let Config = ",
      "      { browser : Browser",
      "      , logging : Bool",
      "      , httpUrl : Text",
      "      , httpPort : Natural",
      "      , pauseMS : Natural",
      "      }",
      "",
      "-- Config value",
      "let browser : Browser = ",
      "      Browser.Firefox ",
      "        { headless = True",
      "        , profilePath = None Text",
      "        }",
      "",
      "let config : Config = ",
      "      { browser = browser",
      "      , logging = True",
      "      , httpUrl = \"127.0.0.1\"",
      "      , httpPort = 4444",
      "      , pauseMS = 2000",
      "      }",
      "",
      "in config"
    ]

readConfig :: IO Config
readConfig =
  userPath >>= input auto . pack

userPath :: IO FilePath
userPath =
  configDir >>= pure . (flip combine) "config.dhall"
#endif

loadConfig :: IO Config
loadConfig = do
#ifdef DEBUG_LOCAL_CONFIG
  T.putStrLn "Using debug local config" 
  pure debugConfig
#else
  userPath' <- userPath
  T.putStrLn $ "Loading config from file: " <> pack userPath'
  initialiseTestConfig 
  readConfig
#endif