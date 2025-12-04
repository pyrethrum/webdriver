module Config
  ( Config (..),
    DemoBrowser (..),
    isFirefox,
    loadConfig,
  )
where

import Control.Monad (unless)
import Data.Text as T (Text, pack, unlines)
import Data.Text.IO qualified as T
import Dhall (FromDhall, Generic, ToDhall, auto, input)
import IOUtils (findWebDriverRoot)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (combine, (</>))

-- TODO - GET Dhall LSP Server working or switch to JSON

isFirefox :: DemoBrowser -> Bool
isFirefox = \case
  Firefox {} -> True
  Chrome -> False

data DemoBrowser = Chrome | Firefox {headless :: Bool, profilePath :: Maybe Text}
  deriving (Eq, Show, Generic)

instance FromDhall DemoBrowser

instance ToDhall DemoBrowser

data Config = MkConfig
  { browser :: DemoBrowser,
    httpUrl :: Text,
    httpPort :: Word,
    logging :: Bool,
    pauseMS :: Word
  }
  deriving (Eq, Generic, Show)

instance FromDhall Config

instance ToDhall Config

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
      "      < Chrome",
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
      "        { headless = False",
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

loadConfig :: IO Config
loadConfig =
  initialiseTestConfig >> readConfig
