module Config
  ( Config (..),
    loadConfig,
  )
where

import Control.Monad (unless)
import Data.Text (Text, pack)
import Dhall (FromDhall, Generic, ToDhall, auto, input)
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((</>))
import Prelude

configDir :: FilePath
configDir = "webdriver-examples" </> "driver-demo-e2e" </> "config"

defaultPath :: FilePath
defaultPath = configDir </> "config.default.dhall"

userPath :: FilePath
userPath = configDir </> "config.dhall"

initialiseTestConfig :: IO ()
initialiseTestConfig = do
  exists <- doesFileExist userPath
  unless exists $ do
    putStrLn "No local config detected - copying defaults"
    copyFile defaultPath userPath

loadConfig :: IO Config
loadConfig =
  initialiseTestConfig *> input auto (pack userPath) :: IO Config

data Config = MkConfig
  { useFirefox :: Bool,
    firefoxHeadless :: Bool,
    customFirefoxProfilePath :: Maybe Text,
    wantConsoleLogging :: Bool
  }
  deriving (Eq, Generic, Show)

instance FromDhall Config

instance ToDhall Config
