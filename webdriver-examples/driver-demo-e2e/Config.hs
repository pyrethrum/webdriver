module Config
  ( Config (..),
    DemoBrowser (..),
    loadConfig,
  )
where

import Control.Monad (unless)
import Data.Text (Text, pack)
import Dhall (FromDhall, Generic, ToDhall, auto, input, Encoder (embed), inject)
import System.Directory (copyFile, doesFileExist, getCurrentDirectory)
import System.FilePath (combine, joinPath, splitDirectories, (</>))
import Prelude
import qualified Dhall.Pretty as P
import qualified Data.Text.IO as T


data DemoBrowser = Chrome | Firefox | FirefoxHeadless
  deriving (Eq, Show, Generic)

instance FromDhall DemoBrowser
instance ToDhall DemoBrowser

data Config = MkConfig
  { browser :: DemoBrowser,
    customFirefoxProfilePath :: Maybe Text,
    wantConsoleLogging :: Bool
  }
  deriving (Eq, Generic, Show)

instance FromDhall Config

instance ToDhall Config

defaultConfig :: Config
defaultConfig =
  MkConfig
    { browser = Firefox,
      customFirefoxProfilePath = Nothing,
      wantConsoleLogging = False
    }

findWebDriverRoot :: FilePath -> Maybe FilePath
findWebDriverRoot path =
  if rootDir `elem` dirs
    then Just webDriverPath
    else Nothing
  where
    rootDir = "webdriver"
    dirs = splitDirectories path
    webDriverPath = (joinPath $ takeWhile (/= rootDir) dirs) </> rootDir

configDir :: IO FilePath
configDir = do
  currentDir <- getCurrentDirectory
  case findWebDriverRoot currentDir of
    Just root -> pure $ root </> "webdriver-examples" </> "driver-demo-e2e" </> "config"
    Nothing -> error "Could not find webdriver root directory"

userConfigFile :: FilePath
userConfigFile = "config.dhall"

initialiseTestConfig :: IO ()
initialiseTestConfig = do
  userPath' <- userPath
  exists <- doesFileExist userPath'
  unless exists $ do
    let expr = embed (inject @Config) defaultConfig
        doc = pack (show (P.prettyCharacterSet P.ASCII expr))

    

    putStrLn $ "Saving default config to: " <> userPath'
    T.writeFile userPath' doc

    {-
    -}


-- initialiseTestConfig :: IO ()
-- initialiseTestConfig = do
--   userPath' <- userPath
--   exists <- doesFileExist userPath'
--   unless exists $ do
--     cfgDir <- configDir
--     let defaultPath = combine cfgDir defaultConfigFile
--     putStrLn $ "Copying default config: " <> defaultPath <> " => " <> userPath'
--     copyFile defaultPath userPath'

readConfig :: IO Config
readConfig =
  userPath >>= input auto . pack

userPath :: IO FilePath
userPath =
  configDir >>= pure . (flip combine) userConfigFile

loadConfig :: IO Config
loadConfig = do
  initialiseTestConfig
  readConfig




