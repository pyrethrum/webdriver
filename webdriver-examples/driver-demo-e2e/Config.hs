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
import Dhall (Generic, FromDhall, ToDhall, input, auto)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (combine, joinPath, splitDirectories, (</>))
import Prelude

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
    wantConsoleLogging :: Bool
  }
  deriving (Eq, Generic, Show)

instance FromDhall Config

instance ToDhall Config

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
configText = T.unlines
        [ "-- Config types"
        , "let Browser = "
        , "      < Chrome"
        , "      | Firefox : "
        , "          { headless : Bool"
        , "          , profilePath : Optional Text "
        , "          }"
        , "      >"
        , ""
        , "let Config = "
        , "      { browser : Browser"
        , "      , wantConsoleLogging : Bool"
        , "      }"
        , ""
        , "-- Config value"
        , "let browser : Browser = "
        , "      Browser.Firefox "
        , "        { headless = False"
        , "        , profilePath = None Text"
        , "        }"
        , ""
        , "let config : Config = "
        , "      { browser = browser"
        , "      , wantConsoleLogging = False"
        , "      }"
        , ""
        , "in config"
        ]

readConfig :: IO Config
readConfig =
  userPath >>= input auto . pack

userPath :: IO FilePath
userPath =
  configDir >>= pure . (flip combine) "config.dhall"

loadConfig :: IO Config
loadConfig = do
  initialiseTestConfig
  readConfig


