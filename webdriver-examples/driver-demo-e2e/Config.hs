module Config (
  Config (..),
  loadConfig
) where

import Data.Text
import GHC.Generics (Generic)
import Prelude

loadConfig :: IO Config
loadConfig =
  pure $
    MkConfig
      { useFirefox = True,
        firefoxHeadless = True,
        customFirefoxProfilePath = Nothing,
        wantConsoleLogging = False
      }

data Config = MkConfig
  { useFirefox :: Bool,
    firefoxHeadless :: Bool,
    customFirefoxProfilePath :: Maybe Text,
    wantConsoleLogging :: Bool
  }
  deriving (Eq, Generic, Show)
