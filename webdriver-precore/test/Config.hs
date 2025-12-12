module Config
  ( Config (..),
    DemoBrowser (..),
    isFirefox
  )
where

import Data.Text as T (Text)
import Dhall (FromDhall, Generic, ToDhall)

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
