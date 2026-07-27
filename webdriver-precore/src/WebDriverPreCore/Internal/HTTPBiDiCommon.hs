module WebDriverPreCore.Internal.HTTPBiDiCommon
  ( URL (..),
    JSUInt (..),
    Session (..),
    BrowserName (..),
    PlatformName (..)
  )
where

import Data.Aeson as A
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    withText,
    (.:),
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)
import Utils (txt, JSUInt (..))
import GHC.Generics (Generic)


newtype Session = MkSession {id :: Text}
  deriving (Generic)
  deriving newtype (Show, Eq, FromJSON, ToJSON)


newtype URL = MkUrl {url :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON)

instance FromJSON URL where
  parseJSON :: Value -> Parser URL
  parseJSON = \case
    String t -> pure $ MkUrl t
    Object o -> do
      url <- o .: "url"
      pure $ MkUrl url
    v -> fail $ unpack $ "Expected URL as String or Object with url property, got: " <> txt v

-- | Browser name with extensibility for unknown browsers
data BrowserName
  = Chrome
  | Firefox
  | Safari
  | Edge
  | InternetExplorer
  | Other Text
  deriving (Show, Eq)

-- | Platform name with extensibility for unknown platforms
data PlatformName
  = Windows
  | Mac
  | Linux
  | Android
  | IOS
  | OtherPlatform Text
  deriving (Show, Eq)

-- | Convert text to BrowserName
textToBrowserName :: Text -> BrowserName
textToBrowserName = \case
  "chrome" -> Chrome
  "firefox" -> Firefox
  "safari" -> Safari
  "edge" -> Edge
  "internet explorer" -> InternetExplorer
  other -> Other other

-- | Convert BrowserName to text
browserNameToText :: BrowserName -> Text
browserNameToText = \case
  Chrome -> "chrome"
  Firefox -> "firefox"
  Safari -> "safari"
  Edge -> "edge"
  InternetExplorer -> "internet explorer"
  Other t -> t

-- | Convert text to PlatformName
textToPlatformName :: Text -> PlatformName
textToPlatformName = \case
  "windows" -> Windows
  "mac" -> Mac
  "linux" -> Linux
  "android" -> Android
  "ios" -> IOS
  other -> OtherPlatform other

-- | Convert PlatformName to text
platformNameToText :: PlatformName -> Text
platformNameToText = \case
  Windows -> "windows"
  Mac -> "mac"
  Linux -> "linux"
  Android -> "android"
  IOS -> "ios"
  OtherPlatform t -> t

-- | ToJSON instance for BrowserName
instance ToJSON BrowserName where
  toJSON :: BrowserName -> Value
  toJSON = String . browserNameToText

-- | FromJSON instance for BrowserName
instance FromJSON BrowserName where
  parseJSON :: Value -> Parser BrowserName
  parseJSON = withText "BrowserName" $ pure . textToBrowserName

-- | ToJSON instance for PlatformName
instance ToJSON PlatformName where
  toJSON :: PlatformName -> Value
  toJSON = String . platformNameToText

-- | FromJSON instance for PlatformName
instance FromJSON PlatformName where
  parseJSON :: Value -> Parser PlatformName
  parseJSON = withText "PlatformName" $ pure . textToPlatformName
