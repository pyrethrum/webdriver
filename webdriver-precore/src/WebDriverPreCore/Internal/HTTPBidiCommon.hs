module WebDriverPreCore.Internal.HTTPBidiCommon
  ( URL (..),
    JSUInt (..),
    Session (..),
  )
where

import Data.Aeson as A
  ( FromJSON (..),
    ToJSON,
    Value (..),
    (.:),
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)
import Data.Word (Word64)
import Utils (txt)
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
