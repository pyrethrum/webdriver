module WebDriverPreCore.Internal.HttpBidiCommon
  ( URL (..),
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
import WebDriverPreCore.Internal.Utils (txt)
import Prelude

newtype URL = MkUrl {url :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON)

-- instance ToJSON URL where
--   toJSON :: URL -> Value
--   toJSON MkUrl {url} = object ["url" .= url]

instance FromJSON URL where
  parseJSON :: Value -> Parser URL
  parseJSON = \case
    String t -> pure $ MkUrl t
    Object o -> do
      url <- o .: "url"
      pure $ MkUrl url
    v -> fail $ unpack $ "Expected URL as String or Object with url property, got: " <> txt v
