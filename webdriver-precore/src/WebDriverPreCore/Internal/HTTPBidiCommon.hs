module WebDriverPreCore.Internal.HTTPBidiCommon
  ( URL (..),
    JSUInt (..),
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

newtype JSUInt = MkJSUInt Word64 deriving newtype (Show, Eq, Enum, FromJSON, ToJSON) -- JSUnit ::  0..9007199254740991  -     Word64 :: 18446744073709551615

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
