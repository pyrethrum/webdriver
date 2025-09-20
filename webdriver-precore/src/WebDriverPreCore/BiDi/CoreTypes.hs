module WebDriverPreCore.BiDi.CoreTypes
  ( BrowsingContext (..),
    EmptyResult (..),
    Handle (..),
    InternalId (..),
    JSInt (..),
    JSUInt (..),
    NodeProperties (..),
    NodeRemoteValue (..),
    SharedId (..),
    StringValue (..),
    UserContext (..),
  )
where

import Control.Monad (unless)
import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser, withObject)
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Prelude

-- | Core types for the WebDriver BiDi (Bidirectional) protocol.

-- Base types              -- word 64  18446744073709551615

newtype JSUInt = MkJSUInt Word64 deriving newtype (Show, Eq, Enum, FromJSON, ToJSON) -- JSUnit ::  0..9007199254740991  -     Word64 :: 18446744073709551615

newtype JSInt = MkJSInt Int64 deriving newtype (Show, Eq, FromJSON, ToJSON) -- JSINt  :: -9007199254740991..9007199254740991 - Int64  -9223372036854775808 to 9223372036854775807

-- Main BrowsingContext types
newtype BrowsingContext = MkBrowsingContext {context :: Text}
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON BrowsingContext where
  parseJSON :: Value -> Parser BrowsingContext
  parseJSON =
    \case
      String s -> pure $ MkBrowsingContext s
      Object obj -> MkBrowsingContext <$> obj .: "context"
      v -> fail $ "Expected BrowsingContext as String or Object, but got: " ++ show v

-- Node type used by BrowsingContext and Script

newtype UserContext = MkUserContext {userContext :: Text}
  deriving (Show, Eq, ToJSON)

instance FromJSON UserContext where
  parseJSON :: Value -> Parser UserContext
  parseJSON = withObject "UserContext" $ \obj -> do
    context <- obj .: "userContext"
    pure $ MkUserContext context

data NodeRemoteValue = MkNodeRemoteValue
  { sharedId :: Maybe SharedId,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe NodeProperties
  }
  deriving (Show, Eq, Generic)

instance FromJSON NodeRemoteValue where
  parseJSON :: Value -> Parser NodeRemoteValue
  parseJSON = withObject "NodeRemoteValue" $ \obj -> do
    sharedId <- obj .:? "sharedId"
    handle <- obj .:? "handle"
    internalId <- obj .:? "internalId"
    value <- obj .:? "value"
    pure $ MkNodeRemoteValue {..}

newtype Handle = MkHandle Text deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype InternalId
  = MkInternalId Text
  deriving newtype (Show, Eq, FromJSON)

data NodeProperties = MkNodeProperties
  { nodeType :: JSUInt,
    childNodeCount :: JSUInt,
    attributes :: Maybe (Map.Map Text Text),
    children :: Maybe [NodeRemoteValue],
    localName :: Maybe Text,
    mode :: Maybe Text, -- "open" or "closed"
    namespaceURI :: Maybe Text,
    nodeValue :: Maybe Text,
    shadowRoot :: Maybe NodeRemoteValue -- null allowed
  }
  deriving (Show, Eq, Generic)

newtype StringValue = MkStringValue {value :: Text}
  deriving (Show, Eq, Generic)

instance ToJSON StringValue where
  toJSON :: StringValue -> Value
  toJSON (MkStringValue val) =
    String val

instance FromJSON StringValue where
  parseJSON :: Value -> Parser StringValue
  parseJSON = withObject "StringValue" $ \obj -> do
    typ <- obj .: "type"
    unless (typ == "string") $
      fail $
        "Expected type 'string' but got: " <> show typ
    MkStringValue <$> obj .: "value"

instance FromJSON NodeProperties

newtype SharedId = MkSharedId
  { id :: Text -- SharedId
  }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype EmptyResult = MkEmptyResult {extensible :: Object} deriving (Show, Eq, Generic)

instance FromJSON EmptyResult where
  parseJSON :: Value -> Parser EmptyResult
  parseJSON = withObject "EmptyResult" $ fmap MkEmptyResult . pure

instance ToJSON EmptyResult where
  toJSON :: EmptyResult -> Value
  toJSON (MkEmptyResult obj) = Object obj
