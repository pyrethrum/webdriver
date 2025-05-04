module WebDriverPreCore.BiDi.CoreTypes
  ( BrowsingContext (..),
    Handle (..),
    InternalId (..),
    JSInt (..),
    JSUint (..),
    NodeProperties (..),
    NodeRemoteValue (..),
    SharedId (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON)
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Prelude (Eq, Maybe, Show)

-- | Core types for the WebDriver BiDi (Bidirectional) protocol.

-- Base types              -- word 64  18446744073709551615

newtype JSUint = MkJSUInt Word64 deriving newtype (Show, Eq, FromJSON, ToJSON) -- JSUnit ::  0..9007199254740991  -     Word64 :: 18446744073709551615

newtype JSInt = MkJSInt Int64 deriving newtype (Show, Eq, FromJSON, ToJSON) -- JSINt  :: -9007199254740991..9007199254740991 - Int64  -9223372036854775808 to 9223372036854775807

-- Main BrowsingContext types
newtype BrowsingContext = BrowsingContext Text deriving (Show, Generic, ToJSON, FromJSON)

-- Node type used by BrowsingContext and Script

data NodeRemoteValue = MkNodeRemoteValue
  { typ :: Text, -- "node"
    sharedId :: Maybe SharedId,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe NodeProperties
  }
  deriving (Show, Generic)

instance FromJSON NodeRemoteValue

instance ToJSON NodeRemoteValue

data NodeProperties = MkNodeProperties
  { nodeType :: JSUint,
    childNodeCount :: JSUint,
    attributes :: Maybe (Map.Map Text Text),
    children :: Maybe [NodeRemoteValue],
    localName :: Maybe Text,
    mode :: Maybe Text, -- "open" or "closed"
    namespaceURI :: Maybe Text,
    nodeValue :: Maybe Text,
    shadowRoot :: Maybe NodeRemoteValue -- null allowed
  }
  deriving (Show, Generic)

instance ToJSON NodeProperties

instance FromJSON NodeProperties

newtype Handle = MkHandle Text deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype InternalId = MkInternalId Text deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype SharedId = MkSharedId Text deriving newtype (Show, Eq, ToJSON, FromJSON)