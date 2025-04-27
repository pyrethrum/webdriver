module WebDriverPreCore.BiDi.CoreTypes (
  JSUint(..),
  JSInt(..),
  NodeRemoteValue(..),
  NodeProperties(..),
  Handle(..),
  InternalId(..),
  SharedId(..)
) where
import Data.Word (Word64)
import Prelude (Show, Eq, Maybe)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), ToJSON, Options (fieldLabelModifier))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Aeson.Types (ToJSON(..))

-- | Core types for the WebDriver BiDi (Bidirectional) protocol.


-- Base types              -- word 64  18446744073709551615 

newtype JSUint = MkJSUInt Word64 deriving  newtype (Show, Eq, FromJSON, ToJSON) -- JSUnit ::  0..9007199254740991  -     Word64 :: 18446744073709551615 
newtype JSInt = MkJSInt Int64 deriving newtype (Show, Eq,  FromJSON, ToJSON)    -- JSINt  :: -9007199254740991..9007199254740991 - Int64  -9223372036854775808 to 9223372036854775807

-- Node type used by BrowsingContext and Script
data NodeRemoteValue = MkNodeRemoteValue
  { typ :: Text  -- "node"
  , sharedId :: Maybe SharedId
  , handle :: Maybe Handle
  , internalId :: Maybe InternalId
  , value :: Maybe NodeProperties
  } deriving (Show, Generic)

instance ToJSON NodeRemoteValue where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON NodeRemoteValue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

data NodeProperties = MkNodeProperties
  { nodeType :: JSUint
  , childNodeCount :: JSUint
  , attributes :: Maybe (Map.Map Text Text)
  , children :: Maybe [NodeRemoteValue]
  , localName :: Maybe Text
  , mode :: Maybe Text  -- "open" or "closed"
  , namespaceURI :: Maybe Text
  , nodeValue :: Maybe Text
  , shadowRoot :: Maybe NodeRemoteValue  -- null allowed
  } deriving (Show, Generic, ToJSON, FromJSON)


newtype Handle = MkHandle Text deriving newtype (Show,  ToJSON, FromJSON)
newtype InternalId = MkInternalId Text deriving newtype (Show, ToJSON, FromJSON)
newtype SharedId = MkSharedId Text deriving newtype (Show, ToJSON, FromJSON)