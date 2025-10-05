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
    SubscriptionType (..),
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

-- work backwards from use case
-- for all return types have the option of bringing back whole value
-- TypeOnly, Value Only, TypeAndValue
-- return type should not need empty return types:
-- TODO: need 3 newtypes EmptyParams, EmptyResult, and Extensible ~ update API accordingly
-- TODO: revisit and demos for setExtended
-- TODO: getExtended - maybe just return value as well

newtype EmptyResult = MkEmptyResult {extensible :: Object} deriving (Show, Eq, Generic)

instance FromJSON EmptyResult where
  parseJSON :: Value -> Parser EmptyResult
  parseJSON = withObject "EmptyResult" $ fmap MkEmptyResult . pure

instance ToJSON EmptyResult where
  toJSON :: EmptyResult -> Value
  toJSON (MkEmptyResult obj) = Object obj

data SubscriptionType
  = -- Log module
    LogEntryAdded
  | BrowsingContextContextCreated
  | BrowsingContextContextDestroyed
  | BrowsingContextNavigationStarted
  | BrowsingContextFragmentNavigated
  | BrowsingContextHistoryUpdated
  | BrowsingContextDomContentLoaded
  | BrowsingContextLoad
  | BrowsingContextDownloadWillBegin
  | BrowsingContextDownloadEnd
  | BrowsingContextNavigationAborted
  | BrowsingContextNavigationCommitted
  | BrowsingContextNavigationFailed
  | BrowsingContextUserPromptClosed
  | BrowsingContextUserPromptOpened
  | -- Network module
    NetworkAuthRequired
  | NetworkBeforeRequestSent
  | NetworkFetchError
  | NetworkResponseCompleted
  | NetworkResponseStarted
  | -- Script module
    ScriptMessage
  | ScriptRealmCreated
  | ScriptRealmDestroyed
  | -- Input module
    InputFileDialogOpened
  deriving (Show, Eq, Generic)

instance ToJSON SubscriptionType where
  toJSON :: SubscriptionType -> Value
  toJSON = \case
    -- Log module
    LogEntryAdded -> "log.entryAdded"
    -- BrowsingContext module
    BrowsingContextContextCreated -> "browsingContext.contextCreated"
    BrowsingContextContextDestroyed -> "browsingContext.contextDestroyed"
    BrowsingContextNavigationStarted -> "browsingContext.navigationStarted"
    BrowsingContextFragmentNavigated -> "browsingContext.fragmentNavigated"
    BrowsingContextHistoryUpdated -> "browsingContext.historyUpdated"
    BrowsingContextDomContentLoaded -> "browsingContext.domContentLoaded"
    BrowsingContextLoad -> "browsingContext.load"
    BrowsingContextDownloadWillBegin -> "browsingContext.downloadWillBegin"
    BrowsingContextDownloadEnd -> "browsingContext.downloadEnd"
    BrowsingContextNavigationAborted -> "browsingContext.navigationAborted"
    BrowsingContextNavigationCommitted -> "browsingContext.navigationCommitted"
    BrowsingContextNavigationFailed -> "browsingContext.navigationFailed"
    BrowsingContextUserPromptClosed -> "browsingContext.userPromptClosed"
    BrowsingContextUserPromptOpened -> "browsingContext.userPromptOpened"
    -- Network module
    NetworkAuthRequired -> "network.authRequired"
    NetworkBeforeRequestSent -> "network.beforeRequestSent"
    NetworkFetchError -> "network.fetchError"
    NetworkResponseCompleted -> "network.responseCompleted"
    NetworkResponseStarted -> "network.responseStarted"
    -- Script module
    ScriptMessage -> "script.message"
    ScriptRealmCreated -> "script.realmCreated"
    ScriptRealmDestroyed -> "script.realmDestroyed"
    -- Input module
    InputFileDialogOpened -> "input.fileDialogOpened"

instance FromJSON SubscriptionType where
  parseJSON :: Value -> Parser SubscriptionType
  parseJSON = \case
    -- Log module
    String "log.entryAdded" -> pure LogEntryAdded
    -- BrowsingContext module
    String "browsingContext.contextCreated" -> pure BrowsingContextContextCreated
    String "browsingContext.contextDestroyed" -> pure BrowsingContextContextDestroyed
    String "browsingContext.navigationStarted" -> pure BrowsingContextNavigationStarted
    String "browsingContext.fragmentNavigated" -> pure BrowsingContextFragmentNavigated
    String "browsingContext.historyUpdated" -> pure BrowsingContextHistoryUpdated
    String "browsingContext.domContentLoaded" -> pure BrowsingContextDomContentLoaded
    String "browsingContext.load" -> pure BrowsingContextLoad
    String "browsingContext.downloadWillBegin" -> pure BrowsingContextDownloadWillBegin
    String "browsingContext.downloadEnd" -> pure BrowsingContextDownloadEnd
    String "browsingContext.navigationAborted" -> pure BrowsingContextNavigationAborted
    String "browsingContext.navigationCommitted" -> pure BrowsingContextNavigationCommitted
    String "browsingContext.navigationFailed" -> pure BrowsingContextNavigationFailed
    String "browsingContext.userPromptClosed" -> pure BrowsingContextUserPromptClosed
    String "browsingContext.userPromptOpened" -> pure BrowsingContextUserPromptOpened
    -- Network module
    String "network.authRequired" -> pure NetworkAuthRequired
    String "network.beforeRequestSent" -> pure NetworkBeforeRequestSent
    String "network.fetchError" -> pure NetworkFetchError
    String "network.responseCompleted" -> pure NetworkResponseCompleted
    String "network.responseStarted" -> pure NetworkResponseStarted
    -- Script module
    String "script.message" -> pure ScriptMessage
    String "script.realmCreated" -> pure ScriptRealmCreated
    String "script.realmDestroyed" -> pure ScriptRealmDestroyed
    -- Input module
    String "input.fileDialogOpened" -> pure InputFileDialogOpened
    _ -> fail "Invalid SubscriptionType"
