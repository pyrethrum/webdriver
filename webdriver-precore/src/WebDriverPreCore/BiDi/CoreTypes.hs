module WebDriverPreCore.BiDi.CoreTypes
  ( BrowsingContext (..),
    ClientWindow (..),
    EmptyResult (..),
    Handle (..),
    InternalId (..),
    JSInt (..),
    JSUInt (..),
    KnownSubscriptionType (..),
    NodeProperties (..),
    NodeRemoteValue (..),
    SharedId (..),
    StringValue (..),
    SubscriptionType (..),
    OffSpecSubscriptionType (..),
    UserContext (..),
    URL (..),
    subscriptionTypeToText,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), withText, (.:), (.:?))
import Data.Aeson.Types (Parser, withObject)
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import AesonUtils (jsonToText)
import WebDriverPreCore.Internal.HTTPBidiCommon (URL (..), JSUInt (..))

newtype ClientWindow = MkClientWindow Text
  deriving newtype (Show, Eq, FromJSON, ToJSON)

-- | Core types for the WebDriver BiDi (Bidirectional) protocol.

-- Base types              -- word 64  18446744073709551615

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
  parseJSON = \case
    String t -> pure $ MkUserContext t
    Object obj ->
      obj .: "userContext" >>= pure . MkUserContext
    v -> fail . unpack $ "Unable to parse UserContext as String or Object, but got:\n  " <> jsonToText v

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
  { id :: Text
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

data KnownSubscriptionType
  = -- BrowsingContext module
    BrowsingContextContextCreated
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
  | -- Log module
    LogEntryAdded
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
  deriving (Show, Eq, Generic, Enum, Bounded)

instance FromJSON KnownSubscriptionType where
  parseJSON :: Value -> Parser KnownSubscriptionType
  parseJSON = withText "KnownSubscriptionType" $ \method -> do
    case method of
      "browsingContext.contextCreated" -> pure BrowsingContextContextCreated
      "browsingContext.contextDestroyed" -> pure BrowsingContextContextDestroyed
      "browsingContext.navigationStarted" -> pure BrowsingContextNavigationStarted
      "browsingContext.fragmentNavigated" -> pure BrowsingContextFragmentNavigated
      "browsingContext.historyUpdated" -> pure BrowsingContextHistoryUpdated
      "browsingContext.domContentLoaded" -> pure BrowsingContextDomContentLoaded
      "browsingContext.load" -> pure BrowsingContextLoad
      "browsingContext.downloadWillBegin" -> pure BrowsingContextDownloadWillBegin
      "browsingContext.downloadEnd" -> pure BrowsingContextDownloadEnd
      "browsingContext.navigationAborted" -> pure BrowsingContextNavigationAborted
      "browsingContext.navigationCommitted" -> pure BrowsingContextNavigationCommitted
      "browsingContext.navigationFailed" -> pure BrowsingContextNavigationFailed
      "browsingContext.userPromptClosed" -> pure BrowsingContextUserPromptClosed
      "browsingContext.userPromptOpened" -> pure BrowsingContextUserPromptOpened
      "log.entryAdded" -> pure LogEntryAdded
      "network.authRequired" -> pure NetworkAuthRequired
      "network.beforeRequestSent" -> pure NetworkBeforeRequestSent
      "network.fetchError" -> pure NetworkFetchError
      "network.responseCompleted" -> pure NetworkResponseCompleted
      "network.responseStarted" -> pure NetworkResponseStarted
      "script.message" -> pure ScriptMessage
      "script.realmCreated" -> pure ScriptRealmCreated
      "script.realmDestroyed" -> pure ScriptRealmDestroyed
      "input.fileDialogOpened" -> pure InputFileDialogOpened
      _ -> fail $ "Unknown KnownSubscriptionType method: " <> show method

data SubscriptionType
  = KnownSubscriptionType KnownSubscriptionType
  | OffSpecSubscriptionType OffSpecSubscriptionType
  deriving (Show, Eq, Generic)

newtype OffSpecSubscriptionType = MkOffSpecSubscriptionType
  { method :: Text
  }
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

instance ToJSON SubscriptionType where
  toJSON :: SubscriptionType -> Value
  toJSON = String . subscriptionTypeToText

subscriptionTypeToText :: SubscriptionType -> Text
subscriptionTypeToText = \case
  KnownSubscriptionType known -> case known of
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
  OffSpecSubscriptionType MkOffSpecSubscriptionType {method} -> method

instance FromJSON SubscriptionType where
  parseJSON :: Value -> Parser SubscriptionType
  parseJSON v =
    (KnownSubscriptionType <$> parseJSON @KnownSubscriptionType v)
      <|> (OffSpecSubscriptionType <$> parseJSON @OffSpecSubscriptionType v)
      <|> (fail . unpack $ "Invalid SubscriptionType: " <> jsonToText v)
