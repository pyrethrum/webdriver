module WebDriverPreCore.BiDi.Script where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, Handle, InternalId, JSUint, NodeRemoteValue)
import Prelude (Bool (..), Double, Either, Eq (..), Maybe, Show)

-- Main Script types
data ScriptResult
  = AddPreloadScriptResult {script :: PreloadScript}
  | EvaluateResult EvaluateResult
  | GetRealmsResult {realms :: [RealmInfo]}
  deriving (Show, Generic)

data ScriptEvent
  = MessageEvent
      { method :: Text, -- "script.message"
        params :: MessageParameters
      }
  | RealmCreatedEvent RealmInfo
  | RealmDestroyedEvent RealmDestroyedParams
  deriving (Show, Generic)

-- Channel types
newtype Channel = Channel Text deriving newtype (Show, ToJSON, FromJSON)

data ChannelValue = ChannelValue
  { typ :: Text, -- "channel"
    value :: ChannelProperties
  }
  deriving (Show, Generic)

instance ToJSON ChannelValue where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "typ" -> "type"
            x -> x
        }

instance FromJSON ChannelValue where
  parseJSON :: Value -> Parser ChannelValue
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "type" -> "typ"
            x -> x
        }

data ChannelProperties = ChannelProperties
  { channel :: Channel,
    serializationOptions :: Maybe SerializationOptions,
    ownership :: Maybe ResultOwnership
  }
  deriving (Show, Generic)

instance ToJSON ChannelProperties

instance FromJSON ChannelProperties where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "channel" -> "channel"
            x -> x
        }

-- Evaluation types
data EvaluateResult
  = EvaluateResultSuccess
      { typ :: Text, -- "success"
        result :: RemoteValue,
        realm :: Realm
      }
  | EvaluateResultException
      { typ :: Text, -- "exception"
        exceptionDetails :: ExceptionDetails,
        realm :: Realm
      }
  deriving (Show, Generic)

instance ToJSON EvaluateResult where
  toJSON :: EvaluateResult -> Value
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "typ" -> "type"
            x -> x
        }

instance FromJSON EvaluateResult where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "type" -> "typ"
            x -> x
        }

data ExceptionDetails = ExceptionDetails
  { columnNumber :: JSUint,
    exception :: RemoteValue,
    lineNumber :: JSUint,
    stackTrace :: StackTrace,
    text :: Text
  }
  deriving (Show, Generic)

instance ToJSON ExceptionDetails

instance FromJSON ExceptionDetails

-- Remote Value types
data RemoteValue
  = PrimitiveValue PrimitiveProtocolValue
  | SymbolValue
      { typ :: Text, -- "symbol"
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ArrayValue
      { typ :: Text, -- "array"
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | ObjectValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        -- change to value from / to JSON
        values :: Maybe [(Either RemoteValue Text, RemoteValue)]
      }
  | FunctionValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | RegExpValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        pattern' :: Text,
        flags :: Maybe Text
      }
  | DateValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        dateValue :: Text
      }
  | MapValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        values :: Maybe [(Either RemoteValue Text, RemoteValue)]
      }
  | SetValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | WeakMapValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | GeneratorValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ErrorValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ProxyValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | PromiseValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | TypedArrayValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ArrayBufferValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | NodeListValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | HTMLCollectionValue
      { typ :: Text,
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | NodeValue NodeRemoteValue
  | WindowProxyValue
      { typ :: Text, -- "window"
        winProxyValues :: WindowProxyProperties,
        handle :: Maybe Handle, -- Optional handle
        internalId :: Maybe InternalId -- Optional internal ID
      }
  deriving (Show, Generic)


instance ToJSON RemoteValue

instance FromJSON RemoteValue

-- | Properties of a WindowProxy remote value
newtype WindowProxyProperties = MkWindowProxyProperties
  { context :: Text -- BrowsingContext ID
  }
  deriving (Show, Eq, Generic)

instance ToJSON WindowProxyProperties

instance FromJSON WindowProxyProperties

-- | WindowProxy remote value representation
data PrimitiveProtocolValue
  = UndefinedValue
  | NullValue
  | StringValue Text
  | NumberValue (Either Double SpecialNumber)
  | BooleanValue Bool
  | BigIntValue Text
  deriving (Show, Generic)

instance ToJSON PrimitiveProtocolValue where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON PrimitiveProtocolValue where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data SpecialNumber
  = NaN
  | NegativeZero
  | Infinity
  | NegativeInfinity
  deriving (Show, Generic)

instance FromJSON SpecialNumber where

instance ToJSON SpecialNumber where 

-- Remote Object types

data WeakSetRemoteValue = MkWeakSetRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  }

-- Additional remote value types implemented similarly...

-- Realm types
newtype Realm = MkRealm Text deriving (Show, Generic, ToJSON, FromJSON)

newtype PreloadScript = MkPreloadScript Text deriving (Show, Generic, ToJSON, FromJSON)

data RealmInfo
  = WindowRealmInfo
      { base :: BaseRealmInfo,
        typ :: Text, -- "window"
        context :: BrowsingContext,
        sandbox :: Maybe Text
      }
  | DedicatedWorkerRealmInfo {base :: BaseRealmInfo, typ :: Text, owners :: [Realm]}
  | SharedWorkerRealmInfo {base :: BaseRealmInfo, typ :: Text}
  | ServiceWorkerRealmInfo {base :: BaseRealmInfo, typ :: Text}
  | WorkerRealmInfo {base :: BaseRealmInfo, typ :: Text}
  | PaintWorkletRealmInfo {base :: BaseRealmInfo, typ :: Text}
  | AudioWorkletRealmInfo {base :: BaseRealmInfo, typ :: Text}
  | WorkletRealmInfo {base :: BaseRealmInfo, typ :: Text}
  deriving (Show, Generic)

data BaseRealmInfo = BaseRealmInfo
  { realm :: Realm,
    origin :: Text
  }
  deriving (Show, Generic)

-- Stack trace types
data StackTrace = StackTrace
  { callFrames :: [StackFrame]
  }
  deriving (Show, Generic)

instance ToJSON StackTrace

instance FromJSON StackTrace

data StackFrame = StackFrame
  { columnNumber :: JSUint,
    functionName :: Text,
    lineNumber :: JSUint,
    url :: Text
  }
  deriving (Show, Generic)

instance ToJSON StackFrame

instance FromJSON StackFrame

data MessageParameters = MessageParameters
  { channel :: Channel,
    data_ :: RemoteValue,
    source :: Source
  }
  deriving (Show, Generic)

instance ToJSON MessageParameters where
  toJSON :: MessageParameters -> Value
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "data_" -> "data"
            x -> x
        }

instance FromJSON MessageParameters where
  parseJSON :: Value -> Parser MessageParameters
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "data" -> "data_"
            x -> x
        }

data Source = MkSource
  { realm :: Realm,
    context :: Maybe BrowsingContext
  }
  deriving (Show, Generic)

instance ToJSON Source

instance FromJSON Source

data RealmDestroyedParams = RealmDestroyedParams
  { realm :: Realm
  }
  deriving (Show, Generic)

-- Configuration types
data ResultOwnership = Root | None deriving (Show, Generic)

instance ToJSON ResultOwnership where
  toJSON :: ResultOwnership -> Value
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON ResultOwnership where
  parseJSON :: Value -> Parser ResultOwnership
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data SerializationOptions = SerializationOptions
  { maxDomDepth :: Maybe (Maybe JSUint), -- .default 0
    maxObjectDepth :: Maybe (Maybe JSUint), -- .default null
    includeShadowTree :: Maybe Text -- "none", "open", "all" .default "none"
  }
  deriving (Show, Generic)

instance ToJSON SerializationOptions where
  toJSON :: SerializationOptions -> Value
  toJSON =
    genericToJSON
      defaultOptions {omitNothingFields = True}

instance FromJSON SerializationOptions where
  parseJSON :: Value -> Parser SerializationOptions
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}