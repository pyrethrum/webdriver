module WebDriverPreCore.BiDi.Script where

import Data.Aeson
import Data.Aeson.Types
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, Handle, InternalId, JSInt, JSUint, NodeRemoteValue)
import Prelude (Bool (..), Double, Either, Eq (..), Maybe, Show, Semigroup (..), ($), (<$>), (<*>), Applicative (..))
import Control.Monad (unless)
import Control.Monad (MonadFail(..))

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

-- Remote Value types
data RemoteValue
  = PrimitiveValue PrimitiveProtocolValue
  | SymbolValue SymbolRemoteValue
  | ArrayValue ArrayRemoteValue
  | ObjectValue ObjectRemoteValue
  | FunctionValue FunctionRemoteValue
  | RegExpValue RegExpRemoteValue
  | DateValue DateRemoteValue
  | MapValue MapRemoteValue
  | SetValue SetRemoteValue
  | WeakMapValue WeakMapRemoteValue
  | GeneratorValue GeneratorRemoteValue
  | ErrorValue ErrorRemoteValue
  | ProxyValue ProxyRemoteValue
  | PromiseValue PromiseRemoteValue
  | TypedArrayValue TypedArrayRemoteValue
  | ArrayBufferValue ArrayBufferRemoteValue
  | NodeListValue NodeListRemoteValue
  | HTMLCollectionValue HTMLCollectionRemoteValue
  | NodeValue NodeRemoteValue
  | WindowProxyValue WindowProxyRemoteValue
  deriving (Show, Generic)

-- | Properties of a WindowProxy remote value
data WindowProxyProperties = WindowProxyProperties
  { context :: Text -- BrowsingContext ID
  }
  deriving (Show, Eq, Generic)

-- | WindowProxy remote value representation
data WindowProxyRemoteValue = WindowProxyRemoteValue
  { typ :: Text, -- "window"
    value :: WindowProxyProperties,
    handle :: Maybe Handle, -- Optional handle
    internalId :: Maybe InternalId -- Optional internal ID
  }
  deriving (Show, Eq, Generic)

-- Custom JSON instances to handle the "type" field naming
instance ToJSON WindowProxyRemoteValue where
  toJSON WindowProxyRemoteValue {..} =
    object
      [ "type" .= typ,
        "value" .= value,
        "handle" .= handle,
        "internalId" .= internalId
      ]
  toEncoding WindowProxyRemoteValue {..} =
    pairs
      ( "type"
          .= typ
            <> "value"
          .= value
            <> "handle"
          .= handle
            <> "internalId"
          .= internalId
      )

instance FromJSON WindowProxyRemoteValue where
  parseJSON = withObject "WindowProxyRemoteValue" $ \v -> do
    typ <- v .: "type"
    unless (typ == "window") $ fail "Type must be 'window'"
    WindowProxyRemoteValue
      <$> pure typ
      <*> v
      .: "value"
      <*> v
      .:? "handle"
      <*> v
      .:? "internalId"

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

-- Remote Object types
data SymbolRemoteValue = SymbolRemoteValue
  { typ :: Text, -- "symbol"
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  }
  deriving (Show, Generic)

instance ToJSON SymbolRemoteValue where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "typ" -> "type"
            x -> x
        }

instance FromJSON SymbolRemoteValue where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "type" -> "typ"
            x -> x
        }

data ArrayRemoteValue = ArrayRemoteValue
  { typ :: Text, -- "array"
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe [RemoteValue]
  }
  deriving (Show, Generic)

instance ToJSON ArrayRemoteValue where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "typ" -> "type"
            x -> x
        }

instance FromJSON ArrayRemoteValue where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "type" -> "typ"
            x -> x
        }

data ObjectRemoteValue = MkObjectRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe [(Either RemoteValue Text, RemoteValue)]
  } deriving (Show, Generic)

data FunctionRemoteValue = MkFunctionRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data RegExpRemoteValue = MkRegExpRemoteValue
  { handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    pattern' :: Text,
    flags :: Maybe Text
  } deriving (Show, Generic)
data DateRemoteValue = MkDateRemoteValue
  { handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Text
  } deriving (Show, Generic)

data MapRemoteValue = MkMapRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe [(Either RemoteValue Text, RemoteValue)]
  } deriving (Show, Generic)

data SetRemoteValue = MkSetRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe [RemoteValue]
  } deriving (Show, Generic)

data WeakMapRemoteValue = MkWeakMapRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data WeakSetRemoteValue = MkWeakSetRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  }

data GeneratorRemoteValue = MkGeneratorRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data ErrorRemoteValue = MkErrorRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data ProxyRemoteValue = MkProxyRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data PromiseRemoteValue = MkPromiseRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data TypedArrayRemoteValue = MkTypedArrayRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data ArrayBufferRemoteValue = MkArrayBufferRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId
  } deriving (Show, Generic)

data NodeListRemoteValue = MkNodeListRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe [RemoteValue]
  } deriving (Show, Generic)

data HTMLCollectionRemoteValue = MkHTMLCollectionRemoteValue
  { typ :: Text,
    handle :: Maybe Handle,
    internalId :: Maybe InternalId,
    value :: Maybe [RemoteValue]
  } deriving (Show, Generic)

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

data StackFrame = StackFrame
  { columnNumber :: JSUint,
    functionName :: Text,
    lineNumber :: JSUint,
    url :: Text
  }
  deriving (Show, Generic)

data MessageParameters = MessageParameters
  { channel :: Channel,
    data_ :: RemoteValue,
    source :: Source
  }
  deriving (Show, Generic)

instance ToJSON MessageParameters where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "data_" -> "data"
            x -> x
        }

instance FromJSON MessageParameters where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "data" -> "data_"
            x -> x
        }

data Source = Source
  { realm :: Realm,
    context :: Maybe BrowsingContext
  }
  deriving (Show, Generic)

data RealmDestroyedParams = RealmDestroyedParams
  { realm :: Realm
  }
  deriving (Show, Generic)

-- Configuration types
data ResultOwnership = Root | None deriving (Show, Generic)

data SerializationOptions = SerializationOptions
  { maxDomDepth :: Maybe (Maybe JSUint), -- .default 0
    maxObjectDepth :: Maybe (Maybe JSUint), -- .default null
    includeShadowTree :: Maybe Text -- "none", "open", "all" .default "none"
  }
  deriving (Show, Generic)