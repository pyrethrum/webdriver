module WebDriverPreCore.BiDi.Script where


import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map
import WebDriverPreCore.BiDi.CoreTypes (JSUint, JSInt)

-- Main Script types
data ScriptResult
  = AddPreloadScriptResult AddPreloadScriptResult
  | EvaluateResult EvaluateResult
  | GetRealmsResult GetRealmsResult
  deriving (Show, Generic, ToJSON, FromJSON)

data ScriptEvent
  = MessageEvent MessageEvent
  | RealmCreatedEvent RealmInfo
  | RealmDestroyedEvent RealmDestroyedParams
  deriving (Show, Generic, ToJSON, FromJSON)

-- Channel types
newtype Channel = Channel Text deriving newtype (Show, Generic, ToJSON, FromJSON)

data ChannelValue = ChannelValue
  { typ :: Text  -- "channel"
  , value :: ChannelProperties
  } deriving (Show, Generic)

instance ToJSON ChannelValue where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON ChannelValue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

data ChannelProperties = ChannelProperties
  { channel :: Channel
  , serializationOptions :: Maybe SerializationOptions
  , ownership :: Maybe ResultOwnership
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Evaluation types
data EvaluateResult
  = EvaluateResultSuccess EvaluateResultSuccess
  | EvaluateResultException EvaluateResultException
  deriving (Show, Generic, ToJSON, FromJSON)

data EvaluateResultSuccess = EvaluateResultSuccess
  { typ :: Text  -- "success"
  , result :: RemoteValue
  , realm :: Realm
  } deriving (Show, Generic)

instance ToJSON EvaluateResultSuccess where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON EvaluateResultSuccess where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

data EvaluateResultException = EvaluateResultException
  { typ :: Text  -- "exception"
  , exceptionDetails :: ExceptionDetails
  , realm :: Realm
  } deriving (Show, Generic)

instance ToJSON EvaluateResultException where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON EvaluateResultException where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

data ExceptionDetails = ExceptionDetails
  { columnNumber :: JSUint
  , exception :: RemoteValue
  , lineNumber :: JSUint
  , stackTrace :: StackTrace
  , text :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

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
  | WeakSetValue WeakSetRemoteValue
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
  deriving (Show, Generic, ToJSON, FromJSON)

data PrimitiveProtocolValue
  = UndefinedValue
  | NullValue
  | StringValue Text
  | NumberValue (Either Double SpecialNumber)
  | BooleanValue Bool
  | BigIntValue Text
  deriving (Show, Generic)

instance ToJSON PrimitiveProtocolValue where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON PrimitiveProtocolValue where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

data SpecialNumber
  = NaN
  | NegativeZero
  | Infinity
  | NegativeInfinity
  deriving (Show, Generic, ToJSON, FromJSON)

-- Remote Object types
data SymbolRemoteValue = SymbolRemoteValue
  { typ :: Text  -- "symbol"
  , handle :: Maybe Handle
  , internalId :: Maybe InternalId
  } deriving (Show, Generic)

instance ToJSON SymbolRemoteValue where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON SymbolRemoteValue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

data ArrayRemoteValue = ArrayRemoteValue
  { typ :: Text  -- "array"
  , handle :: Maybe Handle
  , internalId :: Maybe InternalId
  , value :: Maybe [RemoteValue]
  } deriving (Show, Generic)

instance ToJSON ArrayRemoteValue where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON ArrayRemoteValue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

-- Additional remote value types implemented similarly...
data ObjectRemoteValue = ObjectRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId, value :: Maybe [(Either RemoteValue Text, RemoteValue)] }
data FunctionRemoteValue = FunctionRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data RegExpRemoteValue = RegExpRemoteValue { handle :: Maybe Handle, internalId :: Maybe InternalId, pattern :: Text, flags :: Maybe Text }
data DateRemoteValue = DateRemoteValue { handle :: Maybe Handle, internalId :: Maybe InternalId, value :: Text }
data MapRemoteValue = MapRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId, value :: Maybe [(Either RemoteValue Text, RemoteValue)] }
data SetRemoteValue = SetRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId, value :: Maybe [RemoteValue] }
data WeakMapRemoteValue = WeakMapRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data WeakSetRemoteValue = WeakSetRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data GeneratorRemoteValue = GeneratorRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data ErrorRemoteValue = ErrorRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data ProxyRemoteValue = ProxyRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data PromiseRemoteValue = PromiseRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data TypedArrayRemoteValue = TypedArrayRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data ArrayBufferRemoteValue = ArrayBufferRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId }
data NodeListRemoteValue = NodeListRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId, value :: Maybe [RemoteValue] }
data HTMLCollectionRemoteValue = HTMLCollectionRemoteValue { typ :: Text, handle :: Maybe Handle, internalId :: Maybe InternalId, value :: Maybe [RemoteValue] }


-- Realm types
newtype Realm = Realm Text deriving (Show, Generic, ToJSON, FromJSON)
newtype PreloadScript = PreloadScript Text deriving (Show, Generic, ToJSON, FromJSON)

data RealmInfo
  = WindowRealmInfo WindowRealmInfo
  | DedicatedWorkerRealmInfo DedicatedWorkerRealmInfo
  | SharedWorkerRealmInfo SharedWorkerRealmInfo
  | ServiceWorkerRealmInfo ServiceWorkerRealmInfo
  | WorkerRealmInfo WorkerRealmInfo
  | PaintWorkletRealmInfo PaintWorkletRealmInfo
  | AudioWorkletRealmInfo AudioWorkletRealmInfo
  | WorkletRealmInfo WorkletRealmInfo
  deriving (Show, Generic, ToJSON, FromJSON)

data BaseRealmInfo = BaseRealmInfo
  { realm :: Realm
  , origin :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

data WindowRealmInfo = WindowRealmInfo
  { base :: BaseRealmInfo
  , typ :: Text  -- "window"
  , context :: BrowsingContext
  , sandbox :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON WindowRealmInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON WindowRealmInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

-- Other realm types implemented similarly...
data DedicatedWorkerRealmInfo = DedicatedWorkerRealmInfo { base :: BaseRealmInfo, typ :: Text, owners :: [Realm] }
data SharedWorkerRealmInfo = SharedWorkerRealmInfo { base :: BaseRealmInfo, typ :: Text }
data ServiceWorkerRealmInfo = ServiceWorkerRealmInfo { base :: BaseRealmInfo, typ :: Text }
data WorkerRealmInfo = WorkerRealmInfo { base :: BaseRealmInfo, typ :: Text }
data PaintWorkletRealmInfo = PaintWorkletRealmInfo { base :: BaseRealmInfo, typ :: Text }
data AudioWorkletRealmInfo = AudioWorkletRealmInfo { base :: BaseRealmInfo, typ :: Text }
data WorkletRealmInfo = WorkletRealmInfo { base :: BaseRealmInfo, typ :: Text }

-- Stack trace types
data StackTrace = StackTrace
  { callFrames :: [StackFrame]
  } deriving (Show, Generic, ToJSON, FromJSON)

data StackFrame = StackFrame
  { columnNumber :: JSUint
  , functionName :: Text
  , lineNumber :: JSUint
  , url :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Result types
data AddPreloadScriptResult = AddPreloadScriptResult
  { script :: PreloadScript
  } deriving (Show, Generic, ToJSON, FromJSON)

data GetRealmsResult = GetRealmsResult
  { realms :: [RealmInfo]
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Event types
data MessageEvent = MessageEvent
  { method :: Text  -- "script.message"
  , params :: MessageParameters
  } deriving (Show, Generic, ToJSON, FromJSON)

data MessageParameters = MessageParameters
  { channel :: Channel
  , data_ :: RemoteValue
  , source :: Source
  } deriving (Show, Generic)

instance ToJSON MessageParameters where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "data_" -> "data"
    x -> x }

instance FromJSON MessageParameters where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "data" -> "data_"
    x -> x }

data Source = Source
  { realm :: Realm
  , context :: Maybe BrowsingContext
  } deriving (Show, Generic, ToJSON, FromJSON)

data RealmDestroyedParams = RealmDestroyedParams
  { realm :: Realm
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Configuration types
data ResultOwnership = Root | None deriving (Show, Generic, ToJSON, FromJSON)

data SerializationOptions = SerializationOptions
  { maxDomDepth :: Maybe (Maybe JSUint)  -- .default 0
  , maxObjectDepth :: Maybe (Maybe JSUint)  -- .default null
  , includeShadowTree :: Maybe Text  -- "none", "open", "all" .default "none"
  } deriving (Show, Generic, ToJSON, FromJSON)