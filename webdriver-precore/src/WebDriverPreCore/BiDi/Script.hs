module WebDriverPreCore.BiDi.Script
  ( -- * ScriptCommand
    AddPreloadScript (..),
    ContextTarget (..),
    RemoteValue (..),
    PrimitiveProtocolValue (..),
    SpecialNumber (..),
    WindowProxyProperties (..),
    CallFunction (..),
    LocalValue (..),
    ListLocalValue (..),
    ArrayLocalValue (..),
    DateLocalValue (..),
    MappingLocalValue (..),
    MapLocalValue (..),
    ObjectLocalValue (..),
    IncludeShadowTree (..),
    RegExpValue (..),
    RegExpLocalValue (..),
    SetLocalValue (..),
    ResultOwnership (..),
    SerializationOptions (..),
    RealmType (..),
    Disown (..),
    Target (..),
    Realm (..),
    RealmDestroyed (..),
    Sandbox (..),
    Evaluate (..),
    GetRealms (..),
    RemovePreloadScript (..),

    -- * ScriptResult
    AddPreloadScriptResult (..),
    GetRealmsResult (..),
    RealmInfo (..),
    BaseRealmInfo (..),
    EvaluateResult (..),
    ExceptionDetails (..),
    StackTrace (..),
    StackFrame (..),
    ScriptEvent (..),
    Message (..),
    Channel (..),
    Source (..),
    ChannelValue (..),
    ChannelProperties (..),

    -- * PreloadScript
    PreloadScript (..),
    RemoteReference (..),
    SharedReference (..),
    RemoteObjectReference (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), defaultOptions, genericToJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, Parser, omitNothingFields)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Vector qualified as V
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, Handle, InternalId (..), JSUInt, NodeRemoteValue (..), SharedId, StringValue (..), UserContext)
import AesonUtils (jsonToText, opt, parseJSONOmitNothing, toJSONOmitNothing)

-- TODO :: amke all imports exports explict

-- ######### REMOTE #########

-- AddPreloadScript command
data AddPreloadScript = MkAddPreloadScript
  { functionDeclaration :: Text,
    arguments :: Maybe [ChannelValue],
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext],
    sandbox :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- Remote Value types
data RemoteValue
  = PrimitiveValue PrimitiveProtocolValue
  | SymbolValue
      { -- "symbol"
        handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ArrayValue
      { -- "array"
        handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | ObjectValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        -- change to value from / to JSON
        values :: Maybe [(Either RemoteValue Text, RemoteValue)]
      }
  | FunctionValue
      { handle :: Maybe Handle,
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
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        values :: Maybe [(Either RemoteValue Text, RemoteValue)]
      }
  | SetValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | WeakMapValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | WeakSetValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | GeneratorValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ErrorValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ProxyValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | PromiseValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | TypedArrayValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | ArrayBufferValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId
      }
  | NodeListValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | HTMLCollectionValue
      { handle :: Maybe Handle,
        internalId :: Maybe InternalId,
        value :: Maybe [RemoteValue]
      }
  | NodeValue NodeRemoteValue
  | WindowProxyValue
      { -- "window"
        winProxyValue :: WindowProxyProperties,
        handle :: Maybe Handle, -- Optional handle
        internalId :: Maybe InternalId -- Optional internal ID
      }
  deriving (Show, Eq, Generic)

instance FromJSON RemoteValue where
  parseJSON :: Value -> Parser RemoteValue
  parseJSON = withObject "RemoteValue" $ \obj -> do
    typ <- obj .: "type"
    handle <- obj .:? "handle"
    internalId <- obj .:? "internalId"
    case typ of
      "undefined" -> pure $ PrimitiveValue UndefinedValue
      "null" -> pure $ PrimitiveValue NullValue
      "string" -> PrimitiveValue . StringValue . MkStringValue <$> obj .: "value"
      "number" ->
        PrimitiveValue . NumberValue <$> do
          v <- obj .: "value"
          case v of
            Number n -> pure $ Left (realToFrac n)
            String s -> case s of
              "NaN" -> pure $ Right NaN
              "-0" -> pure $ Right NegativeZero
              "Infinity" -> pure $ Right Infinity
              "-Infinity" -> pure $ Right NegativeInfinity
              _ -> fail $ "Unknown SpecialNumber string: " <> unpack s
            _ -> fail $ "Expected number or special number string, got: " <> show v
      "boolean" -> PrimitiveValue . BooleanValue <$> obj .: "value"
      "bigint" -> PrimitiveValue . BigIntValue <$> obj .: "value"
      "node" -> NodeValue <$> parseJSON (Object obj)
      "array" -> do
        value <- obj .:? "value"
        pure $ ArrayValue {..}
      "object" -> do
        values <- obj .:? "values"
        pure $ ObjectValue {..}
      "regexp" -> do
        value <- obj .: "value"
        pattern' <- value .: "pattern"
        flags <- value .:? "flags"
        pure $ RegExpValue {..}
      "date" -> do
        dateValue <- obj .: "value"
        pure $ DateValue {..}
      "map" -> do
        maybeValues <- obj .:? "value"
        values <- traverse (mapM parseMapEntry) maybeValues
        pure $ MapValue {..}
        where
          parseMapEntry :: Value -> Parser (Either RemoteValue Text, RemoteValue)
          parseMapEntry val = case val of
            Array arr -> case V.toList arr of
              [keyVal, valueVal] -> do
                -- try parse Text first, then RemoteValue
                key <- (Right <$> parseJSON keyVal) <|> (Left <$> parseJSON keyVal)
                value <- parseJSON valueVal
                pure (key, value)
              _ -> fail "Map entry must be an array with exactly 2 elements"
            _ -> fail "Map entry must be an array"
      "set" -> do
        value <- obj .:? "value"
        pure $ SetValue {..}
      "window" -> do
        winProxyValue <- obj .: "value"
        pure $ WindowProxyValue {..}
      "nodelist" -> do
        value <- obj .:? "value"
        pure $ NodeListValue {..}
      "htmlcollection" -> do
        value <- obj .:? "value"
        pure $ HTMLCollectionValue {..}
      "function" -> pure $ FunctionValue {..}
      "symbol" -> pure $ SymbolValue {..}
      "weakmap" -> pure $ WeakMapValue {..}
      "weakset" -> pure $ WeakSetValue {..}
      "generator" -> pure $ GeneratorValue {..}
      "error" -> pure $ ErrorValue {..}
      "proxy" -> pure $ ProxyValue {..}
      "promise" -> pure $ PromiseValue {..}
      "typedarray" -> pure $ TypedArrayValue {..}
      "arraybuffer" -> pure $ ArrayBufferValue {..}
      _ -> fail $ "Unknown RemoteValue type: " <> show typ <> "\n" <> (unpack $ jsonToText $ Object obj)

-- | WindowProxy remote value representation
data PrimitiveProtocolValue
  = UndefinedValue
  | NullValue
  | StringValue StringValue
  | NumberValue (Either Double SpecialNumber)
  | BooleanValue Bool
  | BigIntValue Text
  deriving (Show, Eq, Generic)

data SpecialNumber
  = NaN
  | NegativeZero
  | Infinity
  | NegativeInfinity
  deriving (Show, Eq, Generic)

instance FromJSON SpecialNumber

instance ToJSON SpecialNumber where
  toJSON :: SpecialNumber -> Value
  toJSON = \case
    NaN -> "NaN"
    NegativeZero -> "-0"
    Infinity -> "Infinity"
    NegativeInfinity -> "-Infinity"

-- | Properties of a WindowProxy remote value
newtype WindowProxyProperties = MkWindowProxyProperties
  { context :: BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance FromJSON WindowProxyProperties

instance ToJSON WindowProxyProperties

-- CallFunction command
data CallFunction = MkCallFunction
  { functionDeclaration :: Text,
    awaitPromise :: Bool,
    target :: Target,
    arguments :: Maybe [LocalValue],
    resultOwnership :: Maybe ResultOwnership,
    serializationOptions :: Maybe SerializationOptions,
    this :: Maybe LocalValue
  }
  deriving (Show, Eq, Generic)

-- \| Local value representation
data LocalValue
  = RemoteReference RemoteReference
  | PrimitiveLocalValue PrimitiveProtocolValue
  | ChannelValue ChannelValue
  | ArrayLocalValue ArrayLocalValue
  | DateLocalValue DateLocalValue
  | MapLocalValue MapLocalValue
  | ObjectLocalValue ObjectLocalValue
  | RegExpLocalValue RegExpLocalValue
  | SetLocalValue SetLocalValue
  deriving (Show, Eq, Generic)

data RemoteReference = MkRemoteReference
  { sharedreference :: SharedReference,
    remoteObjectReference :: RemoteObjectReference
  }
  deriving (Show, Eq, Generic)

data SharedReference = MkSharedReference
  { sharedId :: SharedId,
    handle :: Maybe Handle,
    extensions :: Maybe (Map.Map Text Value) -- "extensions" field is optional
  }
  deriving (Show, Eq, Generic)

instance FromJSON SharedReference where
  parseJSON :: Value -> Parser SharedReference
  parseJSON = parseJSONOmitNothing

data RemoteObjectReference = MkRemoteObjectReference
  { handle :: Handle,
    shartedId :: Maybe SharedId,
    extensions :: Maybe (Map.Map Text Value) -- "extensions" field is optional
  }
  deriving (Show, Eq, Generic)

-- | List of local values
newtype ListLocalValue = MkListLocalValue [LocalValue]
  deriving (Show, Eq, Generic)

-- | Array local value
data ArrayLocalValue = MkArrayLocalValue
  { typ :: Text, -- "array"
    value :: ListLocalValue
  }
  deriving (Show, Eq, Generic)

-- | Date local value
newtype DateLocalValue = MkDateLocalValue
  { value :: Text
  }
  deriving (Show, Eq, Generic)

-- | Mapping of local values
newtype MappingLocalValue = MkMappingLocalValue [(Either LocalValue Text, LocalValue)]
  deriving (Show, Eq, Generic)

-- | Map local value
data MapLocalValue = MkMapLocalValue
  { value :: MappingLocalValue
  }
  deriving (Show, Eq, Generic)

-- | Object local value
data ObjectLocalValue = MkObjectLocalValue
  { value :: MappingLocalValue
  }
  deriving (Show, Eq, Generic)

-- | RegExp value
data RegExpValue = MkRegExpValue
  { regExpPattern :: Text,
    flags :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | RegExp local value
data RegExpLocalValue = MkRegExpLocalValue
  { value :: RegExpValue
  }
  deriving (Show, Eq, Generic)

-- | Set local value
data SetLocalValue = MkSetLocalValue
  { typ :: Text, -- "set"
    value :: ListLocalValue
  }
  deriving (Show, Eq, Generic)

-- OwnershipNone ~ None - renamed to avoid clash with BrowsingContext None
data ResultOwnership = Root | OwnershipNone deriving (Show, Eq, Generic)

instance FromJSON ResultOwnership where
  parseJSON :: Value -> Parser ResultOwnership
  parseJSON = withObject "ResultOwnership" $ \obj -> do
    typ <- obj .: "type"
    case typ of
      "root" -> pure Root
      "none" -> pure OwnershipNone
      _ -> fail $ "Unknown ResultOwnership type: " <> unpack typ

instance ToJSON ResultOwnership where
  toJSON :: ResultOwnership -> Value
  toJSON = \case
    Root -> "root"
    OwnershipNone -> "none"

-- | RealmType represents the different types of Realm
data RealmType
  = WindowRealm
  | DedicatedWorkerRealm
  | SharedWorkerRealm
  | ServiceWorkerRealm
  | WorkerRealm
  | PaintWorkletRealm
  | AudioWorkletRealm
  | WorkletRealm
  deriving (Show, Eq, Generic)

instance FromJSON RealmType

instance ToJSON RealmType where
  toJSON :: RealmType -> Value
  toJSON = \case
    WindowRealm -> "window"
    DedicatedWorkerRealm -> "dedicated-worker"
    SharedWorkerRealm -> "shared-worker"
    ServiceWorkerRealm -> "service-worker"
    WorkerRealm -> "worker"
    PaintWorkletRealm -> "paint-worklet"
    AudioWorkletRealm -> "audio-worklet"
    WorkletRealm -> "worklet"

data SerializationOptions = MkSerializationOptions
  { maxDomDepth :: Maybe (Maybe JSUInt), -- .default 0
    maxObjectDepth :: Maybe (Maybe JSUInt), -- .default null
    includeShadowTree :: Maybe IncludeShadowTree -- "none", "open", "all" .default "none"
  }
  deriving (Show, Eq, Generic)

instance ToJSON SerializationOptions where
  toJSON :: SerializationOptions -> Value
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data IncludeShadowTree = ShadowTreeNone | Open | All deriving (Show, Eq, Generic)

instance ToJSON IncludeShadowTree where
  toJSON :: IncludeShadowTree -> Value
  toJSON = \case
    ShadowTreeNone -> "none" -- name changed to avoid clash with BrowsingContext None
    Open -> "open"
    All -> "all"

-- Disown command
data Disown = MkDisown
  { handles :: [Handle],
    target :: Target
  }
  deriving (Show, Eq, Generic)

data Target
  = RealmTarget Realm
  | ContextTarget ContextTarget
  deriving (Show, Eq, Generic)

newtype Realm = MkRealm {realm :: Text} deriving (Show, Eq, Generic, FromJSON)

data ContextTarget = MkContextTarget
  { context :: BrowsingContext,
    sandbox :: Maybe Sandbox
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContextTarget

instance ToJSON ContextTarget where
  toJSON :: ContextTarget -> Value
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

newtype Sandbox = MkSandbox Text
  deriving (Show, Eq)
  deriving newtype (ToJSON, FromJSON)

instance ToJSON Realm

-- Simple sum types
instance ToJSON Target where
  toJSON :: Target -> Value
  toJSON = \case
    RealmTarget r -> toJSON r
    ContextTarget ct -> toJSON ct

-- Evaluate command
data Evaluate = MkEvaluate
  { expression :: Text,
    target :: Target,
    awaitPromise :: Bool,
    resultOwnership :: Maybe ResultOwnership,
    serializationOptions :: Maybe SerializationOptions
  }
  deriving (Show, Eq, Generic)

instance ToJSON Evaluate where
  toJSON :: Evaluate -> Value
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- GetRealms command
data GetRealms = MkGetRealms
  { context :: Maybe BrowsingContext,
    realmType :: Maybe RealmType
  }
  deriving (Show, Eq, Generic)

-- RemovePreloadScript command
newtype RemovePreloadScript = MkRemovePreloadScript
  { script :: PreloadScript
  }
  deriving (Show, Eq, Generic)

newtype PreloadScript = MkPreloadScript Text deriving (Show, Generic, Eq)

instance FromJSON PreloadScript

instance ToJSON PreloadScript

-- Target specification

-- ######### Local #########

newtype AddPreloadScriptResult = MkAddPreloadScriptResult
  { script :: PreloadScript
  }
  deriving (Show, Eq, Generic)

newtype GetRealmsResult = MkGetRealmsResult
  { realms :: [RealmInfo]
  }
  deriving (Show, Eq, Generic)

data RealmInfo
  = WindowRealmInfo
      { base :: BaseRealmInfo,
        context :: BrowsingContext,
        sandbox :: Maybe Text
      }
  | DedicatedWorker {base :: BaseRealmInfo, owners :: [Realm]}
  | SharedWorker {base :: BaseRealmInfo}
  | ServiceWorker {base :: BaseRealmInfo}
  | Worker {base :: BaseRealmInfo}
  | PaintWorklet {base :: BaseRealmInfo}
  | AudioWorklet {base :: BaseRealmInfo}
  | Worklet {base :: BaseRealmInfo}
  deriving (Show, Eq, Generic)

instance FromJSON RealmInfo where
  parseJSON :: Value -> Parser RealmInfo
  parseJSON = withObject "RealmInfo" $ \o -> do
    typ <- o .: "type"
    base <- BaseRealmInfo <$> o .: "realm" <*> o .: "origin"
    case typ of
      "window" -> do
        context <- o .: "context"
        sandbox <- o .:? "sandbox"
        pure $ WindowRealmInfo {base, context, sandbox}
      "dedicated-worker" -> do
        owners <- o .: "owners"
        pure $ DedicatedWorker {base, owners}
      "shared-worker" -> pure $ SharedWorker {base}
      "service-worker" -> pure $ ServiceWorker {base}
      "worker" -> pure $ Worker {base}
      "paint-worklet" -> pure $ PaintWorklet {base}
      "audio-worklet" -> pure $ AudioWorklet {base}
      "worklet" -> pure $ Worklet {base}
      _ -> fail $ "Unknown RealmInfo type: " <> unpack typ

data BaseRealmInfo = BaseRealmInfo
  { realm :: Realm,
    origin :: Text
  }
  deriving (Show, Eq, Generic)

data EvaluateResult
  = EvaluateResultSuccess
      { result :: RemoteValue,
        realm :: Realm
      }
  | EvaluateResultException
      { exceptionDetails :: ExceptionDetails,
        realm :: Realm
      }
  deriving (Show, Eq, Generic)

instance FromJSON EvaluateResult where
  parseJSON :: Value -> Parser EvaluateResult
  parseJSON = withObject "EvaluateResult" $ \o -> do
    typ <- o .: "type"
    case typ of
      "success" -> do
        result <- o .: "result"
        realm <- o .: "realm"
        pure $ EvaluateResultSuccess {result, realm}
      "exception" -> do
        exceptionDetails <- o .: "exceptionDetails"
        realm <- o .: "realm"
        pure $ EvaluateResultException {exceptionDetails, realm}
      _ -> fail $ "Unknown EvaluateResult type: " <> unpack typ

data ExceptionDetails = MkExceptionDetails
  { columnNumber :: JSUInt,
    exception :: RemoteValue,
    lineNumber :: JSUInt,
    stackTrace :: StackTrace,
    text :: Text
  }
  deriving (Show, Eq, Generic)

data StackTrace = MkStackTrace
  { callFrames :: [StackFrame]
  }
  deriving (Show, Eq, Generic)

data StackFrame = StackFrame
  { columnNumber :: JSUInt,
    functionName :: Text,
    lineNumber :: JSUInt,
    url :: Text
  }
  deriving (Show, Eq, Generic)

-- ScriptEvent types

data ScriptEvent
  = MessageEvent Message
  | RealmCreatedEvent RealmInfo
  | RealmDestroyed RealmDestroyed
  deriving (Show, Eq, Generic)

instance FromJSON ScriptEvent where
  parseJSON :: Value -> Parser ScriptEvent
  parseJSON = withObject "ScriptEvent" $ \o -> do
    typ <- o .: "method"
    case typ of
      "script.message" -> do
        params <- o .: "params"
        pure $ MessageEvent params
      "script.realmCreated" -> do
        params <- o .: "params"
        realmInfo <- parseJSON params
        pure $ RealmCreatedEvent realmInfo
      "script.realmDestroyed" -> do
        params <- o .: "params"
        realmid <- params .: "realm"
        pure . RealmDestroyed $ MkRealmDestroyed realmid
      _ -> fail $ "Unknown ScriptEvent method: " <> unpack typ

data Message = MkMessage
  { channel :: Channel,
    messageData :: RemoteValue,
    source :: Source
  }
  deriving (Show, Eq, Generic)

instance FromJSON Message where
  parseJSON :: Value -> Parser Message
  parseJSON = withObject "Message" $ \o -> do
    channel <- o .: "channel"
    messageData <- o .: "data"
    source <- o .: "source"
    pure $ MkMessage {channel, messageData, source}

newtype Channel = Channel {channelId :: Text}
  deriving newtype
    ( Show,
      Eq,
      ToJSON,
      FromJSON
    )

data Source = MkSource
  { realm :: Realm,
    context :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance FromJSON Source where
  parseJSON :: Value -> Parser Source
  parseJSON = parseJSONOmitNothing

newtype ChannelValue = MkChannelValue
  { value :: ChannelProperties
  }
  deriving (Show, Eq, Generic)

data ChannelProperties = MkChannelProperties
  { channel :: Channel,
    serializationOptions :: Maybe SerializationOptions,
    ownership :: Maybe ResultOwnership
  }
  deriving (Show, Eq, Generic)

-- ToJSON instances for Script module

-- Simple newtypes
-- ToJSON instances for missing command types
instance ToJSON AddPreloadScript where
  toJSON :: AddPreloadScript -> Value
  toJSON = toJSONOmitNothing

instance ToJSON CallFunction where
  toJSON :: CallFunction -> Value
  toJSON = toJSONOmitNothing

-- GetRealms has a typ field that needs special handling
instance ToJSON GetRealms where
  toJSON :: GetRealms -> Value
  toJSON MkGetRealms {context, realmType} =
    object $
      catMaybes
        [ opt "context" context,
          opt "type" realmType
        ]

-- RealmType uses specific string values as per spec

-- Command types
instance ToJSON Disown

instance ToJSON RemovePreloadScript

instance ToJSON PrimitiveProtocolValue where
  toJSON :: PrimitiveProtocolValue -> Value
  toJSON = \case
    UndefinedValue -> object ["type" .= "undefined"]
    NullValue -> object ["type" .= "null"]
    StringValue str ->
      object
        [ "type" .= "string",
          "value" .= str
        ]
    NumberValue (Left num) ->
      object
        [ "type" .= "number",
          "value" .= num
        ]
    NumberValue (Right special) ->
      object
        [ "type" .= "number",
          "value" .= special
        ]
    BooleanValue bool ->
      object
        [ "type" .= "boolean",
          "value" .= bool
        ]
    BigIntValue str ->
      object
        [ "type" .= "bigint",
          "value" .= str
        ]

-- Local Value types
instance ToJSON LocalValue where
  toJSON :: LocalValue -> Value
  toJSON = \case
    RemoteReference ref -> toJSON ref
    PrimitiveLocalValue prim -> toJSON prim
    ChannelValue channel -> toJSON channel
    ArrayLocalValue arr -> toJSON arr
    DateLocalValue date -> toJSON date
    MapLocalValue mapVal -> toJSON mapVal
    ObjectLocalValue obj -> toJSON obj
    RegExpLocalValue regex -> toJSON regex
    SetLocalValue set -> toJSON set

instance ToJSON RemoteReference

instance ToJSON SharedReference

instance ToJSON RemoteObjectReference

instance ToJSON ListLocalValue

-- Types with typ field that need manual handling
instance ToJSON ArrayLocalValue where
  toJSON (MkArrayLocalValue _ value) =
    object
      [ "type" .= "array",
        "value" .= value
      ]

instance ToJSON DateLocalValue where
  toJSON (MkDateLocalValue value) =
    object
      [ "type" .= "date",
        "value" .= value
      ]

instance ToJSON MappingLocalValue where
  toJSON :: MappingLocalValue -> Value
  toJSON (MkMappingLocalValue pairs) =
    toJSON $ pairToArray <$> pairs
    where
      pairToArray :: (Either LocalValue Text, LocalValue) -> [Value]
      pairToArray (key, value) = [keyToJson key, toJSON value]

      keyToJson :: Either LocalValue Text -> Value
      keyToJson (Left localVal) = toJSON localVal
      keyToJson (Right text) = toJSON text

instance ToJSON MapLocalValue where
  toJSON :: MapLocalValue -> Value
  toJSON (MkMapLocalValue value) =
    object
      [ "type" .= "map",
        "value" .= value
      ]

instance ToJSON ObjectLocalValue where
  toJSON :: ObjectLocalValue -> Value
  toJSON (MkObjectLocalValue value) =
    object
      [ "type" .= "object",
        "value" .= value
      ]

instance ToJSON RegExpValue

instance ToJSON RegExpLocalValue where
  toJSON (MkRegExpLocalValue value) =
    object
      [ "type" .= "regexp",
        "value" .= value
      ]

instance ToJSON SetLocalValue where
  toJSON (MkSetLocalValue _ value) =
    object
      [ "type" .= "set",
        "value" .= value
      ]

instance ToJSON AddPreloadScriptResult

instance ToJSON GetRealmsResult

-- RealmInfo has typ fields that need special handling
instance ToJSON RealmInfo where
  toJSON :: RealmInfo -> Value
  toJSON ri =
    object $ baseObj <> typeAndSpecificProps
    where
      base = ri.base
      baseObj :: [Pair]
      baseObj =
        [ "realm" .= base.realm,
          "origin" .= base.origin
        ]

      typeAndSpecificProps :: [Pair]
      typeAndSpecificProps = case ri of
        WindowRealmInfo {context, sandbox} ->
          [ "type" .= WindowRealm,
            "context" .= context
          ]
            <> catMaybes [opt "sandbox" sandbox]
        ri' -> case ri' of
          DedicatedWorker {owners} ->
            [ "type" .= DedicatedWorkerRealm,
              "owners" .= owners
            ]
          SharedWorker {} ->
            [ "type" .= SharedWorkerRealm
            ]
          ServiceWorker {} ->
            [ "type" .= ServiceWorkerRealm
            ]
          Worker {} ->
            [ "type" .= WorkerRealm
            ]
          PaintWorklet {} ->
            [ "type" .= PaintWorkletRealm
            ]
          AudioWorklet {} ->
            [ "type" .= AudioWorkletRealm
            ]
          Worklet {} ->
            [ "type" .= WorkletRealm
            ]

instance ToJSON BaseRealmInfo

instance ToJSON StackTrace

instance ToJSON StackFrame

instance ToJSON Source

-- ChannelValue has typ field that needs manual handling
instance ToJSON ChannelValue where
  toJSON :: ChannelValue -> Value
  toJSON (MkChannelValue value) =
    object
      [ "type" .= "channel",
        "value" .= value
      ]

instance ToJSON ChannelProperties where
  toJSON :: ChannelProperties -> Value
  toJSON = toJSONOmitNothing

-- FromJSON instances for Script module

-- Basic types

instance FromJSON PrimitiveProtocolValue

-- Complex result types
instance FromJSON AddPreloadScriptResult

instance FromJSON GetRealmsResult

instance FromJSON BaseRealmInfo

instance FromJSON ExceptionDetails

instance FromJSON StackTrace

instance FromJSON StackFrame

newtype RealmDestroyed = MkRealmDestroyed {realm :: Realm} deriving (Show, Eq, Generic)

instance FromJSON RealmDestroyed
