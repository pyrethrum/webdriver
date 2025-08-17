module WebDriverPreCore.BiDi.Script
  ( -- * ScriptCommand
    AddPreloadScript (..),
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
    RegExpValue (..),
    RegExpLocalValue (..),
    SetLocalValue (..),
    ResultOwnership (..),
    SerializationOptions (..),
    RealmType (..),
    Disown (..),
    Target (..),
    Realm (..),
    Evaluate (..),
    GetRealms (..),
    RemovePreloadScript (..),

    -- * ScriptResult
    AddPreloadScriptResult (..),
    CallFunctionResult (..),
    GetRealmsResult (..),
    ScriptResult (..),
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
    SharedId (..),
    ScriptCommand (..),
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Object, Pair)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes
  ( BrowsingContext,
    EmptyResult,
    Handle,
    InternalId (..),
    JSUInt,
    NodeRemoteValue (..),
  )
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, opt)
import Prelude (Bool (..), Double, Either (..), Eq (..), Maybe (..), Semigroup (..), Show, undefined, ($))

-- ######### REMOTE #########

-- Script Command types
data ScriptCommand
  = AddPreloadScript AddPreloadScript
  | CallFunction CallFunction
  | Disown Disown
  | Evaluate Evaluate
  | GetRealms GetRealms
  | RemovePreloadScript RemovePreloadScript
  deriving (Show, Eq, Generic)

-- AddPreloadScript command
data AddPreloadScript = MkAddPreloadScript
  { functionDeclaration :: Text,
    arguments :: Maybe [RemoteValue],
    contexts :: Maybe [BrowsingContext],
    sandbox :: Maybe Text
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

-- | WindowProxy remote value representation
data PrimitiveProtocolValue
  = UndefinedValue
  | NullValue
  | StringValue Text
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

-- | Properties of a WindowProxy remote value
newtype WindowProxyProperties = MkWindowProxyProperties
  { contextId :: Text -- BrowsingContext ID
  }
  deriving (Show, Eq, Generic)

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

data RemoteObjectReference = MkRemoteObjectReference
  { handle :: Handle,
    shartedId :: Maybe SharedId,
    extensions :: Maybe (Map.Map Text Value) -- "extensions" field is optional
  }
  deriving (Show, Eq, Generic)

newtype SharedId = MkShareId
  { id :: Text -- SharedId
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
  { typ :: Text, -- "map"
    value :: MappingLocalValue
  }
  deriving (Show, Eq, Generic)

-- | Object local value
data ObjectLocalValue = MkObjectLocalValue
  { typ :: Text, -- "object"
    value :: MappingLocalValue
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
  { typ :: Text, -- "regexp"
    value :: RegExpValue
  }
  deriving (Show, Eq, Generic)

-- | Set local value
data SetLocalValue = MkSetLocalValue
  { typ :: Text, -- "set"
    value :: ListLocalValue
  }
  deriving (Show, Eq, Generic)

data ResultOwnership = Root | None deriving (Show, Eq, Generic)

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

data SerializationOptions = SerializationOptions
  { maxDomDepth :: Maybe (Maybe JSUInt), -- .default 0
    maxObjectDepth :: Maybe (Maybe JSUInt), -- .default null
    includeShadowTree :: Maybe Text -- "none", "open", "all" .default "none"
  }
  deriving (Show, Eq, Generic)

-- Disown command
data Disown = MkDisown
  { handles :: [Handle],
    target :: Target
  }
  deriving (Show, Eq, Generic)

data Target
  = RealmTarget Realm
  | ContextTarget BrowsingContext
  deriving (Show, Eq, Generic)

newtype Realm = MkRealm Text deriving (Show, Eq, Generic)

-- Evaluate command
data Evaluate = MkEvaluate
  { expression :: Text,
    target :: Target,
    awaitPromise :: Bool,
    resultOwnership :: Maybe ResultOwnership,
    serializationOptions :: Maybe SerializationOptions
  }
  deriving (Show, Eq, Generic)

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

-- Target specification

-- ######### Local #########

data ScriptResult
  = AddPreloadScriptResult AddPreloadScriptResult
  | CallFunctionResult CallFunctionResult
  | DisownResult EmptyResult
  | EvaluateResult EvaluateResult
  | GetRealmsResult GetRealmsResult
  | RemovePreloadScriptResult EmptyResult
  deriving (Show, Eq, Generic)

newtype AddPreloadScriptResult = MkAddPreloadScriptResult
  { script :: PreloadScript
  }
  deriving (Show, Eq, Generic)

data CallFunctionResult = MkCallFunctionResult
  { result :: RemoteValue,
    realm :: Realm,
    stackTrace :: Maybe StackTrace
  }
  deriving (Show, Eq, Generic)

newtype GetRealmsResult = MkGetRealmsResult
  { realms :: [RealmInfo]
  }
  deriving (Show, Eq, Generic)

data RealmInfo
  = Window
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

data BaseRealmInfo = BaseRealmInfo
  { realm :: Realm,
    origin :: Text
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

data ExceptionDetails = ExceptionDetails
  { columnNumber :: JSUInt,
    exception :: RemoteValue,
    lineNumber :: JSUInt,
    stackTrace :: StackTrace,
    text :: Text
  }
  deriving (Show, Eq, Generic)

data StackTrace = StackTrace
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
  = MessageEvent
      { method :: Text, -- "script.message"
        params :: Message
      }
  | RealmCreatedEvent RealmInfo
  | RealmDestroyed Realm
  deriving (Show, Generic)

data Message = MkMessage
  { channel :: Channel,
    messageData :: RemoteValue,
    source :: Source
  }
  deriving (Show, Eq, Generic)

-- Channel types
newtype Channel = Channel Text deriving newtype (Show, Eq, ToJSON)

data Source = MkSource
  { realm :: Realm,
    context :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

data ChannelValue = MkChannelValue
  { typ :: Text, -- "channel"
    value :: ChannelProperties
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
instance ToJSON AddPreloadScript

instance ToJSON CallFunction

instance ToJSON Evaluate

-- GetRealms has a typ field that needs special handling
instance ToJSON GetRealms where
  toJSON :: GetRealms -> Value
  toJSON MkGetRealms {context, realmType} =
    object $
      catMaybes
        [ opt "context" context,
          opt "type" realmType
        ]

instance ToJSON PreloadScript

instance ToJSON Realm

-- RealmType uses specific string values as per spec
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

-- Simple sum types
instance ToJSON Target where
  toJSON :: Target -> Value
  toJSON = enumCamelCase

-- Command types
instance ToJSON Disown

instance ToJSON RemovePreloadScript

-- ScriptCommand - enable when all dependencies are ready
instance ToJSON ScriptCommand where
  toJSON = enumCamelCase

-- Remote Value types - simplified to avoid orphan instances
instance ToJSON RemoteValue where
  toJSON :: RemoteValue -> Value
  toJSON = \case
    PrimitiveValue prim -> toJSON prim
    SymbolValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("symbol" :: Text)),
            opt "handle" handle
          ]
    ArrayValue {handle, value} ->
      object $
        catMaybes
          [ Just ("type" .= ("array" :: Text)),
            opt "handle" handle,
            opt "value" value
          ]
    ObjectValue {handle, values} ->
      object $
        catMaybes
          [ Just ("type" .= ("object" :: Text)),
            opt "handle" handle,
            opt "value" values
          ]
    FunctionValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("function" :: Text)),
            opt "handle" handle
          ]
    RegExpValue {handle, pattern', flags} ->
      object $
        catMaybes
          [ Just ("type" .= ("regexp" :: Text)),
            opt "handle" handle,
            Just
              ( "value"
                  .= object
                    [ "pattern" .= pattern',
                      "flags" .= flags
                    ]
              )
          ]
    DateValue {handle, dateValue} ->
      object $
        catMaybes
          [ Just ("type" .= ("date" :: Text)),
            opt "handle" handle,
            Just ("value" .= dateValue)
          ]
    MapValue {handle, values} ->
      object $
        catMaybes
          [ Just ("type" .= ("map" :: Text)),
            opt "handle" handle,
            opt "value" values
          ]
    SetValue {handle, value} ->
      object $
        catMaybes
          [ Just ("type" .= ("set" :: Text)),
            opt "handle" handle,
            opt "value" value
          ]
    WeakMapValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("weakmap" :: Text)),
            opt "handle" handle
          ]
    GeneratorValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("generator" :: Text)),
            opt "handle" handle
          ]
    ErrorValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("error" :: Text)),
            opt "handle" handle
          ]
    ProxyValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("proxy" :: Text)),
            opt "handle" handle
          ]
    PromiseValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("promise" :: Text)),
            opt "handle" handle
          ]
    TypedArrayValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("typedarray" :: Text)),
            opt "handle" handle
          ]
    ArrayBufferValue {handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("arraybuffer" :: Text)),
            opt "handle" handle
          ]
    NodeListValue {handle, value} ->
      object $
        catMaybes
          [ Just ("type" .= ("nodelist" :: Text)),
            opt "handle" handle,
            opt "value" value
          ]
    HTMLCollectionValue {handle, value} ->
      object $
        catMaybes
          [ Just ("type" .= ("htmlcollection" :: Text)),
            opt "handle" handle,
            opt "value" value
          ]
    NodeValue _ -> object ["type" .= ("node" :: Text)] -- Skip complex NodeRemoteValue for now
    WindowProxyValue {winProxyValues, handle} ->
      object $
        catMaybes
          [ Just ("type" .= ("window" :: Text)),
            Just ("value" .= winProxyValues),
            opt "handle" handle
          ]

instance ToJSON PrimitiveProtocolValue where
  toJSON = \case
    UndefinedValue -> object ["type" .= ("undefined" :: Text)]
    NullValue -> object ["type" .= ("null" :: Text)]
    StringValue str -> object ["type" .= ("string" :: Text), "value" .= str]
    NumberValue (Left num) -> object ["type" .= ("number" :: Text), "value" .= num]
    NumberValue (Right special) -> object ["type" .= ("number" :: Text), "value" .= special]
    BooleanValue bool -> object ["type" .= ("boolean" :: Text), "value" .= bool]
    BigIntValue str -> object ["type" .= ("bigint" :: Text), "value" .= str]

instance ToJSON SpecialNumber where
  toJSON = \case
    NaN -> "NaN"
    NegativeZero -> "-0"
    Infinity -> "Infinity"
    NegativeInfinity -> "-Infinity"

instance ToJSON WindowProxyProperties

instance ToJSON ResultOwnership where
  toJSON = enumCamelCase

instance ToJSON SerializationOptions

-- Local Value types
instance ToJSON LocalValue where
  toJSON = \case
    RemoteReference ref -> toJSON ref
    PrimitiveLocalValue prim -> toJSON prim
    ChannelValue channel -> toJSON channel
    ArrayLocalValue arr -> toJSON arr
    DateLocalValue date -> toJSON date
    MapLocalValue map -> toJSON map
    ObjectLocalValue obj -> toJSON obj
    RegExpLocalValue regex -> toJSON regex
    SetLocalValue set -> toJSON set

instance ToJSON RemoteReference

instance ToJSON SharedReference

instance ToJSON RemoteObjectReference

instance ToJSON SharedId

instance ToJSON ListLocalValue

-- Types with typ field that need manual handling
instance ToJSON ArrayLocalValue where
  toJSON (MkArrayLocalValue _ value) =
    object
      [ "type" .= ("array" :: Text),
        "value" .= value
      ]

instance ToJSON DateLocalValue where
  toJSON (MkDateLocalValue value) =
    object
      [ "type" .= ("date" :: Text),
        "value" .= value
      ]

instance ToJSON MappingLocalValue

instance ToJSON MapLocalValue where
  toJSON (MkMapLocalValue _ value) =
    object
      [ "type" .= ("map" :: Text),
        "value" .= value
      ]

instance ToJSON ObjectLocalValue where
  toJSON (MkObjectLocalValue _ value) =
    object
      [ "type" .= ("object" :: Text),
        "value" .= value
      ]

instance ToJSON RegExpValue

instance ToJSON RegExpLocalValue where
  toJSON (MkRegExpLocalValue _ value) =
    object
      [ "type" .= ("regexp" :: Text),
        "value" .= value
      ]

instance ToJSON SetLocalValue where
  toJSON (MkSetLocalValue _ value) =
    object
      [ "type" .= ("set" :: Text),
        "value" .= value
      ]

-- Result types
instance ToJSON ScriptResult where
  toJSON = enumCamelCase

instance ToJSON AddPreloadScriptResult

instance ToJSON CallFunctionResult

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
        Window {context, sandbox} ->
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

instance ToJSON EvaluateResult where
  toJSON :: EvaluateResult -> Value
  toJSON = \case
    EvaluateResultSuccess {result, realm} ->
      object
        [ "type" .= ("success" :: Text),
          "result" .= result,
          "realm" .= realm
        ]
    EvaluateResultException {exceptionDetails, realm} ->
      object
        [ "type" .= ("exception" :: Text),
          "exceptionDetails" .= exceptionDetails,
          "realm" .= realm
        ]

instance ToJSON ExceptionDetails

instance ToJSON StackTrace

instance ToJSON StackFrame

-- Event types
instance ToJSON ScriptEvent where
  toJSON = \case
    MessageEvent {params} ->
      object
        [ "method" .= ("script.message" :: Text),
          "params" .= params
        ]
    RealmCreatedEvent realmInfo ->
      object
        [ "method" .= ("script.realmCreated" :: Text),
          "params" .= realmInfo
        ]
    RealmDestroyed realm ->
      object
        [ "method" .= ("script.realmDestroyed" :: Text),
          "params" .= object ["realm" .= realm]
        ]

instance ToJSON Message

instance ToJSON Source

-- ChannelValue has typ field that needs manual handling
instance ToJSON ChannelValue where
  toJSON (MkChannelValue _ value) =
    object
      [ "type" .= ("channel" :: Text),
        "value" .= value
      ]

instance ToJSON ChannelProperties
