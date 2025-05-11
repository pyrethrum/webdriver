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
    Disown (..),
    Target (..),
    Realm (..),
    Evaluate (..),
    GetRealms (..),
    RemovePreloadScript (..),

    -- * ScriptResult
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
    RealmDestroyedParams (..),
    ChannelValue (..),
    ChannelProperties (..),

    -- * PreloadScript
    PreloadScript (..),
  )
where

import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes
  ( BrowsingContext,
    Handle,
    InternalId,
    JSUInt,
    NodeRemoteValue,
  )
import Prelude (Bool (..), Double, Either, Eq (..), Maybe, Show)

-- https://www.w3.org/TR/2025/WD-webdriver-bidi-20250508/#module-script-definition

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
  = RemoteReference RemoteValue
  | PrimitiveLocalValue PrimitiveProtocolValue
  | ChannelValue ChannelValue
  | ArrayLocalValue ArrayLocalValue
  | DateLocalValue DateLocalValue
  | MapLocalValue MapLocalValue
  | ObjectLocalValue ObjectLocalValue
  | RegExpLocalValue RegExpLocalValue
  | SetLocalValue SetLocalValue
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
    typ :: Maybe Text
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

-- ScriptResult = (
--   script.AddPreloadScriptResult /
--   script.EvaluateResult /
--   script.GetRealmsResult
-- )

-- ScriptEvent = (
--   script.Message //
--   script.RealmCreated //
--   script.RealmDestroyed
-- )

data ScriptResult
  = AddPreloadScriptResult {script :: PreloadScript}
  | EvaluateResult EvaluateResult
  | GetRealmsResult {realms :: [RealmInfo]}
  deriving (Show, Eq, Generic)

data RealmInfo
  = Window
      { base :: BaseRealmInfo,
        typ :: Text, -- "window"
        context :: BrowsingContext,
        sandbox :: Maybe Text
      }
  | DedicatedWorker {base :: BaseRealmInfo, typ :: Text, owners :: [Realm]}
  | SharedWorker {base :: BaseRealmInfo, typ :: Text}
  | ServiceWorker {base :: BaseRealmInfo, typ :: Text}
  | Worker {base :: BaseRealmInfo, typ :: Text}
  | PaintWorklet {base :: BaseRealmInfo, typ :: Text}
  | AudioWorklet {base :: BaseRealmInfo, typ :: Text}
  | Worklet {base :: BaseRealmInfo, typ :: Text}
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
  | RealmDestroyedEvent RealmDestroyedParams
  deriving (Show, Generic)

data Message = MkMessage
  { channel :: Channel,
    messageData :: RemoteValue,
    source :: Source
  }
  deriving (Show, Eq, Generic)

-- Channel types
newtype Channel = Channel Text deriving newtype (Show, Eq)

data Source = MkSource
  { realm :: Realm,
    context :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

newtype RealmDestroyedParams = RealmDestroyedParams
  { realm :: Realm
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
