module WebDriverPreCore.BiDi.Network
  ( -- * NetworkCommand
    NetworkCommand (..),
    AddIntercept (..),
    InterceptPhase (..),
    UrlPattern (..),
    ContinueRequest (..),
    RequestId (..),
    BytesValue (..),
    Cookie (..),
    SameSite (..),
    Header (..),
    ContinueResponse (..),
    ContinueWithAuth (..),
    Intercept (..),
    AuthCredentials (..),
    AuthResponse (..),
    FailRequest (..),
    ProvideResponse (..),
    RemoveIntercept (..),
    SetCacheBehavior (..),
    CacheBehavior (..),

    -- * NetworkResult
    AddInterceptResult (..),

    -- * NetworkEvent
    NetworkEvent (..),
    AuthRequired (..),
    ResponseData (..),
    ResponseContent (..),
    AuthChallenge (..),
    BeforeRequestSent (..),
    Initiator (..),
    InitiatorType (..),
    FetchError (..),
    ResponseCompleted (..),
    ResponseStarted (..),
  )
where

-- This module provides functionality related to BiDi (Bidirectional) network operations
-- for WebDriverPreCore. It is currently a placeholder for future implementation.

-- Data structures for network protocol
import Data.Text (Text)
import Data.Word (Word)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (UserContext)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSUInt)
import WebDriverPreCore.BiDi.Script (StackTrace)
import Prelude (Bool, Eq, Maybe, Show)

-- ######### REMOTE #########

-- | NetworkCommand type for remote end operations
data NetworkCommand
  = AddDataCollector AddDataCollector
  | AddIntercept AddIntercept
  | ContinueRequest ContinueRequest
  | ContinueResponse ContinueResponse
  | ContinueWithAuth ContinueWithAuth
  | DisownData DisownData
  | FailRequest FailRequest
  | GetData GetData
  | ProvideResponse ProvideResponse
  | RemoveDataCollector RemoveDataCollector
  | RemoveIntercept RemoveIntercept
  | SetCacheBehavior SetCacheBehavior
  deriving (Show, Eq, Generic)

data AddDataCollector = MkAddDataCollector
  { dataTypes :: [DataType],
    maxEncodedDataSize :: JSUInt,
    collectorType :: Maybe CollectorType,
    contexts :: Maybe [BrowsingContext],
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

-- TODO - not sure what this is about
-- network.DataType = "response"
newtype DataType = MkDataType {dataType :: Text}
  deriving (Show, Eq, Generic)

-- TODO - not sure what this is about
-- network.CollectorType = "blob"
newtype CollectorType = MkCollectorType {collectorType :: Text}
  deriving (Show, Eq, Generic)

data DisownData = MkDisownData
  { dataType :: DataType,
    collector :: Collector,
    request :: Request
  }
  deriving (Show, Eq, Generic)

newtype Collector = MkCollector {collector :: Text}
  deriving (Show, Eq, Generic)

newtype Request = MkRequest {request :: Text}
  deriving (Show, Eq, Generic)

data GetData = MkGetData
  { dataType :: DataType,
    collector :: Maybe Collector,
    disown :: Bool,
    request :: Request
  }
  deriving (Show, Eq, Generic)

data RemoveDataCollector = MkRemoveDataCollector
  { collector :: Collector
  }
  deriving (Show, Eq, Generic)

-- | AddIntercept parameters
data AddIntercept = MkAddIntercept
  { phases :: [InterceptPhase],
    contexts :: [BrowsingContext],
    urlPatterns :: Maybe [UrlPattern]
  }
  deriving (Show, Eq, Generic)

-- | Intercept phases for network requests
data InterceptPhase
  = BeforeRequestSent
  | ResponseStarted
  | AuthRequired
  deriving (Show, Eq, Generic)

-- | URL pattern for interception
data UrlPattern = MkUrlPattern
  { protocol :: Maybe Text,
    hostname :: Maybe Text,
    port :: Maybe Text,
    pathname :: Maybe Text,
    search :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | ContinueRequest parameters
data ContinueRequest = MkContinueRequest
  { request :: RequestId,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    method :: Maybe Text,
    url :: Maybe Text
  }
  deriving (Show, Eq, Generic)

newtype RequestId = MkRequestId {id :: Text}
  deriving (Show, Eq, Generic)

-- | BytesValue can be either string or base64-encoded
data BytesValue
  = StringValue Text
  | Base64Value Text
  deriving (Show, Eq, Generic)

-- | Cookie information
data Cookie = MkCookie
  { name :: Text,
    value :: BytesValue,
    domain :: Text,
    path :: Text,
    size :: Word,
    httpOnly :: Bool,
    secure :: Bool,
    sameSite :: SameSite,
    expiry :: Maybe Word
  }
  deriving (Show, Eq, Generic)

data SameSite
  = Strict
  | Lax
  | None
  | Default
  deriving (Show, Eq, Generic)

-- | Headers for requests and responses
data Header = MkHeader
  { headerName :: Text,
    headerValue :: BytesValue
  }
  deriving (Show, Eq, Generic)

-- | ContinueResponse parameters
data ContinueResponse = MkContinueResponse
  { request :: RequestId,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    reasonPhrase :: Maybe Text,
    statusCode :: Maybe JSUInt
  }
  deriving (Show, Eq, Generic)

-- | ContinueWithAuth parameters
data ContinueWithAuth = MkContinueWithAuth
  { intercept :: Intercept,
    authCredentials :: Maybe AuthCredentials,
    response :: AuthResponse
  }
  deriving (Show, Eq, Generic)

-- | Network intercept identifier
newtype Intercept = MkIntercept Text
  deriving (Show, Eq, Generic)

-- | Auth credentials for authentication
data AuthCredentials = MkAuthCredentials
  { password :: Text,
    username :: Text
  }
  deriving (Show, Eq, Generic)

-- | Authentication response type
data AuthResponse
  = DefaultResponse
  | Cancel
  | Provide
  deriving (Show, Eq, Generic)

-- | FailRequest parameters
data FailRequest = MkFailRequest
  { intercept :: Intercept,
    errorText :: Text
  }
  deriving (Show, Eq, Generic)

-- | ProvideResponse parameters
data ProvideResponse = MkProvideResponse
  { intercept :: Intercept,
    body :: Maybe BytesValue,
    cookies :: Maybe [Cookie],
    headers :: Maybe [Header],
    reasonPhrase :: Text,
    statusCode :: Word
  }
  deriving (Show, Eq, Generic)

-- | RemoveIntercept parameters
newtype RemoveIntercept = MkRemoveIntercept
  { intercept :: Intercept
  }
  deriving (Show, Eq, Generic)

-- | SetCacheBehavior parameters
data SetCacheBehavior = MkSetCacheBehavior
  { behavior :: CacheBehavior,
    context :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

-- | Cache behavior options
data CacheBehavior
  = DefaultCacheBehavior
  | BypassCache
  | ForceCacheIgnoreNoStore
  deriving (Show, Eq, Generic)

-- ######### LOCAL #########

newtype AddInterceptResult = MkAddInterceptResult {addedIntercept :: Intercept}
  deriving (Show, Eq, Generic)

data NetworkEvent
  = AuthRequiredEvent AuthRequired
  | BeforeRequestSentEvent BeforeRequestSent
  | FetchError FetchError
  | ResponseCompleted ResponseCompleted
  | ResponseStartedEvent ResponseStarted
  deriving (Show, Eq, Generic)

-- | AuthRequired parameters
newtype AuthRequired = MkAuthRequired
  { authRequiredResponse :: ResponseData
  }
  deriving (Show, Eq, Generic)

-- | Response data
data ResponseData = MkResponseData
  { responseUrl :: Text,
    responseProtocol :: Text,
    responseStatus :: Word,
    responseStatusText :: Text,
    responseFromCache :: Bool,
    responseHeaders :: [Header],
    responseMimeType :: Text,
    responseBytesReceived :: Word,
    responseHeadersSize :: Maybe Word,
    responseBodySize :: Maybe Word,
    responseContent :: ResponseContent,
    responseAuthChallenges :: Maybe [AuthChallenge]
  }
  deriving (Show, Eq, Generic)

-- | Response content information
newtype ResponseContent = MkResponseContent
  { contentSize :: JSUInt
  }
  deriving (Show, Eq, Generic)

data AuthChallenge = MkAuthChallenge
  { authScheme :: Text,
    authRealm :: Text
  }
  deriving (Show, Eq, Generic)

-- | BeforeRequestSent parameters
newtype BeforeRequestSent = MkBeforeRequestSent
  { beforeRequestInitiator :: Maybe Initiator
  }
  deriving (Show, Eq, Generic)

data Initiator = MkInitiator
  { initiatorColumnNumber :: Maybe Word,
    initiatorLineNumber :: Maybe Word,
    initiatorRequest :: Maybe RequestId,
    initiatorStackTrace :: Maybe StackTrace,
    initiatorType :: Maybe InitiatorType
  }
  deriving (Show, Eq, Generic)

-- | Information about what initiated a request
data InitiatorType
  = Parser
  | Script
  | Preflight
  | Other
  deriving (Show, Eq, Generic)

-- | FetchError parameters
newtype FetchError = MkFetchError
  { fetchErrorText :: Text
  }
  deriving (Show, Eq, Generic)

-- | ResponseCompleted parameters
newtype ResponseCompleted = MkResponseCompleted
  { completedResponse :: ResponseData
  }
  deriving (Show, Eq, Generic)

-- | ResponseStarted parameters
data ResponseStarted = MkResponseStarted
  { startedResponse :: ResponseData
  }
  deriving (Show, Eq, Generic)

newtype GetDataResult = MkGetDataResult
  { bytes :: BytesValue
  }

newtype CollectorResult = MkCollectorResult
  { collector :: Collector
  }
