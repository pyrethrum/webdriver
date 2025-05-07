module WebDriverPreCore.BiDi.Network where

-- This module provides functionality related to BiDi (Bidirectional) network operations
-- for WebDriverPreCore. It is currently a placeholder for future implementation.

-- Data structures for network protocol
import Data.Text (Text)
import Data.Word (Word)
import GHC.Generics (Generic)
import Prelude (Bool, Maybe, Show, Eq, Float)
import WebDriverPreCore.BiDi.Script (StackTrace)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext)

-- | NetworkResult type
data NetworkResult = AddInterceptResult NetworkAddInterceptResult
  deriving (Show, Eq, Generic)

-- | NetworkEvent type
data NetworkEvent
  = AuthRequired NetworkAuthRequiredParameters
  | BeforeRequestSent NetworkBeforeRequestSentParameters
  | FetchError NetworkFetchErrorParameters
  | ResponseCompleted NetworkResponseCompletedParameters
  | ResponseStarted NetworkResponseStartedParameters
  deriving (Show, Eq, Generic)

-- | Authentication challenge details
data NetworkAuthChallenge = MkNetworkAuthChallenge
  { authScheme :: Text
  , authRealm :: Text
  } deriving (Show, Eq, Generic)

-- | Base parameters for network events
data NetworkBaseParameters = MkNetworkBaseParameters
  { context :: Maybe BrowsingContext
  , isBlocked :: Bool
  , navigation :: Maybe Navigation
  , redirectCount :: Word
  , request :: NetworkRequestData
  , timestamp :: Word
  , intercepts :: Maybe [NetworkIntercept]
  } deriving (Show, Eq, Generic)

-- | BytesValue can be either string or base64-encoded
data NetworkBytesValue
  = StringValue Text
  | Base64Value Text
  deriving (Show, Eq, Generic)

-- | Cookie SameSite attribute
data NetworkSameSite
  = Strict
  | Lax
  | None
  deriving (Show, Eq, Generic)

-- | Cookie information
data NetworkCookie = MkNetworkCookie
  { cookieName :: Text
  , cookieValue :: NetworkBytesValue
  , cookieDomain :: Text
  , cookiePath :: Text
  , cookieSize :: Word
  , cookieHttpOnly :: Bool
  , cookieSecure :: Bool
  , cookieSameSite :: NetworkSameSite
  , cookieExpiry :: Maybe Word
  } deriving (Show, Eq, Generic)

-- | Timing information for fetches
data NetworkFetchTimingInfo = MkNetworkFetchTimingInfo
  { timeOrigin :: Float
  , requestTime :: Float
  , redirectStart :: Float
  , redirectEnd :: Float
  , fetchStart :: Float
  , dnsStart :: Float
  , dnsEnd :: Float
  , connectStart :: Float
  , connectEnd :: Float
  , tlsStart :: Float
  , requestStart :: Float
  , responseStart :: Float
  , responseEnd :: Float
  } deriving (Show, Eq, Generic)

-- | Headers for requests and responses
data NetworkHeader = MkNetworkHeader
  { headerName :: Text
  , headerValue :: NetworkBytesValue
  } deriving (Show, Eq, Generic)

-- | Information about what initiated a request
data NetworkInitiatorType
  = Parser
  | Script
  | Preflight
  | Other
  deriving (Show, Eq, Generic)

data NetworkInitiator = MkNetworkInitiator
  { initiatorColumnNumber :: Maybe Word
  , initiatorLineNumber :: Maybe Word
  , initiatorRequest :: Maybe NetworkRequest
  , initiatorStackTrace :: Maybe StackTrace
  , initiatorType :: Maybe NetworkInitiatorType
  } deriving (Show, Eq, Generic)

-- | Network intercept identifier
newtype NetworkIntercept = MkNetworkIntercept Text
  deriving (Show, Eq, Generic)

-- | Network request identifier
newtype NetworkRequest = MkNetworkRequest Text
  deriving (Show, Eq, Generic)

-- | Request data
data NetworkRequestData = MkNetworkRequestData
  { requestId :: NetworkRequest
  , requestUrl :: Text
  , requestMethod :: Text
  , requestHeaders :: [NetworkHeader]
  , requestCookies :: [NetworkCookie]
  , requestHeadersSize :: Word
  , requestBodySize :: Maybe Word
  , requestDestination :: Text
  , requestInitiatorType :: Maybe Text
  , requestTimings :: NetworkFetchTimingInfo
  } deriving (Show, Eq, Generic)

-- | Response content information
data NetworkResponseContent = MkNetworkResponseContent
  { contentSize :: Word
  } deriving (Show, Eq, Generic)

-- | Response data
data NetworkResponseData = MkNetworkResponseData
  { responseUrl :: Text
  , responseProtocol :: Text
  , responseStatus :: Word
  , responseStatusText :: Text
  , responseFromCache :: Bool
  , responseHeaders :: [NetworkHeader]
  , responseMimeType :: Text
  , responseBytesReceived :: Word
  , responseHeadersSize :: Maybe Word
  , responseBodySize :: Maybe Word
  , responseContent :: NetworkResponseContent
  , responseAuthChallenges :: Maybe [NetworkAuthChallenge]
  } deriving (Show, Eq, Generic)

-- | Result of adding an intercept
data NetworkAddInterceptResult = MkNetworkAddInterceptResult
  { addedIntercept :: NetworkIntercept
  } deriving (Show, Eq, Generic)

-- | Parameters for authentication required events
newtype NetworkAuthRequiredParameters = MkNetworkAuthRequiredParameters
  { authRequiredResponse :: NetworkResponseData
  } deriving (Show, Eq, Generic)

-- | Parameters for before request sent events
newtype NetworkBeforeRequestSentParameters = MkNetworkBeforeRequestSentParameters
  { beforeRequestInitiator :: Maybe NetworkInitiator
  } deriving (Show, Eq, Generic)

-- | Parameters for fetch error events
newtype NetworkFetchErrorParameters = MkNetworkFetchErrorParameters
  { fetchErrorText :: Text
  } deriving (Show, Eq, Generic)

-- | Parameters for response completed events
newtype NetworkResponseCompletedParameters = MkNetworkResponseCompletedParameters
  { completedResponse :: NetworkResponseData
  } deriving (Show, Eq, Generic)

-- | Parameters for response started events
newtype NetworkResponseStartedParameters = MkNetworkResponseStartedParameters
  { startedResponse :: NetworkResponseData
  } deriving (Show, Eq, Generic)

{-
NetworkResult = (
   network.AddInterceptResult
)

NetworkEvent = (
    network.AuthRequired //
    network.BeforeRequestSent //
    network.FetchError //
    network.ResponseCompleted //
    network.ResponseStarted
)


network.AuthChallenge = {
  scheme: text,
  realm: text,
}

network.BaseParameters = (
    context: browsingContext.BrowsingContext / null,
    isBlocked: bool,
    navigation: browsingContext.Navigation / null,
    redirectCount: js-uint,
    request: network.RequestData,
    timestamp: js-uint,
    ? intercepts: [+network.Intercept]
)

network.BytesValue = network.StringValue / network.Base64Value;

network.StringValue = {
  type: "string",
  value: text,
}

network.Base64Value = {
  type: "base64",
  value: text,
}


network.SameSite = "strict" / "lax" / "none"


network.Cookie = {
    name: text,
    value: network.BytesValue,
    domain: text,
    path: text,
    size: js-uint,
    httpOnly: bool,
    secure: bool,
    sameSite: network.SameSite,
    ? expiry: js-uint,
    Extensible,
}

network.FetchTimingInfo = {
    timeOrigin: float,
    requestTime: float,
    redirectStart: float,
    redirectEnd: float,
    fetchStart: float,
    dnsStart: float,
    dnsEnd: float,
    connectStart: float,
    connectEnd: float,
    tlsStart: float,
    
    requestStart: float,
    responseStart: float,
    
    responseEnd: float,
}

network.Header = {
  name: text,
  value: network.BytesValue,
}

network.Initiator = {
    ? columnNumber: js-uint,
    ? lineNumber: js-uint,
    ? request: network.Request,
    ? stackTrace: script.StackTrace,
    ? type: "parser" / "script" / "preflight" / "other"
}

network.Intercept = text

network.Request = text;

network.RequestData = {
    request: network.Request,
    url: text,
    method: text,
    headers: [*network.Header],
    cookies: [*network.Cookie],
    headersSize: js-uint,
    bodySize: js-uint / null,
    destination: text,
    initiatorType: text / null,
    timings: network.FetchTimingInfo,
}

network.ResponseContent = {
    size: js-uint
}

network.ResponseData = {
    url: text,
    protocol: text,
    status: js-uint,
    statusText: text,
    fromCache: bool,
    headers: [*network.Header],
    mimeType: text,
    bytesReceived: js-uint,
    headersSize: js-uint / null,
    bodySize: js-uint / null,
    content: network.ResponseContent,
    ?authChallenges: [*network.AuthChallenge],
}

network.AddInterceptResult = {
  intercept: network.Intercept
}

network.AuthRequired = (
  method: "network.authRequired",
  params: network.AuthRequiredParameters
)

network.AuthRequiredParameters = {
  network.BaseParameters,
  response: network.ResponseData
}

 network.BeforeRequestSent = (
  method: "network.beforeRequestSent",
  params: network.BeforeRequestSentParameters
 )

network.BeforeRequestSentParameters = {
  network.BaseParameters,
  ? initiator: network.Initiator,
}

 network.FetchError = (
  method: "network.fetchError",
  params: network.FetchErrorParameters
 )

network.FetchErrorParameters = {
  network.BaseParameters,
  errorText: text,
}

 network.ResponseCompleted = (
  method: "network.responseCompleted",
  params: network.ResponseCompletedParameters
 )

network.ResponseCompletedParameters = {
  network.BaseParameters,
  response: network.ResponseData,
}

 network.ResponseStarted = (
  method: "network.responseStarted",
  params: network.ResponseStartedParameters
 )

network.ResponseStartedParameters = {
  network.BaseParameters,
  response: network.ResponseData,
}

-}