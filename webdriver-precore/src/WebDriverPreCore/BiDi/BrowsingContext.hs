module WebDriverPreCore.BiDi.BrowsingContext where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map
import WebDriverPreCore.BiDi.Script (NodeRemoteValue)
import WebDriverPreCore.BiDi.CoreTypes (JSUint, JSInt)

data Info = Info
  { children :: Maybe [Info]  -- null allowed per spec
  , clientWindow :: Text
  , context :: BrowsingContext
  , originalOpener :: Maybe BrowsingContext  -- null allowed
  , url :: Text
  , userContext :: Text
  , parent :: Maybe BrowsingContext  -- null allowed
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Locator types
data Locator
  = AccessibilityLocator AccessibilityLocator
  | CssLocator CssLocator
  | ContextLocator ContextLocator
  | InnerTextLocator InnerTextLocator
  | XPathLocator XPathLocator
  deriving (Show, Generic, ToJSON, FromJSON)

data AccessibilityLocator = AccessibilityLocator
  { typ :: Text  -- "accessibility"
  , value :: AccessibilityValue
  } deriving (Show, Generic)

instance ToJSON AccessibilityLocator where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "typ" -> "type"
    x -> x }

instance FromJSON AccessibilityLocator where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "type" -> "typ"
    x -> x }

data AccessibilityValue = AccessibilityValue
  { name :: Maybe Text
  , role :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Other locator types implemented similarly...
data CssLocator = CssLocator { typ :: Text, value :: Text } -- "css"
data ContextLocator = ContextLocator { typ :: Text, value :: ContextValue } -- "context"
data InnerTextLocator = InnerTextLocator { typ :: Text, value :: Text, ignoreCase :: Maybe Bool, matchType :: Maybe Text, maxDepth :: Maybe JSUint } -- "innerText"
data XPathLocator = XPathLocator { typ :: Text, value :: Text } -- "xpath"

-- Result types
data BrowsingContextResult
  = CaptureScreenshotResult CaptureScreenshotResult
  | CreateResult CreateResult
  | GetTreeResult GetTreeResult
  | LocateNodesResult LocateNodesResult
  | NavigateResult NavigateResult
  | PrintResult PrintResult
  | TraverseHistoryResult (Map.Map Text Value)  -- EmptyResult with Extensible
  deriving (Show, Generic, ToJSON, FromJSON)

data CaptureScreenshotResult = CaptureScreenshotResult
  { data_ :: Text  -- 'data' is a Haskell keyword
  } deriving (Show, Generic)

instance ToJSON CaptureScreenshotResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "data_" -> "data"
    x -> x }

instance FromJSON CaptureScreenshotResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \case
    "data" -> "data_"
    x -> x }

data CreateResult = CreateResult
  { context :: BrowsingContext
  } deriving (Show, Generic, ToJSON, FromJSON)

data GetTreeResult = GetTreeResult
  { contexts :: [Info]
  } deriving (Show, Generic, ToJSON, FromJSON)

data LocateNodesResult = LocateNodesResult
  { nodes :: [NodeRemoteValue]
  } deriving (Show, Generic, ToJSON, FromJSON)

data NavigateResult = NavigateResult
  { navigation :: Maybe Text  -- null allowed
  , url :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

data PrintResult = PrintResult
  { data_ :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)  -- Similar to CaptureScreenshotResult

-- Event types
data BrowsingContextEvent
  = ContextCreated ContextCreated
  | ContextDestroyed ContextDestroyed
  | DomContentLoaded NavigationEvent
  | DownloadWillBegin DownloadEvent
  | FragmentNavigated NavigationEvent
  | HistoryUpdated HistoryUpdatedEvent
  | Load NavigationEvent
  | NavigationAborted NavigationEvent
  | NavigationCommitted NavigationEvent
  | NavigationFailed NavigationEvent
  | NavigationStarted NavigationEvent
  | UserPromptClosed UserPromptClosedEvent
  | UserPromptOpened UserPromptOpenedEvent
  deriving (Show, Generic, ToJSON, FromJSON)

data NavigationEvent = NavigationEvent
  { method :: Text
  , params :: NavigationInfo
  } deriving (Show, Generic, ToJSON, FromJSON)

data NavigationInfo = NavigationInfo
  { context :: BrowsingContext
  , navigation :: Maybe Text  -- null allowed
  , timestamp :: JSUint
  , url :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- Other event types implemented similarly...
data ContextCreated = ContextCreated { method :: Text, params :: Info }
data ContextDestroyed = ContextDestroyed { method :: Text, params :: Info }
data DownloadEvent = DownloadEvent { method :: Text, params :: DownloadParams }
data HistoryUpdatedEvent = HistoryUpdatedEvent { method :: Text, params :: HistoryUpdatedParams }
data UserPromptClosedEvent = UserPromptClosedEvent { method :: Text, params :: UserPromptClosedParams }
data UserPromptOpenedEvent = UserPromptOpenedEvent { method :: Text, params :: UserPromptOpenedParams }

-- Parameter types
data DownloadParams = DownloadParams
  { suggestedFilename :: Text
  , baseNavigationInfo :: NavigationInfo
  } deriving (Show, Generic, ToJSON, FromJSON)

data HistoryUpdatedParams = HistoryUpdatedParams
  { context :: BrowsingContext
  , url :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

data UserPromptClosedParams = UserPromptClosedParams
  { context :: BrowsingContext
  , accepted :: Bool
  , typ :: Text  -- "alert", "beforeunload", etc.
  , userText :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON)

data UserPromptOpenedParams = UserPromptOpenedParams
  { context :: BrowsingContext
  , handler :: Text  -- "accept", "dismiss", etc.
  , message :: Text
  , typ :: Text
  , defaultValue :: Maybe Text
  } deriving (Show, Generic, ToJSON, FromJSON)