module WebDriverPreCore.BiDi.BrowsingContext where

import Data.Aeson (Value, ToJSON (..))
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSInt, JSUInt, NodeRemoteValue, BiDiMethod (bidiMethod))
import Prelude (Bool, Eq, Float, Maybe, Show, error)

-- ######### REMOTE #########

-- | Commands for browsing context operations
data BrowsingContextCommand
  = Activate Activate
  | CaptureScreenshot CaptureScreenshot
  | Close Close
  | Create Create
  | GetTree GetTree
  | HandleUserPrompt HandleUserPrompt
  | LocateNodes LocateNodes
  | Navigate Navigate
  | Print Print
  | Reload Reload
  | SetViewport SetViewport
  | TraverseHistory TraverseHistory
  deriving (Show, Eq, Generic)

instance ToJSON BrowsingContextCommand where
  toJSON :: BrowsingContextCommand -> Value
  toJSON = \case
    -- Activate cmd -> toJSON cmd
    -- CaptureScreenshot cmd -> toJSON cmd
    -- Close cmd -> toJSON cmd
    Create cmd -> toJSON cmd
    -- GetTree cmd -> toJSON cmd
    -- HandleUserPrompt cmd -> toJSON cmd
    -- LocateNodes cmd -> toJSON cmd
    -- Navigate cmd -> toJSON cmd
    -- Print cmd -> toJSON cmd
    -- Reload cmd -> toJSON cmd
    -- SetViewport cmd -> toJSON cmd
    -- TraverseHistory cmd -> toJSON cmd
    _ -> error "Unsupported browsing context command type for JSON serialization"

instance BiDiMethod BrowsingContextCommand where
  bidiMethod :: BrowsingContextCommand -> Text
  bidiMethod = \case
    Activate _ -> "browsingContext.activate"
    CaptureScreenshot _ -> "browsingContext.captureScreenshot"
    Close _ -> "browsingContext.close"
    Create _ -> "browsingContext.create"
    GetTree _ -> "browsingContext.getTree"
    HandleUserPrompt _ -> "browsingContext.handleUserPrompt"
    LocateNodes _ -> "browsingContext.locateNodes"
    Navigate _ -> "browsingContext.navigate"
    Print _ -> "browsingContext.print"
    Reload _ -> "browsingContext.reload"
    SetViewport _ -> "browsingContext.setViewport"
    TraverseHistory _ -> "browsingContext.traverseHistory"

-- |  for activate command
newtype Activate = MkActivate
  { context :: BrowsingContext
  }
  deriving (Show, Eq, Generic)

-- |  for captureScreenshot command
data CaptureScreenshot = MkCaptureScreenshot
  { context :: BrowsingContext,
    origin :: Maybe Text, -- "viewport" / "document"
    format :: Maybe ImageFormat,
    clip :: Maybe ClipRectangle
  }
  deriving (Show, Eq, Generic)

-- | Clip rectangle for screenshots
data ClipRectangle
  = BoxClipRectangle
      { clipType :: Text, -- "box"
        x :: Float,
        y :: Float,
        width :: Float,
        height :: Float
      }
  | ElementClipRectangle
      { clipType :: Text, -- "element"
        element :: Text -- script.SharedReference
      }
  deriving (Show, Eq, Generic)

-- | Image format specification
data ImageFormat = MkImageFormat
  { imageType :: Text,
    quality :: Maybe Float
  }
  deriving (Show, Eq, Generic)

-- |  for close command
data Close = MkClose
  { context :: BrowsingContext,
    promptUnload :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

-- |  for create command
data Create = MkCreate
  { createType :: CreateType,
    referenceContext :: Maybe BrowsingContext,
    background :: Maybe Bool,
    userContext :: Maybe Text -- browser.UserContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON Create 

-- |  for getTree command
data GetTree = MkGetTree
  { maxDepth :: Maybe JSUInt,
    root :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

-- |  for handleUserPrompt command
data HandleUserPrompt = MkHandleUserPrompt
  { context :: BrowsingContext,
    accept :: Maybe Bool,
    userText :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- |  for locateNodes command
data LocateNodes = MkLocateNodes
  { context :: BrowsingContext,
    locator :: Locator,
    maxNodeCount :: Maybe JSUInt,
    serializationOptions :: Maybe Value, -- script.SerializationOptions
    startNodes :: Maybe [Text] -- script.SharedReference
  }
  deriving (Show, Eq, Generic)

-- |  for navigate command
data Navigate = MkNavigate
  { context :: BrowsingContext,
    url :: Text,
    wait :: Maybe ReadinessState
  }
  deriving (Show, Eq, Generic)

-- |  for print command
data Print = MkPrint
  { context :: BrowsingContext,
    background :: Maybe Bool,
    margin :: Maybe PrintMargin,
    orientation :: Maybe Text, -- "portrait" / "landscape"
    page :: Maybe PrintPage,
    pageRanges :: Maybe [Value], -- Mix of JSUInt and Text
    scale :: Maybe Float,
    shrinkToFit :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

-- |  for reload command
data Reload = MkReload
  { context :: BrowsingContext,
    ignoreCache :: Maybe Bool,
    wait :: Maybe ReadinessState
  }
  deriving (Show, Eq, Generic)

-- |  for setViewport command
data SetViewport = MkSetViewport
  { context :: Maybe BrowsingContext,
    viewport :: Maybe (Maybe Viewport), -- Viewport or null
    devicePixelRatio :: Maybe (Maybe Float), -- Float or null
    userContexts :: Maybe [Text] -- browser.UserContext
  }
  deriving (Show, Eq, Generic)

-- |  for traverseHistory command
data TraverseHistory = MkTraverseHistory
  { context :: BrowsingContext,
    delta :: JSInt
  }
  deriving (Show, Eq, Generic)

-- | Represents a browsing context identifier
newtype BrowsingContextId = MkBrowsingContextId Text
  deriving (Show, Eq, Generic)

-- | Different types of locators for elements
data Locator
  = Accessibility
      { typ :: Text, -- "accessibility"
        name :: Maybe Text,
        role :: Maybe Text
      }
  | Css
      { typ :: Text, -- "css"
        value :: Text
      }
  | Context
      { typ :: Text, -- "context"
        context :: BrowsingContext
      }
  | InnerText
      { typ :: Text, -- "innerText"
        value :: Text,
        ignoreCase :: Maybe Bool,
        matchType :: Maybe Text, -- "full" / "partial"
        maxDepth :: Maybe JSUInt
      }
  | XPath
      { typ :: Text, -- "xpath"
        value :: Text
      }
  deriving (Show, Eq, Generic)

-- | Readiness state of a browsing context
data ReadinessState = None | Interactive | Complete
  deriving (Show, Eq, Generic)

-- | User prompt types
data UserPromptType = Alert | BeforeUnload | Confirm | Prompt
  deriving (Show, Eq, Generic)

-- | Type of browsing context to create
data CreateType = Tab | Window
  deriving (Show, Eq, Generic)

instance ToJSON CreateType where
  toJSON :: CreateType -> Value
  toJSON = \case
    Tab -> "tab"
    Window -> "window"

-- | Print margin
data PrintMargin = MkPrintMargin
  { bottom :: Maybe Float,
    left :: Maybe Float,
    right :: Maybe Float,
    top :: Maybe Float
  }
  deriving (Show, Eq, Generic)

-- | Print page
data PrintPage = MkPrintPage
  { height :: Maybe Float,
    width :: Maybe Float
  }
  deriving (Show, Eq, Generic)

-- | Viewport dimensions
data Viewport = MkViewport
  { width :: JSUInt,
    height :: JSUInt
  }
  deriving (Show, Eq, Generic)

-- ######### Local #########

-- | Result of a browsing context command
data BrowsingContextResult
  = CaptureScreenshotResult Text
  | CreateResult BrowsingContext
  | GetTreeResult [Info]
  | LocateNodesResult [NodeRemoteValue]
  | NavigateResult NavigateResult
  | PrintResult Text
  | TraverseHistoryResult TraverseHistoryResult
  deriving (Show, Eq, Generic)

data Info = MkInfo
  { children :: Maybe [Info],
    clientWindow :: Text, -- browser.ClientWindow
    context :: BrowsingContext,
    originalOpener :: Maybe BrowsingContext,
    url :: Text,
    userContext :: Text, -- browser.UserContext
    parent :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

data NavigateResult = MkNavigateResult
  { navigation :: Maybe Text,
    url :: Text
  }
  deriving (Show, Eq, Generic)

data TraverseHistoryResult = MkTraverseHistoryResult
  { extensions :: Maybe (Map.Map Text Value)
  }
  deriving (Show, Eq, Generic)

-- | Event from a browsing context
data BrowsingContextEvent
  = ContextCreated Info
  | ContextDestroyed Info
  | DomContentLoaded NavigationInfo
  | DownloadEnd
  | DownloadWillBegin DownloadWillBegin
  | FragmentNavigated NavigationInfo
  | HistoryUpdated HistoryUpdated
  | Load NavigationInfo
  | NavigationAborted NavigationInfo
  | NavigationCommitted NavigationInfo
  | NavigationFailed NavigationInfo
  | NavigationStarted NavigationInfo
  | UserPromptClosed UserPromptClosed
  | UserPromptOpened UserPromptOpened
  deriving (Show, Eq, Generic)

data NavigationInfo = MkNavigationInfo
  { context :: BrowsingContext,
    navigation :: Maybe Navigation,
    timestamp :: JSUInt,
    url :: Text
  }
  deriving (Show, Eq, Generic)

data DownloadEnd
  = DownloadCompleted
      { filePath :: Maybe Text,
        navigationInfo :: NavigationInfo
      }
  | DownloadCanceled {navigationInfo :: NavigationInfo}
  deriving (Show, Eq, Generic)

data DownloadWillBegin = MkDownloadWillBegin
  { suggestedFilename :: Text,
    context :: BrowsingContext,
    navigation :: Maybe Navigation,
    timestamp :: JSUInt,
    url :: Text
  }
  deriving (Show, Eq, Generic)

data HistoryUpdated = MkHistoryUpdated
  { context :: BrowsingContext,
    timestamp :: JSUInt,
    url :: Text
  }
  deriving (Show, Eq, Generic)

data UserPromptClosed = MkUserPromptClosed
  { context :: BrowsingContext,
    accepted :: Bool,
    typ :: Text, -- UserPromptType
    userText :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data UserPromptOpened = MkUserPromptOpened
  { context :: BrowsingContext,
    handler :: Text, -- session.UserPromptHandlerType
    message :: Text,
    typ :: Text, -- UserPromptType
    defaultValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

newtype Navigation = MkNavigation
  { navigationId :: Text
  }
  deriving (Show, Eq, Generic)
