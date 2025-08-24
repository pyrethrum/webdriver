module WebDriverPreCore.BiDi.BrowsingContext where

import Data.Aeson (FromJSON, KeyValue (..), ToJSON (..), Value, object, (.=))
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSInt, JSUInt, NodeRemoteValue)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, opt)
import Prelude (Bool, Eq, Float, Maybe, Semigroup ((<>)), Show, ($))

-- ######### REMOTE #########

-- |  for activate command
newtype Activate = MkActivate
  { context :: BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON Activate

-- |  for captureScreenshot command
data CaptureScreenshot = MkCaptureScreenshot
  { context :: BrowsingContext,
    origin :: Maybe ScreenShotOrigin,
    format :: Maybe ImageFormat,
    clip :: Maybe ClipRectangle
  }
  deriving (Show, Eq, Generic)

instance ToJSON CaptureScreenshot

data ScreenShotOrigin = Viewport | Document deriving (Show, Eq, Generic)

instance ToJSON ScreenShotOrigin where
  toJSON :: ScreenShotOrigin -> Value
  toJSON = enumCamelCase

-- | Clip rectangle for screenshots
data ClipRectangle
  = BoxClipRectangle
      { x :: Float,
        y :: Float,
        width :: Float,
        height :: Float
      }
  | ElementClipRectangle
      { element :: Text -- script.SharedReference
      }
  deriving (Show, Eq, Generic)

instance ToJSON ClipRectangle where
  toJSON :: ClipRectangle -> Value
  toJSON = \case
    BoxClipRectangle {x, y, width, height} ->
      object
        [ "type" .= "box",
          "x" .= x,
          "y" .= y,
          "width" .= width,
          "height" .= height
        ]
    ElementClipRectangle {element} ->
      object
        [ "type" .= "element",
          "element" .= element
        ]

-- | Image format specification
data ImageFormat = MkImageFormat
  { imageType :: Text,
    quality :: Maybe Float
  }
  deriving (Show, Eq, Generic)

instance ToJSON ImageFormat

-- |  for close command
data Close = MkClose
  { context :: BrowsingContext,
    promptUnload :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Close

-- |  for create command
data Create = MkCreate
  { createType :: CreateType,
    referenceContext :: Maybe BrowsingContext,
    background :: Maybe Bool,
    userContext :: Maybe UserContext
  }
  deriving (Show, Eq, Generic)

newtype UserContext = MkUserContext Text
  deriving (Show, Eq, ToJSON, FromJSON)

instance ToJSON Create where
  toJSON :: Create -> Value
  toJSON (MkCreate createType referenceContext background userContext) =
    object $
      [ "type" .= createType
      ]
        <> catMaybes
          [ opt "referenceContext" referenceContext,
            opt "background" background,
            opt "userContext" userContext
          ]

-- |  for getTree command
data GetTree = MkGetTree
  { maxDepth :: Maybe JSUInt,
    root :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON GetTree

-- |  for handleUserPrompt command
data HandleUserPrompt = MkHandleUserPrompt
  { context :: BrowsingContext,
    accept :: Maybe Bool,
    userText :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON HandleUserPrompt

-- |  for locateNodes command
data LocateNodes = MkLocateNodes
  { context :: BrowsingContext,
    locator :: Locator,
    maxNodeCount :: Maybe JSUInt,
    serializationOptions :: Maybe Value, -- script.SerializationOptions
    startNodes :: Maybe [Text] -- script.SharedReference
  }
  deriving (Show, Eq, Generic)

instance ToJSON LocateNodes

-- |  for navigate command
data Navigate = MkNavigate
  { context :: BrowsingContext,
    url :: Text,
    wait :: Maybe ReadinessState
  }
  deriving (Show, Eq, Generic)

instance ToJSON Navigate

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

instance FromJSON BrowsingContextId
instance ToJSON BrowsingContextId

-- | Text matching type for InnerText locator
data MatchType = Full | Partial
  deriving (Show, Eq, Generic)

instance ToJSON MatchType where
  toJSON :: MatchType -> Value
  toJSON = enumCamelCase

-- | Different types of locators for elements
data Locator
  = Accessibility
      { name :: Maybe Text,
        role :: Maybe Text
      }
  | CSS
      { value :: Text
      }
  | Context
      { context :: BrowsingContext
      }
  | InnerText
      { value :: Text,
        ignoreCase :: Maybe Bool,
        matchType :: Maybe MatchType,
        maxDepth :: Maybe JSUInt
      }
  | XPath
      { value :: Text
      }
  deriving (Show, Eq)

instance ToJSON Locator where
  toJSON :: Locator -> Value
  toJSON = \case
    Accessibility {name, role} ->
      object
        [ "type" .= "accessibility",
          "name" .= name,
          "role" .= role
        ]
    CSS {value} ->
      object
        [ "type" .= "css",
          "value" .= value
        ]
    Context {context} ->
      object
        [ "type" .= "context",
          "context" .= context
        ]
    InnerText {value, ignoreCase, matchType, maxDepth} ->
      object $
        [ "type" .= "innerText",
          "value" .= value
        ]
          <> catMaybes
            [ opt "ignoreCase" ignoreCase,
              opt "matchType" matchType,
              opt "maxDepth" maxDepth
            ]
    XPath {value} ->
      object
        [ "type" .= "xpath",
          "value" .= value
        ]

-- | Readiness state of a browsing context
data ReadinessState = None | Interactive | Complete
  deriving (Show, Eq, Generic)

instance ToJSON ReadinessState where
  toJSON :: ReadinessState -> Value
  toJSON = enumCamelCase

-- | User prompt types
data UserPromptType = Alert | BeforeUnload | Confirm | Prompt
  deriving (Show, Eq, Generic)

-- | Type of browsing context to create
data CreateType = Tab | Window
  deriving (Show, Eq, Generic)

instance ToJSON CreateType where
  toJSON :: CreateType -> Value
  toJSON = enumCamelCase

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

newtype CreateResult = MkCreateResult
  { browsingContext :: BrowsingContext
  }
  deriving newtype (Show, Eq, FromJSON)

instance FromJSON NavigateResult

instance FromJSON TraverseHistoryResult

newtype GetTreeResult = MkGetTreeResult
  { info :: [Info]
  }
  deriving newtype (Show, Eq, FromJSON)

newtype LocateNodesResult = MkLocateNodesResult
  { nodeRemoteValues :: [NodeRemoteValue]
  }
  deriving newtype (Show, Eq, FromJSON)

newtype CaptureScreenshotResult = MkCaptureScreenshotResult
  { base64Text :: Text
  }
  deriving newtype (Show, Eq, FromJSON)

newtype PrintResult = MkPrintResult
  { base64Text :: Text
  }
  deriving newtype (Show, Eq, FromJSON)

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

instance FromJSON Info

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

-- Additional ToJSON instances for missing types
instance ToJSON Print
instance ToJSON Reload
instance ToJSON SetViewport
instance ToJSON TraverseHistory
instance ToJSON PrintMargin
instance ToJSON PrintPage
instance ToJSON Viewport
instance ToJSON UserPromptClosed
instance ToJSON UserPromptOpened
instance ToJSON Navigation
