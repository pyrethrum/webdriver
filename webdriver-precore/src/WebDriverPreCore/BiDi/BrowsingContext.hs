module WebDriverPreCore.BiDi.BrowsingContext where

import Data.Aeson (FromJSON (..), GFromJSON, KeyValue (..), Options (..), ToJSON (..), Value (..), Zero, defaultOptions, genericParseJSON, genericToJSON, object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import GHC.Generics ( Generic(Rep) )
import WebDriverPreCore.BiDi.Capabilities (UserPromptHandlerType)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSInt, JSUInt, NodeRemoteValue, UserContext, KnownSubscriptionType (..), ClientWindow, URL (..))
import WebDriverPreCore.BiDi.Script (SharedReference)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, fromJSONCamelCase, opt, parseJSONOmitNothing, toJSONOmitNothing)
import Prelude

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

instance ToJSON CaptureScreenshot where
  toJSON :: CaptureScreenshot -> Value
  toJSON (MkCaptureScreenshot {context, origin, format, clip}) =
    object $
      [ "context" .= context
      ]
        <> catMaybes
          [ opt "origin" origin,
            opt "format" format,
            opt "clip" clip
          ]

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

instance ToJSON ImageFormat where
  toJSON :: ImageFormat -> Value
  toJSON (MkImageFormat {imageType, quality}) =
    object $
      [ "type" .= imageType
      ]
        <> catMaybes
          [ opt "quality" quality
          ]

-- |  for close command
data Close = MkClose
  { context :: BrowsingContext,
    promptUnload :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Close where
  toJSON :: Close -> Value
  toJSON (MkClose {context, promptUnload}) =
    object $
      [ "context" .= context
      ]
        <> catMaybes
          [ opt "promptUnload" promptUnload
          ]

-- |  for create command
data Create = MkCreate
  { createType :: CreateType,
    background :: Bool,
    referenceContext :: Maybe BrowsingContext,
    userContext :: Maybe UserContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON Create where
  toJSON :: Create -> Value
  toJSON (MkCreate {createType, referenceContext, background, userContext}) =
    object $
      [ "type" .= createType,
        "background" .= background
      ]
        <> catMaybes
          [ opt "referenceContext" referenceContext,
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

instance ToJSON HandleUserPrompt where
  toJSON :: HandleUserPrompt -> Value
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- |  for locateNodes command
data LocateNodes = MkLocateNodes
  { context :: BrowsingContext,
    locator :: Locator,
    maxNodeCount :: Maybe JSUInt,
    serializationOptions :: Maybe Value, -- script.SerializationOptions
    startNodes :: Maybe [SharedReference] -- script.SharedReference
  }
  deriving (Show, Eq, Generic)

instance ToJSON LocateNodes where
  toJSON :: LocateNodes -> Value
  toJSON = toJSONOmitNothing

-- |  for navigate command
data Navigate = MkNavigate
  { context :: BrowsingContext,
    url :: URL,
    wait :: Maybe ReadinessState
  }
  deriving (Show, Eq, Generic)

instance ToJSON Navigate where
  toJSON :: Navigate -> Value
  toJSON = toJSONOmitNothing

-- |  for print command
data Print = MkPrint
  { context :: BrowsingContext,
    background :: Maybe Bool,
    margin :: Maybe PrintMargin,
    orientation :: Maybe Orientation,
    page :: Maybe PrintPage,
    pageRanges :: Maybe [PageRange],
    scale :: Maybe Float,
    shrinkToFit :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON Print where
  toJSON :: Print -> Value
  toJSON = toJSONOmitNothing

data Orientation = Portrait | Landscape deriving (Show, Eq, Generic)

instance ToJSON Orientation where
  toJSON :: Orientation -> Value
  toJSON = enumCamelCase

data PageRange
  = Page Word
  | Range
      { fromPage :: Word,
        toPage :: Word
      }
  deriving (Show, Eq)

instance ToJSON PageRange where
  toJSON :: PageRange -> Value
  toJSON = \case
    Page p -> Number $ fromIntegral p
    Range {fromPage, toPage} -> String (pack (show fromPage <> "-" <> show toPage))

-- |  for reload command
data Reload = MkReload
  { context :: BrowsingContext,
    ignoreCache :: Maybe Bool,
    wait :: Maybe ReadinessState
  }
  deriving (Show, Eq, Generic)

instance ToJSON Reload where
  toJSON :: Reload -> Value
  toJSON = toJSONOmitNothing

-- |  for setViewport command
data SetViewport = MkSetViewport
  { context :: Maybe BrowsingContext,
    viewport :: Maybe (Maybe Viewport), -- Viewport or null
    devicePixelRatio :: Maybe (Maybe Float), -- Float or null
    userContexts :: Maybe [Text] -- browser.UserContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetViewport where
  toJSON :: SetViewport -> Value
  toJSON = toJSONOmitNothing

-- |  for traverseHistory command
data TraverseHistory = MkTraverseHistory
  { context :: BrowsingContext,
    delta :: JSInt
  }
  deriving (Show, Eq, Generic)

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
      object $
        [ "type" .= "accessibility",
          "value"
            .= ( object $
                   catMaybes
                     [ opt "name" name,
                       opt "role" role
                     ]
               )
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

instance ToJSON UserPromptType where
  toJSON :: UserPromptType -> Value
  toJSON = enumCamelCase

instance FromJSON UserPromptType where
  parseJSON :: Value -> Parser UserPromptType
  parseJSON = fromJSONCamelCase

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

instance FromJSON NavigateResult

instance FromJSON TraverseHistoryResult

newtype GetTreeResult = MkGetTreeResult
  { contexts :: [Info]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GetTreeResult

newtype LocateNodesResult = MkLocateNodesResult
  { nodes :: [NodeRemoteValue]
  }
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance FromJSON LocateNodesResult

newtype CaptureScreenshotResult = MkCaptureScreenshotResult
  { base64Text :: Text
  }
  deriving newtype (Show, Eq)

parseTextData :: Text -> Value -> Parser Text
parseTextData description = withObject (unpack description) $ flip (.:) "data"

instance FromJSON CaptureScreenshotResult where
  parseJSON :: Value -> Parser CaptureScreenshotResult
  parseJSON = fmap MkCaptureScreenshotResult . parseTextData "CaptureScreenshotResult"

newtype PrintResult = MkPrintResult
  { base64Text :: Text
  }
  deriving newtype (Show, Eq)

instance FromJSON PrintResult where
  parseJSON :: Value -> Parser PrintResult
  parseJSON = fmap MkPrintResult . parseTextData "PrintResult"

data Info = MkInfo
  { children :: Maybe [Info],
    clientWindow :: ClientWindow, 
    context :: BrowsingContext,
    originalOpener :: Maybe BrowsingContext,
    url :: URL,
    userContext :: UserContext, 
    parent :: Maybe BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance FromJSON Info where
  parseJSON :: Value -> Parser Info
  parseJSON = parseJSONOmitNothing

data NavigateResult = MkNavigateResult
  { navigation :: Maybe Text,
    url :: URL
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

instance FromJSON BrowsingContextEvent where
  parseJSON :: Value -> Parser BrowsingContextEvent
  parseJSON = withObject "BrowsingContextEvent" $ \o -> do
    typ <- o .: "method"
    params <- o .: "params"
    let parsedPrms :: forall a b. (FromJSON a) => (a -> b) -> Parser b
        parsedPrms = (<&>) (parseJSON params)
    case typ of
      BrowsingContextContextCreated -> parsedPrms ContextCreated
      BrowsingContextContextDestroyed -> parsedPrms ContextDestroyed
      BrowsingContextDomContentLoaded -> parsedPrms DomContentLoaded
      BrowsingContextDownloadEnd -> pure DownloadEnd
      BrowsingContextDownloadWillBegin -> parsedPrms DownloadWillBegin
      BrowsingContextFragmentNavigated -> parsedPrms FragmentNavigated
      BrowsingContextHistoryUpdated -> parsedPrms HistoryUpdated
      BrowsingContextLoad -> parsedPrms Load
      BrowsingContextNavigationAborted -> parsedPrms NavigationAborted
      BrowsingContextNavigationCommitted -> parsedPrms NavigationCommitted
      BrowsingContextNavigationFailed -> parsedPrms NavigationFailed
      BrowsingContextNavigationStarted -> parsedPrms NavigationStarted
      BrowsingContextUserPromptClosed -> parsedPrms UserPromptClosed
      BrowsingContextUserPromptOpened -> parsedPrms UserPromptOpened
      _ -> fail $ "Unknown BrowsingContextEvent type: " <> show typ

data NavigationInfo = MkNavigationInfo
  { context :: BrowsingContext,
    navigation :: Maybe Navigation,
    timestamp :: JSUInt,
    url :: URL
  }
  deriving (Show, Eq, Generic)

instance FromJSON NavigationInfo where
  parseJSON :: Value -> Parser NavigationInfo
  parseJSON = parseJSONOmitNothing

data DownloadEnd
  = DownloadCompleted
      { filePath :: Maybe Text,
        navigationInfo :: NavigationInfo
      }
  | DownloadCanceled {navigationInfo :: NavigationInfo}
  deriving (Show, Eq, Generic)

instance FromJSON DownloadEnd where
  parseJSON :: Value -> Parser DownloadEnd
  parseJSON = withObject "DownloadEnd" $ \v -> do
    status <- v .: "status"
    case status of
      "complete" -> do
        filePath <- v .: "filepath"
        context <- v .: "context"
        navigation <- v .: "navigation"
        timestamp <- v .: "timestamp"
        url <- v .: "url"
        let navigationInfo = MkNavigationInfo {context, navigation, timestamp, url}
        pure $ DownloadCompleted {filePath, navigationInfo}
      "canceled" -> do
        context <- v .: "context"
        navigation <- v .: "navigation"
        timestamp <- v .: "timestamp"
        url <- v .: "url"
        let navigationInfo = MkNavigationInfo {context, navigation, timestamp, url}
        pure $ DownloadCanceled {navigationInfo}
      _ -> fail $ "Unknown DownloadEnd status: " <> unpack status

data DownloadWillBegin = MkDownloadWillBegin
  { suggestedFilename :: Text,
    context :: BrowsingContext,
    navigation :: Maybe Navigation,
    timestamp :: JSUInt,
    url :: URL
  }
  deriving (Show, Eq, Generic)

instance FromJSON DownloadWillBegin where
  parseJSON :: Value -> Parser DownloadWillBegin
  parseJSON = parseJSONOmitNothing

data HistoryUpdated = MkHistoryUpdated
  { context :: BrowsingContext,
    timestamp :: JSUInt,
    url :: URL
  }
  deriving (Show, Eq, Generic)

instance FromJSON HistoryUpdated

data UserPromptClosed = MkUserPromptClosed
  { context :: BrowsingContext,
    accepted :: Bool,
    promptType :: UserPromptType,
    userText :: Maybe Text
  }
  deriving (Show, Eq, Generic)

promptParser :: forall a. (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
promptParser =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = \case
          "promptType" -> "type"
          other -> other,
        omitNothingFields = True
      }

instance FromJSON UserPromptClosed where
  parseJSON :: Value -> Parser UserPromptClosed
  parseJSON = promptParser

data UserPromptOpened = MkUserPromptOpened
  { context :: BrowsingContext,
    handler :: UserPromptHandlerType,
    message :: Text,
    promptType :: UserPromptType,
    defaultValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserPromptOpened where
  parseJSON :: Value -> Parser UserPromptOpened
  parseJSON = promptParser

newtype Navigation = MkNavigation
  { navigationId :: Text
  }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON)

instance ToJSON TraverseHistory

instance ToJSON PrintMargin

instance ToJSON PrintPage

instance ToJSON Viewport

instance ToJSON UserPromptClosed

instance ToJSON UserPromptOpened

instance ToJSON Navigation
