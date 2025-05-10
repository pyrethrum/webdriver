module WebDriverPreCore.BiDi.BrowsingContext where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, JSUInt, NodeRemoteValue)
import Prelude (Bool, Maybe, Show, Eq)

data Info = Info
  { children :: Maybe [Info], -- null allowed per spec
    clientWindow :: Text,
    context :: BrowsingContext,
    originalOpener :: Maybe BrowsingContext, -- null allowed
    url :: Text,
    userContext :: Text,
    parent :: Maybe BrowsingContext -- null allowed
  }
  deriving (Show, Generic)

instance ToJSON Info

instance FromJSON Info

-- Locator types
data Locator
  = AccessibilityLocator
      { typ :: Text, -- "accessibility"
        name :: Maybe Text,
        role :: Maybe Text
      }
  | CssLocator
      { typ :: Text, -- "css"
        value :: Text
      }
  | ContextLocator
      { typ :: Text, -- "context"
        context :: BrowsingContext
      }
  | InnerTextLocator
      { typ :: Text, -- "innerText"
        value :: Text,
        ignoreCase :: Maybe Bool,
        matchType :: Maybe Text,
        maxDepth :: Maybe JSUInt
      }
  | XPathLocator
      { typ :: Text, -- "xpath"
        value :: Text
      }
  deriving (Show, Generic)

-- Result types
data BrowsingContextResult
  = CaptureScreenshotResult
      { screenShot :: Text -- 'data'
      }
  | CreateResult {context :: BrowsingContext}
  | GetTreeResult {contexts :: [Info]}
  | LocateNodesResult {nodes :: [NodeRemoteValue]}
  | NavigateResult
      { navigation :: Maybe Text, -- null allowed
        url :: Text
      }
  | PrintResult
      { printout :: Text -- deata
      }
  | TraverseHistoryResult (Map.Map Text Value) -- EmptyResult with Extensible
  deriving (Show, Generic)

instance ToJSON BrowsingContextResult


-- | Navigation reference 
newtype Navigation = MkNavigation Text
  deriving (Show, Eq, Generic)


{- 
Note [Put touchable variables on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket #10009, a very nasty example:

    f :: (UnF (F b) ~ b) => F b -> ()

    g :: forall a. (UnF (F a) ~ a) => a -> ()
    g _ = f (undefined :: F a)

For g we get [G]  g1 : UnF (F a) ~ a
             [W] w1 : UnF (F beta) ~ beta
             [W] w2 : F a ~ F beta
-}


--   toJSON =
--     genericToJSON
--       defaultOptions
--         { fieldLabelModifier = \case
--             "data_" -> "data"
--             x -> x
--         }

instance FromJSON BrowsingContextResult

--   parseJSON =
--     genericParseJSON
--       defaultOptions
--         { fieldLabelModifier = \case
--             "data" -> "data_"
--             x -> x
--         }

-- Event types
data BrowsingContextEvent
  = ContextCreated {method :: Text, params :: Info}
  | ContextDestroyed {method :: Text, params :: Info}
  | DownloadWillBegin
      { method :: Text,
        suggestedFilename :: Text,
        baseNavigationInfo :: NavigationInfo
      }
  | FragmentNavigated NavigationEvent
  | HistoryUpdated
      { method :: Text,
        context :: BrowsingContext,
        url :: Text
      }
  | Load NavigationEvent
  | NavigationAborted NavigationEvent
  | NavigationCommitted NavigationEvent
  | NavigationFailed NavigationEvent
  | NavigationStarted NavigationEvent
  | UserPromptClosed
      { method :: Text,
        context :: BrowsingContext,
        accepted :: Bool,
        typ :: Text, -- "alert", "beforeunload", etc.
        userText :: Maybe Text
      }
  | UserPromptOpened {
      method :: Text,
        context :: BrowsingContext,
        handler :: Text, -- "accept", "dismiss", etc.
        message :: Text,
        typ :: Text,
        defaultValue :: Maybe Text
      }

  deriving (Show, Generic)

data NavigationEvent = MkNavigationEvent
  { method :: Text,
    navigationInfo :: NavigationInfo
  }
  deriving (Show, Generic)


data NavigationInfo = MkNavigationInfo
  { navigation :: Maybe Text, -- null allowed
    url :: Text,
    timestamp :: JSUInt,
    userContext :: Text,
    clientWindow :: Text
  }
  deriving (Show, Generic)


