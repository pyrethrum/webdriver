module WebDriverPreCore.BiDi.Browser
  ( ClientWindowInfo (..),
    ClientWindowState (..),
    CreateUserContext (..),
    DownloadBehaviour (..),
    GetClientWindowsResult (..),
    GetUserContextsResult (..),
    NamedState (..),
    RectState (..),
    RemoveUserContext (..),
    SetClientWindowState (..),
    SetDownloadBehavior (..),
    WindowState (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=), KeyValue)
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Capabilities (ProxyConfiguration, UserPromptHandler)
import WebDriverPreCore.BiDi.CoreTypes (ClientWindow, UserContext)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, fromJSONCamelCase, opt)
import Prelude (Bool (..), Eq (..), Int, Maybe, Show, ($), (.), (<>), Applicative (..), MonadFail (..))

-- ######### Remote #########

data ClientWindowInfo = MkClientWindowInfo
  { active :: Bool,
    clientWindow :: ClientWindow,
    height :: Int,
    state :: ClientWindowState,
    width :: Int,
    x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ClientWindowInfo

instance ToJSON ClientWindowInfo

data ClientWindowState
  = Fullscreen
  | Maximized
  | Minimized
  | Normal
  deriving (Show, Eq, Generic)

instance FromJSON ClientWindowState where
  parseJSON :: Value -> Parser ClientWindowState
  parseJSON = fromJSONCamelCase

instance ToJSON ClientWindowState where
  toJSON :: ClientWindowState -> Value
  toJSON = enumCamelCase

data CreateUserContext = MkCreateUserContext
  { -- renamed from acceptInsecureCerts to insecureCerts to avoid name collision with Capabilities
    insecureCerts :: Maybe Bool,
    proxy :: Maybe ProxyConfiguration,
    unhandledPromptBehavior :: Maybe UserPromptHandler
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateUserContext where
  toJSON :: CreateUserContext -> Value
  toJSON MkCreateUserContext {insecureCerts, proxy, unhandledPromptBehavior} =
    object $
      catMaybes
        [ opt "acceptInsecureCerts" insecureCerts,
          opt "proxy" proxy,
          opt "unhandledPromptBehavior" unhandledPromptBehavior
        ]

newtype RemoveUserContext = MkRemoveUserContext
  { userContext :: UserContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON RemoveUserContext

data SetClientWindowState = MkSetClientWindowState
  { clientWindow :: ClientWindow,
    windowState :: WindowState
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetClientWindowState where
  toJSON :: SetClientWindowState -> Value
  toJSON (MkSetClientWindowState cw ws) =
    case ws of
      ClientWindowNamedState ns ->
        object $ cwProp <> ["state" .= ns]
      ClientWindowRectState rs ->
        object $ cwProp <> ["state" .= "normal"]
          <> recStatePairs rs
    where
      cwProp = ["clientWindow" .= cw]

data WindowState
  = ClientWindowNamedState NamedState
  | ClientWindowRectState RectState
  deriving (Show, Eq, Generic)

instance FromJSON WindowState

instance ToJSON WindowState where
  toJSON :: WindowState -> Value
  toJSON = enumCamelCase

data NamedState
  = FullscreenState
  | MaximizedState
  | MinimizedState
  deriving (Show, Eq, Generic)

instance FromJSON NamedState where
  parseJSON :: Value -> Parser NamedState
  parseJSON = \case
    String "fullscreen" -> pure FullscreenState
    String "maximized" -> pure MaximizedState
    String "minimized" -> pure MinimizedState
    _ -> fail "Expected one of: fullscreen, maximized, minimized"

instance ToJSON NamedState where
  toJSON :: NamedState -> Value
  toJSON = \case
    FullscreenState -> "fullscreen"
    MaximizedState -> "maximized"
    MinimizedState -> "minimized"

data RectState = MkRectState
  { width :: Maybe Int,
    height :: Maybe Int,
    x :: Maybe Int,
    y :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON RectState

instance ToJSON RectState where
  toJSON :: RectState -> Value
  toJSON =
    Object . fromList . recStatePairs


recStatePairs ::  KeyValue e a => RectState -> [a]
recStatePairs MkRectState {width, height, x, y} =
    catMaybes
      [ opt "width" width,
        opt "height" height,
        opt "x" x,
        opt "y" y
      ]

-- SetDownloadBehavior command types

data SetDownloadBehavior = MkSetDownloadBehavior
  { downloadBehavior :: Maybe DownloadBehaviour,
    userContexts :: Maybe [UserContext]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetDownloadBehavior where
  toJSON :: SetDownloadBehavior -> Value
  toJSON MkSetDownloadBehavior {downloadBehavior, userContexts} =
    object $
      ["downloadBehavior" .= downloadBehavior]
        <> catMaybes
          [ opt "userContexts" userContexts
          ]

data DownloadBehaviour
  = AllowedDownload
      { destinationFolder :: Text
      }
  | DeniedDownload
  deriving (Show, Eq, Generic)

instance FromJSON DownloadBehaviour

instance ToJSON DownloadBehaviour where
  toJSON :: DownloadBehaviour -> Value
  toJSON (AllowedDownload destinationFolder) =
    object
      [ "type" .= ("allowed" :: Text),
        "destinationFolder" .= destinationFolder
      ]
  toJSON DeniedDownload =
    object
      [ "type" .= ("denied" :: Text)
      ]

-- ######### Local #########

newtype GetClientWindowsResult = MkGetClientWindowsResult
  { clientWindows :: [ClientWindowInfo]
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetClientWindowsResult

instance ToJSON GetClientWindowsResult

newtype GetUserContextsResult = MkGetUserContextsResult
  { userContexts :: [UserContext]
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetUserContextsResult

instance ToJSON GetUserContextsResult