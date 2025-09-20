module WebDriverPreCore.BiDi.Browser
  ( ClientWindow (..),
    ClientWindowInfo (..),
    ClientWindowState (..),
    CreateUserContext (..),
    GetClientWindowsResult (..),
    GetUserContextsResult (..),
    NamedState (..),
    NormalState (..),
    RectState (..),
    RemoveUserContext (..),
    SetClientWindowState (..),
    WindowState (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Capabilities (ProxyConfiguration, UserPromptHandler)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, fromJSONCamelCase, opt)
import Prelude (Bool (..), Eq (..), Int, Maybe, Show, ($))
import WebDriverPreCore.BiDi.CoreTypes (UserContext)
import Data.Aeson.Types (Parser)

{-
create types to represent the remote and local ends for browser:

1. preface singleton data constructors (ie the constructor for types with only one type constructor) with Mk
2. use newtypes where possible
3. ordering - order types such that types that are used by a type are declared immediately below that type in the order they are used
4. derive Show, Eq and Generic for all types
5. use the cddl in this file remote first under the -- ######### Remote ######### header
then local under the -- ######### Local ######### header

-}

-- ######### Remote #########

newtype ClientWindow = MkClientWindow Text
  deriving (Show, Eq, Generic)

instance FromJSON ClientWindow

instance ToJSON ClientWindow

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
  toJSON MkCreateUserContext {insecureCerts, proxy, unhandledPromptBehavior} = object $
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

instance ToJSON SetClientWindowState

data WindowState
  = ClientWindowNamedState NamedState
  | ClientWindowRectState RectState
  deriving (Show, Eq, Generic)

instance FromJSON WindowState

instance ToJSON WindowState where
  toJSON = enumCamelCase

data NamedState
  = NamedFullscreen
  | NamedMaximized
  | NamedMinimized
  deriving (Show, Eq, Generic)

instance FromJSON NamedState

instance ToJSON NamedState where
  toJSON :: NamedState -> Value
  toJSON = enumCamelCase

data RectState = MkRectState
  { state :: NormalState,
    width :: Maybe Int,
    height :: Maybe Int,
    x :: Maybe Int,
    y :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON RectState

instance ToJSON RectState

data NormalState = NormalState
  deriving (Show, Eq, Generic)

instance FromJSON NormalState

instance ToJSON NormalState

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