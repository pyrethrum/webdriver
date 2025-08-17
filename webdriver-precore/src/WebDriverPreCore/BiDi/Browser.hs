module WebDriverPreCore.BiDi.Browser
  ( BrowserCommand (..),
    BrowserResult (..),
    ClientWindow (..),
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
    UserContext (..),
    UserContextInfo (..),
    WindowState (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Bool (..), Eq (..), Int, Maybe, Show (..))
import WebDriverPreCore.BiDi.Capabilities (UserPromptHandler, ProxyConfiguration)
import WebDriverPreCore.BiDi.CoreTypes (EmptyResult)
import Data.Aeson (FromJSON, ToJSON (..))
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase)

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

data BrowserCommand
  = Close
  | CreateUserContext CreateUserContext
  | GetClientWindows
  | GetUserContexts
  | RemoveUserContext RemoveUserContext
  | SetClientWindowState SetClientWindowState
  deriving (Show, Eq, Generic)

instance ToJSON BrowserCommand where
  toJSON = enumCamelCase

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
  = WindowFullscreen
  | WindowMaximized
  | WindowMinimized
  | WindowNormal
  deriving (Show, Eq, Generic)

instance FromJSON ClientWindowState

instance ToJSON ClientWindowState where
  toJSON = enumCamelCase

newtype UserContext = MkUserContext Text
  deriving (Show, Eq, Generic)

instance FromJSON UserContext

instance ToJSON UserContext

newtype UserContextInfo = MkUserContextInfo
  { userContext :: UserContext
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserContextInfo

instance ToJSON UserContextInfo

data CreateUserContext = MkCreateUserContext
  { acceptInsecureCerts :: Maybe Bool,
    proxy :: Maybe ProxyConfiguration,
    unhandledPromptBehavior :: Maybe UserPromptHandler
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateUserContext

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

-- Note includes more than spec spec seems inconsistant
data BrowserResult
  = CreateUserContextResult UserContextInfo
  | GetClientWindowsResult GetClientWindowsResult
  | GetUserContextsResult GetUserContextsResult
  | CloseResult EmptyResult
  | RemoveUserContextResult EmptyResult
  | SetClientWindowStateResult ClientWindowInfo 
  deriving (Show, Eq, Generic)

instance ToJSON BrowserResult where
  toJSON = enumCamelCase

newtype GetClientWindowsResult = MkGetClientWindowsResult
  { clientWindows :: [ClientWindowInfo]
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetClientWindowsResult

instance ToJSON GetClientWindowsResult

newtype GetUserContextsResult = MkGetUserContextsResult
  { userContexts :: [UserContextInfo]
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetUserContextsResult

instance ToJSON GetUserContextsResult