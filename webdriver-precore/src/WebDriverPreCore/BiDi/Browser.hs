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
import WebDriverPreCore.BiDi.Session (ProxyConfiguration, UserPromptHandler)

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

newtype ClientWindow = MkClientWindow Text
  deriving (Show, Eq, Generic)

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

data ClientWindowState
  = WindowFullscreen
  | WindowMaximized
  | WindowMinimized
  | WindowNormal
  deriving (Show, Eq, Generic)

newtype UserContext = MkUserContext Text
  deriving (Show, Eq, Generic)

newtype UserContextInfo = MkUserContextInfo
  { userContext :: UserContext
  }
  deriving (Show, Eq, Generic)

data CreateUserContext = MkCreateUserContext
  { acceptInsecureCerts :: Maybe Bool,
    proxy :: Maybe ProxyConfiguration,
    unhandledPromptBehavior :: Maybe UserPromptHandler
  }
  deriving (Show, Eq, Generic)

newtype RemoveUserContext = MkRemoveUserContext
  { userContext :: UserContext
  }
  deriving (Show, Eq, Generic)

data SetClientWindowState = MkSetClientWindowState
  { clientWindow :: ClientWindow,
    windowState :: WindowState
  }
  deriving (Show, Eq, Generic)

data WindowState
  = ClientWindowNamedState NamedState
  | ClientWindowRectState RectState
  deriving (Show, Eq, Generic)

data NamedState
  = NamedFullscreen
  | NamedMaximized
  | NamedMinimized
  deriving (Show, Eq, Generic)

data RectState = MkRectState
  { state :: NormalState,
    width :: Maybe Int,
    height :: Maybe Int,
    x :: Maybe Int,
    y :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data NormalState = NormalState
  deriving (Show, Eq, Generic)

-- ######### Local #########

data BrowserResult
  = CreateUserContextResult UserContextInfo
  | GetClientWindowsResult GetClientWindowsResult
  | GetUserContextsResult GetUserContextsResult
  deriving (Show, Eq, Generic)

newtype GetClientWindowsResult = MkGetClientWindowsResult
  { clientWindows :: [ClientWindowInfo]
  }
  deriving (Show, Eq, Generic)

newtype GetUserContextsResult = MkGetUserContextsResult
  { userContexts :: [UserContextInfo]
  }
  deriving (Show, Eq, Generic)