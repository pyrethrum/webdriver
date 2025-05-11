module WebDriverPreCore.BiDi.Browser where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Bool (..), Eq (..), Int, Show (..), Maybe)



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
  | CreateUserContext BrowserCreateUserContext
  | GetClientWindows
  | GetUserContexts
  | RemoveUserContext BrowserRemoveUserContext
  | SetClientWindowState BrowserSetClientWindowState
  deriving (Show, Eq, Generic)

newtype BrowserClientWindow = MkBrowserClientWindow Text
  deriving (Show, Eq, Generic)

data BrowserClientWindowInfo = MkBrowserClientWindowInfo
  { active :: Bool,
    clientWindow :: BrowserClientWindow,
    height :: Int,
    state :: BrowserClientWindowState,
    width :: Int,
    x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Generic)

data BrowserClientWindowState
  = WindowFullscreen
  | WindowMaximized
  | WindowMinimized
  | WindowNormal
  deriving (Show, Eq, Generic)

newtype BrowserUserContext = MkBrowserUserContext Text
  deriving (Show, Eq, Generic)

newtype BrowserUserContextInfo = MkBrowserUserContextInfo
  { userContext :: BrowserUserContext
  }
  deriving (Show, Eq, Generic)

newtype BrowserCreateUserContext = MkBrowserCreateUserContext
  { acceptInsecureCerts :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

newtype BrowserRemoveUserContext = MkBrowserRemoveUserContext
  { userContext :: BrowserUserContext
  }
  deriving (Show, Eq, Generic)

data BrowserSetClientWindowState = MkBrowserSetClientWindowState
  { clientWindow :: BrowserClientWindow,
    windowState :: BrowserWindowState
  }
  deriving (Show, Eq, Generic)

data BrowserWindowState
  = BrowserClientWindowNamedState BrowserNamedState
  | BrowserClientWindowRectState BrowserRectState
  deriving (Show, Eq, Generic)

data BrowserNamedState
  = NamedFullscreen
  | NamedMaximized
  | NamedMinimized
  deriving (Show, Eq, Generic)

data BrowserRectState = MkBrowserRectState
  { state :: BrowserNormalState,
    width :: Maybe Int,
    height :: Maybe Int,
    x :: Maybe Int,
    y :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data BrowserNormalState = NormalState
  deriving (Show, Eq, Generic)

-- ######### Local #########

data BrowserResult
  = CreateUserContextResult BrowserUserContextInfo
  | GetClientWindowsResult GetClientWindowsResult
  | GetUserContextsResult GetUserContextsResult
  deriving (Show, Eq, Generic)

newtype GetClientWindowsResult = MkGetClientWindowsResult
  { clientWindows :: [BrowserClientWindowInfo]
  }
  deriving (Show, Eq, Generic)

newtype GetUserContextsResult = MkGetUserContextsResult
  { userContexts :: [BrowserUserContextInfo]
  }
  deriving (Show, Eq, Generic)