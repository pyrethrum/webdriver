module WebDriverPreCore.BiDi.Input
  ( InputCommand (..),
    PerformActions (..),
    SourceActions (..),
    NoneSourceActions (..),
    KeySourceActions (..),
    KeySourceAction (..),
    PointerSourceActions (..),
    PointerSourceAction (..),
    WheelSourceActions (..),
    WheelSourceAction (..),
    PauseAction (..),
    KeyDownAction (..),
    KeyUpAction (..),
    PointerUpAction (..),
    PointerDownAction (..),
    PointerMoveAction (..),
    WheelScrollAction (..),
    PointerCommonProperties (..),
    Origin (..),
    ReleaseActions (..),
    SetFiles (..),
    FileDialogOpened (..),
    FileDialogInfo (..),
    ElementOrigin (..),
    Pointer (..),
    PointerType (..)
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext qualified as BrowsingContext
import WebDriverPreCore.BiDi.Script qualified as Script
import Prelude (Bool, Double, Eq, Int, Maybe, Show)
import Data.Aeson (ToJSON (..))
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase)

-- ######### Local #########

-- Command types
data InputCommand = 
   InputPerformActions PerformActions | 
   InputReleaseActions ReleaseActions | 
   InputSetFiles SetFiles
  deriving (Show, Eq, Generic)

-- Element Origin
data ElementOrigin = MkElementOrigin
  { elementType :: Text, -- will be "element"
    element :: Script.SharedReference
  }
  deriving (Show, Eq, Generic)

data PerformActions = MkPerformActions
  { context :: BrowsingContext.BrowsingContextId,
    actions :: [SourceActions]
  }
  deriving (Show, Eq, Generic)

data SourceActions
  = NoneSourceActions PauseAction
  | KeySourceActions KeySourceActions
  | PointerSourceActions PointerSourceActions
  | WheelSourceActions WheelSourceActions
  deriving (Show, Eq, Generic)

data NoneSourceActions = MkNoneSourceActions
  { noneType :: Text, -- will be "none"
    noneId :: Text,
    noneActions :: [PauseAction]
  }
  deriving (Show, Eq, Generic)

data KeySourceActions = MkKeySourceActions
  { keyType :: Text, -- will be "key"
    keyId :: Text,
    keyActions :: [KeySourceAction]
  }
  deriving (Show, Eq, Generic)

data KeySourceAction
  = KeyPauseAction PauseAction
  | KeyDownAction KeyDownAction
  | KeyUpAction KeyUpAction
  deriving (Show, Eq, Generic)

data PointerSourceActions = MkPointerSourceActions
  { pointerType :: Text, -- will be "pointer"
    pointerId :: Text,
    pointer :: Maybe Pointer,
    pointerActions :: [PointerSourceAction]
  }
  deriving (Show, Eq, Generic)

data PointerType = MousePointer | PenPointer | TouchPointer
  deriving (Show, Eq, Generic)

data Pointer = MkPointer
  { pointerType :: Maybe PointerType -- default "mouse"
  }
  deriving (Show, Eq, Generic)

data PointerSourceAction
  = PointerPauseAction PauseAction
  | PointerDownAction PointerDownAction
  | PointerUpAction PointerUpAction
  | PointerMoveAction PointerMoveAction
  deriving (Show, Eq, Generic)

data WheelSourceActions = MkWheelSourceActions
  { wheelType :: Text, -- will be "wheel"
    wheelId :: Text,
    wheelActions :: [WheelSourceAction]
  }
  deriving (Show, Eq, Generic)

data WheelSourceAction
  = WheelPauseAction PauseAction
  | WheelScrollAction WheelScrollAction
  deriving (Show, Eq, Generic)

data PauseAction = MkPauseAction
  { pauseType :: Text, -- will be "pause"
    duration :: Maybe Int
  }
  deriving (Show, Eq, Generic)

data KeyDownAction = MkKeyDownAction
  { keyDownType :: Text, -- will be "keyDown"
    value :: Text
  }
  deriving (Show, Eq, Generic)

data KeyUpAction = MkKeyUpAction
  { keyUpType :: Text, -- will be "keyUp"
    value :: Text
  }
  deriving (Show, Eq, Generic)

data PointerUpAction = MkPointerUpAction
  { pointerUpType :: Text, -- will be "pointerUp"
    button :: Int
  }
  deriving (Show, Eq, Generic)

data PointerDownAction = MkPointerDownAction
  { pointerDownType :: Text, -- will be "pointerDown"
    button :: Int,
    pointerCommonProperties :: PointerCommonProperties
  }
  deriving (Show, Eq, Generic)

data PointerMoveAction = MkPointerMoveAction
  { pointerMoveType :: Text, -- will be "pointerMove"
    x :: Double,
    y :: Double,
    duration :: Maybe Int,
    origin :: Maybe Origin,
    pointerCommonProperties :: PointerCommonProperties
  }
  deriving (Show, Eq, Generic)

data WheelScrollAction = MkWheelScrollAction
  { scrollType :: Text, -- will be "scroll"
    x :: Int,
    y :: Int,
    deltaX :: Int,
    deltaY :: Int,
    duration :: Maybe Int,
    origin :: Maybe Origin -- default "viewport"
  }
  deriving (Show, Eq, Generic)

data PointerCommonProperties = MkPointerCommonProperties
  { width :: Maybe Int, -- default 1
    height :: Maybe Int, -- default 1
    pressure :: Maybe Double, -- default 0.0
    tangentialPressure :: Maybe Double, -- default 0.0
    twist :: Maybe Int, -- default 0, range 0..359
    altitudeAngle :: Maybe Double, -- default 0.0, range 0..π/2
    azimuthAngle :: Maybe Double -- default 0.0, range 0..2π
  }
  deriving (Show, Eq, Generic)

data Origin
  = ViewportOriginPointerType
  | PointerOrigin
  | ElementOriginRef ElementOrigin
  deriving (Show, Eq, Generic)

-- ReleaseActions
newtype ReleaseActions = MkReleaseActions
  { context :: BrowsingContext.BrowsingContextId
  }
  deriving (Show, Eq, Generic)

data SetFiles = MkSetFiles
  { context :: BrowsingContext.BrowsingContextId,
    element :: Script.SharedReference,
    files :: [Text]
  }
  deriving (Show, Eq, Generic)

data FileDialogOpened = MkFileDialogOpened
  { params :: FileDialogInfo
  }
  deriving (Show, Eq, Generic)

-- ######### Local #########

data FileDialogInfo = MkFileDialogInfo
  { context :: BrowsingContext.BrowsingContextId,
    element :: Maybe Script.SharedReference,
    multiple :: Bool
  }
  deriving (Show, Eq, Generic)

-- ToJSON instances
instance ToJSON InputCommand where
  toJSON = enumCamelCase

instance ToJSON PerformActions
instance ToJSON ReleaseActions
instance ToJSON SetFiles
instance ToJSON SourceActions
instance ToJSON FileDialogOpened
instance ToJSON FileDialogInfo
instance ToJSON ElementOrigin
instance ToJSON NoneSourceActions
instance ToJSON KeySourceActions
instance ToJSON PointerSourceActions
instance ToJSON WheelSourceActions
instance ToJSON Pointer
instance ToJSON PointerCommonProperties

-- All the various action types  
instance ToJSON PauseAction
instance ToJSON KeyDownAction
instance ToJSON KeyUpAction
instance ToJSON PointerUpAction
instance ToJSON PointerDownAction
instance ToJSON PointerMoveAction
instance ToJSON WheelScrollAction

-- Sum type instances with enumCamelCase
instance ToJSON PointerType where
  toJSON = enumCamelCase

instance ToJSON KeySourceAction where
  toJSON = enumCamelCase

instance ToJSON PointerSourceAction where
  toJSON = enumCamelCase

instance ToJSON WheelSourceAction where
  toJSON = enumCamelCase
