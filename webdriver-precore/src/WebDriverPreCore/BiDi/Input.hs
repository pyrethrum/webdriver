module WebDriverPreCore.BiDi.Input
  ( PerformActions (..),
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
    PointerType (..),
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext qualified as BrowsingContext
import WebDriverPreCore.BiDi.Script qualified as Script
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase, toJSONOmitNothing)
import Prelude (Bool, Double, Eq, Int, Maybe, Show)

-- ######### Local #########

-- Element Origin
data ElementOrigin = MkElementOrigin
  { element :: Script.SharedReference
  }
  deriving (Show, Eq, Generic)

instance ToJSON ElementOrigin where
  toJSON :: ElementOrigin -> Value
  toJSON (MkElementOrigin element) =
    object
      [ "elementType" .= "element",
        "element" .= element
      ]

data PerformActions = MkPerformActions
  { context :: BrowsingContext.BrowsingContextId,
    actions :: [SourceActions]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PerformActions

data SourceActions
  = NoneSourceActions PauseAction
  | KeySourceActions KeySourceActions
  | PointerSourceActions PointerSourceActions
  | WheelSourceActions WheelSourceActions
  deriving (Show, Eq, Generic)

instance ToJSON SourceActions

data NoneSourceActions = MkNoneSourceActions
  { noneType :: Text, -- will be "none"
    noneId :: Text,
    noneActions :: [PauseAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON NoneSourceActions

data KeySourceActions = MkKeySourceActions
  { keyType :: Text, -- will be "key"
    keyId :: Text,
    keyActions :: [KeySourceAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON KeySourceActions

data KeySourceAction
  = KeyPauseAction PauseAction
  | KeyDownAction KeyDownAction
  | KeyUpAction KeyUpAction
  deriving (Show, Eq, Generic)

instance ToJSON KeySourceAction where
  toJSON = enumCamelCase

data PointerSourceActions = MkPointerSourceActions
  { pointerType :: Text, -- will be "pointer"
    pointerId :: Text,
    pointer :: Maybe Pointer,
    pointerActions :: [PointerSourceAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PointerSourceActions

data PointerType = MousePointer | PenPointer | TouchPointer
  deriving (Show, Eq, Generic)

instance ToJSON PointerType where
  toJSON = enumCamelCase

data Pointer = MkPointer
  { pointerType :: Maybe PointerType -- default "mouse"
  }
  deriving (Show, Eq, Generic)

instance ToJSON Pointer

data PointerSourceAction
  = PointerPauseAction PauseAction
  | PointerDownAction PointerDownAction
  | PointerUpAction PointerUpAction
  | PointerMoveAction PointerMoveAction
  deriving (Show, Eq, Generic)

instance ToJSON PointerSourceAction where
  toJSON = enumCamelCase

data WheelSourceActions = MkWheelSourceActions
  { wheelType :: Text, -- will be "wheel"
    wheelId :: Text,
    wheelActions :: [WheelSourceAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON WheelSourceActions

data WheelSourceAction
  = WheelPauseAction PauseAction
  | WheelScrollAction WheelScrollAction
  deriving (Show, Eq, Generic)

instance ToJSON WheelSourceAction where
  toJSON = enumCamelCase

newtype PauseAction = MkPauseAction
  { 
    duration :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PauseAction  where
  toJSON :: PauseAction -> Value
  toJSON (MkPauseAction duration) =
    object
      [ "type" .= "pause",
        "duration" .= duration
      ]

newtype KeyDownAction = MkKeyDownAction
  { value :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON KeyDownAction where
  toJSON :: KeyDownAction -> Value
  toJSON (MkKeyDownAction value) =
    object
      [ "type" .= "keyDown",
        "value" .= value
      ]

newtype KeyUpAction = MkKeyUpAction
  { value :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON KeyUpAction where
  toJSON :: KeyUpAction -> Value
  toJSON (MkKeyUpAction value) =
    object
      [ "type" .= "keyUp",
        "value" .= value
      ]

newtype PointerUpAction = MkPointerUpAction
  { button :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PointerUpAction where
  toJSON :: PointerUpAction -> Value
  toJSON (MkPointerUpAction button) =
    object
      [ "type" .= "pointerUp",
        "button" .= button
      ]

data PointerDownAction = MkPointerDownAction
  { button :: Int,
    pointerCommonProperties :: PointerCommonProperties
  }
  deriving (Show, Eq, Generic)

instance ToJSON PointerDownAction where
  toJSON :: PointerDownAction -> Value
  toJSON (MkPointerDownAction button pointerCommonProperties) =
    object
      [ "type" .= "pointerDown",
        "button" .= button,
        "pointerCommonProperties" .= pointerCommonProperties
      ]

data PointerMoveAction = MkPointerMoveAction
  { x :: Double,
    y :: Double,
    duration :: Maybe Int,
    origin :: Maybe Origin,
    pointerCommonProperties :: PointerCommonProperties
  }
  deriving (Show, Eq, Generic)

instance ToJSON PointerMoveAction where
  toJSON :: PointerMoveAction -> Value
  toJSON (MkPointerMoveAction x y duration origin pointerCommonProperties) =
    object
      [ "type" .= "pointerMove",
        "x" .= x,
        "y" .= y,
        "duration" .= duration,
        "origin" .= origin,
        "pointerCommonProperties" .= pointerCommonProperties
      ]

data WheelScrollAction = MkWheelScrollAction
  { x :: Int,
    y :: Int,
    deltaX :: Int,
    deltaY :: Int,
    duration :: Maybe Int,
    origin :: Maybe Origin -- default "viewport"
  }
  deriving (Show, Eq, Generic)

instance ToJSON WheelScrollAction where
  toJSON :: WheelScrollAction -> Value
  toJSON (MkWheelScrollAction x y deltaX deltaY duration origin) =
    object
      [ "type" .= "scroll",
        "x" .= x,
        "y" .= y,
        "deltaX" .= deltaX,
        "deltaY" .= deltaY,
        "duration" .= duration,
        "origin" .= origin
      ]

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

instance ToJSON PointerCommonProperties where
  toJSON :: PointerCommonProperties -> Value
  toJSON = toJSONOmitNothing

data Origin
  = ViewportOriginPointerType
  | PointerOrigin
  | ElementOriginRef ElementOrigin
  deriving (Show, Eq, Generic)

instance ToJSON Origin where
  toJSON :: Origin -> Value
  toJSON = \case
    ViewportOriginPointerType -> "viewport"
    PointerOrigin -> "pointer"
    ElementOriginRef elementOrigin -> toJSON elementOrigin

-- ReleaseActions
newtype ReleaseActions = MkReleaseActions
  { context :: BrowsingContext.BrowsingContextId
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReleaseActions

data SetFiles = MkSetFiles
  { context :: BrowsingContext.BrowsingContextId,
    element :: Script.SharedReference,
    files :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetFiles

data FileDialogOpened = MkFileDialogOpened
  { params :: FileDialogInfo
  }
  deriving (Show, Eq, Generic)

instance ToJSON FileDialogOpened

-- ######### Local #########

data FileDialogInfo = MkFileDialogInfo
  { context :: BrowsingContext.BrowsingContextId,
    element :: Maybe Script.SharedReference,
    multiple :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON FileDialogInfo
