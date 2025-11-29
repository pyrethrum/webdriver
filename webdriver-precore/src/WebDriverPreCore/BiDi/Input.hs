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
    WheelScrollAction (..),
    PointerCommonProperties (..),
    Origin (..),
    ReleaseActions (..),
    SetFiles (..),
    FileDialogOpened (..),
    FileDialogInfo (..),
    Pointer (..),
    PointerType (..),
  )
where

import Data.Aeson (ToJSON (..), Value (Object), object, (.=), FromJSON (..))
import Data.Aeson.KeyMap qualified
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Script qualified as Script
import WebDriverPreCore.Internal.AesonUtils (toJSONOmitNothing, parseJSONOmitNothing)
import Prelude (Bool, Double, Eq, Int, Maybe, Show, ($), (++))
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext(..))
import Data.Aeson.Types (Parser)
import WebDriverPreCore.BiDi.Script (SharedReference)

-- ######### Local #########

data PerformActions = MkPerformActions
  { context :: BrowsingContext,
    actions :: [SourceActions]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PerformActions where
  toJSON :: PerformActions -> Value
  toJSON (MkPerformActions context actions) =
    object
      [ "context" .= context,
        "actions" .= actions
      ]

data SourceActions
  = NoneSourceActions NoneSourceActions
  | KeySourceActions KeySourceActions
  | PointerSourceActions PointerSourceActions
  | WheelSourceActions WheelSourceActions
  deriving (Show, Eq, Generic)

instance ToJSON SourceActions where
  toJSON :: SourceActions -> Value
  toJSON = \case
    NoneSourceActions noneSourceActions -> toJSON noneSourceActions
    KeySourceActions keySourceActions -> toJSON keySourceActions
    PointerSourceActions pointerSourceActions -> toJSON pointerSourceActions
    WheelSourceActions wheelSourceActions -> toJSON wheelSourceActions

data NoneSourceActions = MkNoneSourceActions
  { noneId :: Text,
    noneActions :: [PauseAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON NoneSourceActions where
  toJSON :: NoneSourceActions -> Value
  toJSON (MkNoneSourceActions noneId noneActions) =
    object
      [ "type" .= "none",
        "id" .= noneId,
        "actions" .= noneActions
      ]

data KeySourceActions = MkKeySourceActions
  { keyId :: Text,
    keyActions :: [KeySourceAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON KeySourceActions where
  toJSON :: KeySourceActions -> Value
  toJSON (MkKeySourceActions keyId keyActions) =
    object
      [ "type" .= "key",
        "id" .= keyId,
        "actions" .= keyActions
      ]

data KeySourceAction
  = KeyPause
      { duration :: Maybe Int
      }
  | KeyDown
      { value :: Text
      }
  | KeyUp
      { value :: Text
      }
  deriving (Show, Eq, Generic)

instance ToJSON KeySourceAction where
  toJSON :: KeySourceAction -> Value
  toJSON = \case
    KeyPause {duration} ->
      object
        [ "type" .= "pause",
          "duration" .= duration
        ]
    KeyDown {value} ->
      object
        [ "type" .= "keyDown",
          "value" .= value
        ]
    KeyUp {value} ->
      object
        [ "type" .= "keyUp",
          "value" .= value
        ]

data PointerSourceActions = MkPointerSourceActions
  { pointerId :: Text,
    pointer :: Maybe Pointer,
    pointerActions :: [PointerSourceAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PointerSourceActions where
  toJSON :: PointerSourceActions -> Value
  toJSON (MkPointerSourceActions pointerId pointer pointerActions) =
    object
      [ "type" .= "pointer",
        "id" .= pointerId,
        "parameters" .= pointer,
        "actions" .= pointerActions
      ]

data PointerType = MousePointer | PenPointer | TouchPointer
  deriving (Show, Eq, Generic)

instance ToJSON PointerType where
  toJSON :: PointerType -> Value
  toJSON = \case
    MousePointer -> "mouse"
    PenPointer -> "pen"
    TouchPointer -> "touch"

data Pointer = MkPointer
  { pointerType :: Maybe PointerType -- default "mouse"
  }
  deriving (Show, Eq, Generic)

instance ToJSON Pointer where
  toJSON :: Pointer -> Value
  toJSON MkPointer {pointerType} =
    object
      [ "pointerType" .= fromMaybe MousePointer pointerType
      ]

data PointerSourceAction
  = Pause
      { duration :: Maybe Int
      }
  | PointerDown
      { button :: Int,
        pointerCommonProperties :: PointerCommonProperties
      }
  | PointerUp
      { button :: Int
      }
  | PointerMove
      { x :: Double,
        y :: Double,
        duration :: Maybe Int,
        origin :: Maybe Origin,
        pointerCommonProperties :: PointerCommonProperties
      }
  deriving (Show, Eq, Generic)

instance ToJSON PointerSourceAction where
  toJSON :: PointerSourceAction -> Value
  toJSON = \case
    Pause {duration} ->
      object
        [ "type" .= "pause",
          "duration" .= duration
        ]
    PointerDown {button, pointerCommonProperties} ->
      case toJSON pointerCommonProperties of
        Object props ->
          object $
            [ "type" .= "pointerDown",
              "button" .= button
            ] ++ [(k, v) | (k, v) <- Data.Aeson.KeyMap.toList props]
        _ -> 
          object
            [ "type" .= "pointerDown",
              "button" .= button
            ]
    PointerUp {button} ->
      object
        [ "type" .= "pointerUp",
          "button" .= button
        ]
    PointerMove {x, y, duration, origin, pointerCommonProperties} ->
      case toJSON pointerCommonProperties of
        Object props ->
          object $
            [ "type" .= "pointerMove",
              "x" .= x,
              "y" .= y,
              "duration" .= duration,
              "origin" .= origin
            ] ++ [(k, v) | (k, v) <- Data.Aeson.KeyMap.toList props]
        _ -> 
          object
            [ "type" .= "pointerMove",
              "x" .= x,
              "y" .= y,
              "duration" .= duration,
              "origin" .= origin
            ]

data WheelSourceActions = MkWheelSourceActions
  { wheelId :: Text,
    wheelActions :: [WheelSourceAction]
  }
  deriving (Show, Eq, Generic)

instance ToJSON WheelSourceActions where
  toJSON :: WheelSourceActions -> Value
  toJSON (MkWheelSourceActions wheelId wheelActions) =
    object
      [ "type" .= "wheel",
        "id" .= wheelId,
        "actions" .= wheelActions
      ]

data WheelSourceAction
  = WheelPauseAction PauseAction
  | WheelScrollAction WheelScrollAction
  deriving (Show, Eq, Generic)

instance ToJSON WheelSourceAction where
  toJSON :: WheelSourceAction -> Value
  toJSON = \case
    WheelPauseAction wheelPauseAction -> toJSON wheelPauseAction
    WheelScrollAction wheelScrollAction -> toJSON wheelScrollAction

newtype PauseAction = MkPauseAction
  { duration :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PauseAction where
  toJSON :: PauseAction -> Value
  toJSON (MkPauseAction duration) =
    object
      [ "type" .= "pause",
        "duration" .= duration
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
  | ElementOrigin Script.SharedReference
  deriving (Show, Eq, Generic)

instance ToJSON Origin where
  toJSON :: Origin -> Value
  toJSON = \case
    ViewportOriginPointerType -> "viewport"
    PointerOrigin -> "pointer"
    ElementOrigin element ->
      object
        [ "type" .= "element",
          "element" .= element
        ]

-- ReleaseActions
newtype ReleaseActions = MkReleaseActions
  { context :: BrowsingContext
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReleaseActions

data SetFiles = MkSetFiles
  { context :: BrowsingContext,
    element :: Script.SharedReference,
    files :: [Text]
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetFiles

data FileDialogOpened = MkFileDialogOpened
  { params :: FileDialogInfo
  }
  deriving (Show, Eq, Generic)

instance FromJSON FileDialogOpened

-- ######### Local #########

data FileDialogInfo = MkFileDialogInfo
  { context :: BrowsingContext,
    element :: Maybe SharedReference,
    multiple :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON FileDialogInfo where
  parseJSON :: Value -> Parser FileDialogInfo
  parseJSON = parseJSONOmitNothing

instance ToJSON FileDialogInfo
