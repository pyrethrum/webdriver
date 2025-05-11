module WebDriverPreCore.BiDi.Input where

import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext qualified as BrowsingContext
import WebDriverPreCore.BiDi.Script qualified as Script
import Prelude (Show, Eq, Maybe, Int, Double, Bool)

-- Remote Types for Input module

-- Command types
data InputCommand
  = InputPerformActions PerformActions
  | InputReleaseActions ReleaseActions
  | InputSetFiles SetFiles
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
  = ViewportOrigin
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

data FileDialogInfo = MkFileDialogInfo
  { context :: BrowsingContext.BrowsingContextId,
    element :: Maybe Script.SharedReference,
    multiple :: Bool
  }
  deriving (Show, Eq, Generic)

{-

Remote cddl

InputCommand = (
  input.PerformActions //
  input.ReleaseActions //
  input.SetFiles
)

input.ElementOrigin = {
  type: "element",
  element: script.SharedReference
}

input.PerformActions = (
  method: "input.performActions",
  params: input.PerformActions
)

input.PerformActions = {
  context: browsingContext.BrowsingContext,
  actions: [*input.SourceActions]
}

input.SourceActions = (
  input.NoneSourceActions /
  input.KeySourceActions /
  input.PointerSourceActions /
  input.WheelSourceActions
)

input.NoneSourceActions = {
  type: "none",
  id: text,
  actions: [*input.NoneSourceAction]
}

input.NoneSourceAction = input.PauseAction

input.KeySourceActions = {
  type: "key",
  id: text,
  actions: [*input.KeySourceAction]
}

input.KeySourceAction = (
  input.PauseAction /
  input.KeyDownAction /
  input.KeyUpAction
)

input.PointerSourceActions = {
  type: "pointer",
  id: text,
  ? : input.Pointer,
  actions: [*input.PointerSourceAction]
}

input.PointerType = "mouse" / "pen" / "touch"

input.Pointer = {
  ? pointerType: input.PointerType .default "mouse"
}

input.PointerSourceAction = (
  input.PauseAction /
  input.PointerDownAction /
  input.PointerUpAction /
  input.PointerMoveAction
)

input.WheelSourceActions = {
  type: "wheel",
  id: text,
  actions: [*input.WheelSourceAction]
}

input.WheelSourceAction = (
  input.PauseAction /
  input.WheelScrollAction
)

input.PauseAction = {
  type: "pause",
  ? duration: js-uint
}

input.KeyDownAction = {
  type: "keyDown",
  value: text
}

input.KeyUpAction = {
  type: "keyUp",
  value: text
}

input.PointerUpAction = {
  type: "pointerUp",
  button: js-uint,
}

input.PointerDownAction = {
  type: "pointerDown",
  button: js-uint,
  input.PointerCommonProperties
}

input.PointerMoveAction = {
  type: "pointerMove",
  x: float,
  y: float,
  ? duration: js-uint,
  ? origin: input.Origin,
  input.PointerCommonProperties
}

input.WheelScrollAction = {
  type: "scroll",
  x: js-int,
  y: js-int,
  deltaX: js-int,
  deltaY: js-int,
  ? duration: js-uint,
  ? origin: input.Origin .default "viewport",
}

input.PointerCommonProperties = (
  ? width: js-uint .default 1,
  ? height: js-uint .default 1,
  ? pressure: float .default 0.0,
  ? tangentialPressure: float .default 0.0,
  ? twist: (0..359) .default 0,
  ; 0 .. Math.PI / 2
  ? altitudeAngle: (0.0..1.5707963267948966) .default 0.0,
  ; 0 .. 2 * Math.PI
  ? azimuthAngle: (0.0..6.283185307179586) .default 0.0,
)

input.Origin = "viewport" / "pointer" / input.ElementOrigin

input.ReleaseActions = (
  method: "input.releaseActions",
  params: input.ReleaseActions
)

input.ReleaseActions = {
  context: browsingContext.BrowsingContext,
}

input.SetFiles = (
  method: "input.setFiles",
  params: input.SetFiles
)

input.SetFiles = {
  context: browsingContext.BrowsingContext,
  element: script.SharedReference,
  files: [*text]
}

input.FileDialogOpened = (
   method: "input.fileDialogOpened",
   params: input.FileDialogInfo
)

input.FileDialogInfo = {
   context: browsingContext.BrowsingContext,
   ? element: script.SharedReference,
   multiple: bool,
}

-}
-- ######### Remote #########

{-
loacal cddl

InputEvent = (
  input.FileDialogOpened
)

input.FileDialogOpened = (
   method: "input.fileDialogOpened",
   params: input.FileDialogInfo
)

input.FileDialogInfo = {
   context: browsingContext.BrowsingContext,
   ? element: script.SharedReference,
   multiple: bool,
}
 -}

-- ######### Local #########