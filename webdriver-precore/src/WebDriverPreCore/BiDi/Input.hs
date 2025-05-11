module WebDriverPreCore.BiDi.Log  where

{-
create types to represent the remote  end for module Input where:

1. preface singleton data constructors (ie the constructor for types with only one type constructor) with Mk
2. use newtypes where possible
3. ordering - order types such that types that are used by a type are declared immediately below that type in the order they are used
4. derive Show, Eq and Generic for all types
5. use Text rather than String
6. use the cddl in this file. Create types for remote first  under the -- ######### Remote ######### header then under 
the -- ######### Local ######### header
-}


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
  params: input.PerformActionsParameters
)

input.PerformActionsParameters = {
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
  ? parameters: input.PointerParameters,
  actions: [*input.PointerSourceAction]
}

input.PointerType = "mouse" / "pen" / "touch"

input.PointerParameters = {
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
  params: input.ReleaseActionsParameters
)

input.ReleaseActionsParameters = {
  context: browsingContext.BrowsingContext,
}

input.SetFiles = (
  method: "input.setFiles",
  params: input.SetFilesParameters
)

input.SetFilesParameters = {
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