module BiDi.Demos.InputEventDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Data.Maybe (fromJust)
import IOUtils (DemoUtils (..))
import TestData (uploadUrl)
import WebDriverPreCore.BiDi.BrowsingContext (Locator (..))
import WebDriverPreCore.BiDi.CoreTypes qualified as Core
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)

-- Helper function to create default pointer common properties
defaultPointerProps :: PointerCommonProperties
defaultPointerProps =
  MkPointerCommonProperties
    { width = Nothing,
      height = Nothing,
      pressure = Nothing,
      tangentialPressure = Nothing,
      twist = Nothing,
      altitudeAngle = Nothing,
      azimuthAngle = Nothing
    }

{- 
Input Events - Implementation Status:

1. input.fileDialogOpened :: ✗ inputEventFileDialogOpened, inputEventFileDialogOpenedMultiple 
   (not implemented in geckodriver - bugs 1855044, 1855045)
   
Note: The input.fileDialogOpened event is not yet implemented in geckodriver.
See:
- Meta bug: https://bugzilla.mozilla.org/show_bug.cgi?id=1855044
- Implementation bug: https://bugzilla.mozilla.org/show_bug.cgi?id=1855045
- Status: NEW (as of m18 milestone, last updated October 2025)
- WebDriver BiDi spec: https://w3c.github.io/webdriver-bidi/#event-input-fileDialogOpened

The demos below are ready for when the feature is implemented.
-}

-- >>> runDemo inputEventFileDialogOpened
inputEventFileDialogOpened :: BiDiDemo
inputEventFileDialogOpened =
  demo "Input Events - File Dialog Opened (Single File)" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Subscribe to FileDialogOpened event"
      (fileDialogEventFired, waitFileDialogEventFired) <- timeLimitLog InputFileDialogOpened
      subscribeInputFileDialogOpened fileDialogEventFired

      (manyFileDialogEventFired, waitManyFileDialogEventFired) <- timeLimitLog InputFileDialogOpened
      subscribeMany [InputFileDialogOpened] manyFileDialogEventFired

      bc <- newWindowContext utils cmds
      logTxt "Navigating to upload page"
      url <- uploadUrl
      browsingContextNavigate $ MkNavigate bc url Nothing

      logTxt "Locating single file upload input"
      uploadInput <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#singleUpload"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Upload input element" uploadInput

      let MkLocateNodesResult nodes = uploadInput
          uploadInputId :: Core.SharedId
          uploadInputId = case nodes of
            [input] -> fromJust input.sharedId
            _ -> error "Failed to locate single file upload input"

      logTxt "Clicking on file upload input to open file dialog"
      inputPerformActions $
        MkPerformActions
          { context = bc,
            actions =
              [ PointerSourceActions $
                  MkPointerSourceActions
                    { pointerId = "mouse1",
                      pointer = Just $ MkPointer {pointerType = Just MousePointer},
                      pointerActions =
                        [ PointerMove
                            { x = 0,
                              y = 0,
                              duration = Nothing,
                              origin =
                                Just $
                                  ElementOrigin $
                                    MkSharedReference
                                      { sharedId = uploadInputId,
                                        handle = Nothing,
                                        extensions = Nothing
                                      },
                              pointerCommonProperties = defaultPointerProps
                            },
                          PointerDown
                            { button = 0,
                              pointerCommonProperties = defaultPointerProps
                            },
                          PointerUp
                            { button = 0
                            }
                        ]
                    }
              ]
          }

      logTxt "Waiting for file dialog opened events..."
      sequence_
        [ waitFileDialogEventFired,
          waitManyFileDialogEventFired
        ]

