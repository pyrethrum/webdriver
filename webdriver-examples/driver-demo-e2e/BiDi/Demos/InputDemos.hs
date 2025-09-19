module BiDi.Demos.InputDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Data.Maybe (fromJust)
import IOUtils (DemoUtils (..))
import TestPages (checkboxesUrl, infiniteScrollUrl, textAreaUrl)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextId (..), Locator (..))
import WebDriverPreCore.BiDi.CoreTypes (NodeRemoteValue (..))
import WebDriverPreCore.BiDi.CoreTypes qualified as Core
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log)

{-
Input Module Commands (3 total):

1. input.performActions - Performs a specified sequence of user input actions ✓
2. input.releaseActions - Resets the input state associated with the current session ✓
3. input.setFiles - Sets the files property of a given input element with type file to a set of file paths ✓

Demo coverage:
- inputKeyboardDemo - Key input actions
- inputPointerDemo - Mouse/pointer actions
- inputWheelDemo - Wheel/scroll actions
- inputCombinedActionsDemo - Multiple action types together
- inputReleaseActionsDemo - Release input state
- inputSetFilesDemo - File upload functionality
-}

-- Helper function to convert BrowsingContext to BrowsingContextId
bcToId :: BrowsingContext -> BrowsingContextId
bcToId (MkBrowsingContext ctx) = MkBrowsingContextId ctx

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

-- >>> runDemo inputKeyboardDemo
inputKeyboardDemo :: BiDiDemo
inputKeyboardDemo =
  demo "Input I - Keyboard Actions" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      fileUrl <- textAreaUrl

      logTxt "Navigate to text area page for keyboard testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = fileUrl, wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Locate the text area 1 field using CSS selector"
      textArea' <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#textArea"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Text area 1 field search result" textArea'
      pause

      -- Extract the element's shared reference for clicking
      let MkLocateNodesResult nodes = textArea'

          textAreaId :: SharedId
          textAreaId = case nodes of
            [textArea] -> fromJust textArea.sharedId
            _ -> error "Failed to locate text area 1"

          clickTextArea1 =
            inputPerformActions $
              MkPerformActions
                { context = bcToId bc,
                  actions =
                    [ PointerSourceActions $
                        MkPointerSourceActions
                          { pointerId = "mouse1",
                            pointer = Just $ MkPointer {pointerType = Just MousePointer},
                            pointerActions =
                              [ PointerMove
                                  { x = 0, -- Relative to element center
                                    y = 0, -- Relative to element center
                                    duration = Just 300,
                                    origin =
                                      Just $
                                        ElementOrigin $
                                          MkSharedReference
                                            { -- use a safe function instead in prod
                                              sharedId = textAreaId,
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

      logTxt "Focus the text area by clicking on it using element reference"
      focusTextArea <- clickTextArea1
      logShow "Focus text area result" focusTextArea
      pause

      logTxt "Test 1: Basic key actions - Type in text area"
      basicKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyPause {duration = Just 100},
                            KeyDown "t",
                            KeyUp "t",
                            KeyDown "o",
                            KeyUp "o",
                            KeyDown "m",
                            KeyUp "m",
                            KeyDown "s",
                            KeyUp "s",
                            KeyDown "m",
                            KeyUp "m",
                            KeyDown "i",
                            KeyUp "i",
                            KeyDown "t",
                            KeyUp "t",
                            KeyDown "h",
                            KeyUp "h"
                          ]
                      }
                ]
            }
      logShow "Basic key actions result" basicKeyActions
      pause

      logTxt "Test 2: Special keys - Tab to text area 2 field and type message"
      specialKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ -- Tab key to move to text area 2 field
                            KeyDown "\xE004", -- Tab
                            KeyUp "\xE004",
                            KeyPause {duration = Just 200},
                            -- Type Hi From BiDi!
                            KeyDown "H",
                            KeyUp "H",
                            KeyDown "i",
                            KeyUp "i",
                            KeyDown " ",
                            KeyUp " ",
                            KeyDown "F",
                            KeyUp "F",
                            KeyDown "r",
                            KeyUp "r",
                            KeyDown "o",
                            KeyUp "o",
                            KeyDown "m",
                            KeyUp "m",
                            KeyDown " ",
                            KeyUp " ",
                            KeyDown "B",
                            KeyUp "B",
                            KeyDown "i",
                            KeyUp "i",
                            KeyDown "D",
                            KeyUp "D",
                            KeyDown "i",
                            KeyUp "i",
                            KeyDown "!",
                            KeyUp "!"
                          ]
                      }
                ]
            }
      logShow "Special key actions result" specialKeyActions
      pause

      logTxt "Test 3: Modifier keys - Ctrl+A to select all in text area 1 field"

      _ <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ -- Shift+Tab to go back to text area 1 field
                            KeyDown "\xE008", -- Shift
                            KeyDown "\xE004", -- Tab
                            KeyUp "\xE004",
                            KeyUp "\xE008",
                            KeyPause {duration = Just 200},
                            -- Ctrl+A to select all
                            KeyDown "\xE009", -- Ctrl
                            KeyDown "a",
                            KeyUp "a",
                            KeyUp "\xE009",
                            KeyPause {duration = Just 200},
                            -- Type new text
                            KeyDown "S",
                            KeyUp "S",
                            KeyDown "e",
                            KeyUp "e",
                            KeyDown "c",
                            KeyUp "c",
                            KeyDown "r",
                            KeyUp "r",
                            KeyDown "e",
                            KeyUp "e",
                            KeyDown "t",
                            KeyUp "t",
                            KeyDown "!",
                            KeyUp "!"
                          ]
                      }
                ]
            }
      logShow "Special key actions result" specialKeyActions
      pause

      logTxt "Test 4: Modifier keys - Ctrl+A to select all in text area 2 field"
      modifierKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ -- Tab to go back to text area 2 field
                            KeyDown "\xE004", -- Tab
                            KeyUp "\xE004",
                            KeyUp "\xE008",
                            KeyPause {duration = Just 200},
                            -- Ctrl+A to select all
                            KeyDown "\xE009", -- Ctrl
                            KeyDown "a",
                            KeyUp "a",
                            KeyUp "\xE009",
                            KeyPause {duration = Just 200},
                            -- Type new text
                            KeyDown "a",
                            KeyUp "a",
                            KeyDown "d",
                            KeyUp "d",
                            KeyDown "m",
                            KeyUp "m",
                            KeyDown "i",
                            KeyUp "i",
                            KeyDown "n",
                            KeyUp "n"
                          ]
                      }
                ]
            }
      logShow "Modifier key actions result" modifierKeyActions
      pause

      closeContext utils cmds bc

-- >>> runDemo inputPointerDemo
inputPointerDemo :: BiDiDemo
inputPointerDemo =
  demo "Input II - Pointer/Mouse Actions" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      chkBoxPage <- checkboxesUrl

      logTxt "Navigate to Checkboxes for pointer testing"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = chkBoxPage,
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Basic pointer click - Move and click checkbox"
      basicPointerClick <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 50,
                                y = 150,
                                duration = Just 500,
                                origin = Just ViewportOriginPointerType,
                                pointerCommonProperties = defaultPointerProps
                              },
                            PointerDown
                              { button = 0, -- Left mouse button
                                pointerCommonProperties = defaultPointerProps
                              },
                            PointerUp
                              { button = 0
                              }
                          ]
                      }
                ]
            }
      logShow "Basic pointer click result" basicPointerClick
      pause

      logTxt "Test 2: Pointer move with hover - Move to second checkbox and hover"
      pointerHover <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 50,
                                y = 170,
                                duration = Just 750,
                                origin = Just ViewportOriginPointerType,
                                pointerCommonProperties = defaultPointerProps
                              },
                            Pause {duration = Just 1000}
                          ]
                      }
                ]
            }
      logShow "Pointer hover result" pointerHover
      pause

      logTxt "Locate the first checkbox using CSS selector"
      checkbox1' <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#checkbox1"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "First checkbox search result" checkbox1'
      pause

      -- Extract the element's shared reference for clicking
      let MkLocateNodesResult nodes = checkbox1'

          checkbox1Id :: SharedId
          checkbox1Id = case nodes of
            [checkbox1] -> fromJust checkbox1.sharedId
            _ -> error "Failed to locate first checkbox"

      logTxt "Test 3: Double click - Double click on first checkbox"
      doubleClick <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 0, -- Relative to element center
                                y = 0, -- Relative to element center
                                duration = Just 300,
                                origin =
                                  Just $
                                    ElementOrigin $
                                      MkSharedReference
                                        { -- use a safe function instead in prod
                                          sharedId = checkbox1Id,
                                          handle = Nothing,
                                          extensions = Nothing
                                        },
                                pointerCommonProperties = defaultPointerProps
                              },
                            -- First click
                            PointerDown
                              { button = 0,
                                pointerCommonProperties = defaultPointerProps
                              },
                            PointerUp
                              { button = 0
                              },
                            Pause {duration = Just 100},
                            -- Second click
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
      logShow "Double click result" doubleClick
      pause

      logTxt "Test 4: Right click - Right click on page"
      rightClick <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 200,
                                y = 300,
                                duration = Just 400,
                                origin = Just ViewportOriginPointerType,
                                pointerCommonProperties = defaultPointerProps
                              },
                            PointerDown
                              { button = 2, -- Right mouse button
                                pointerCommonProperties = defaultPointerProps
                              },
                            PointerUp
                              { button = 2
                              },
                            Pause {duration = Just 500}
                          ]
                      }
                ]
            }
      logShow "Right click result" rightClick
      pause

      closeContext utils cmds bc

-- >>> runDemo inputWheelDemo
inputWheelDemo :: BiDiDemo
inputWheelDemo =
  demo "Input III - Wheel/Scroll Actions" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      infiniteScroll <- infiniteScrollUrl

      logTxt "Navigate to The Internet - Infinite Scroll for wheel testing"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = infiniteScroll,
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Basic wheel scroll down"
      scrollDown <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ WheelSourceActions $
                    MkWheelSourceActions
                      { wheelId = "wheel1",
                        wheelActions =
                          [ WheelScrollAction $
                              MkWheelScrollAction
                                { x = 400,
                                  y = 300,
                                  deltaX = 0,
                                  deltaY = 300,
                                  duration = Just 500,
                                  origin = Just ViewportOriginPointerType
                                }
                          ]
                      }
                ]
            }
      logShow "Scroll down result" scrollDown
      pause

      logTxt "Test 2: Pause and scroll up"
      scrollUp <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ WheelSourceActions $
                    MkWheelSourceActions
                      { wheelId = "wheel1",
                        wheelActions =
                          [ WheelPauseAction $ MkPauseAction $ Just 1000,
                            WheelScrollAction $
                              MkWheelScrollAction
                                { x = 400,
                                  y = 300,
                                  deltaX = 0,
                                  deltaY = -200,
                                  duration = Just 300,
                                  origin = Just ViewportOriginPointerType
                                }
                          ]
                      }
                ]
            }
      logShow "Scroll up result" scrollUp
      pause

      logTxt "Test 3: Horizontal scroll"
      horizontalScroll <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ WheelSourceActions $
                    MkWheelSourceActions
                      { wheelId = "wheel1",
                        wheelActions =
                          [ WheelScrollAction $
                              MkWheelScrollAction
                                { x = 400,
                                  y = 300,
                                  deltaX = 100,
                                  deltaY = 0,
                                  duration = Just 400,
                                  origin = Just ViewportOriginPointerType
                                }
                          ]
                      }
                ]
            }
      logShow "Horizontal scroll result" horizontalScroll
      pause

      closeContext utils cmds bc

-- >>> runDemo inputCombinedActionsDemo
-- *** Exception: Error executing BiDi command: MkCommand
--   { method = "input.performActions"
--   , params =
--       MkPerformActions
--         { context =
--             MkBrowsingContextId "96aa45af-9f1d-4a4d-a21d-e2533ccb49f5"
--         , actions =
--             [ NoneSourceActions MkPauseAction { duration = Just 1000 } ]
--         }
--   , extended = Nothing
--   }
-- With JSON: 
-- {
--     "id": 5,
--     "method": "input.performActions",
--     "params": {
--         "actions": [
--             {
--                 "duration": 1000,
--                 "type": "pause"
--             }
--         ],
--         "context": "96aa45af-9f1d-4a4d-a21d-e2533ccb49f5"
--     }
-- }
-- Failed to decode the 'result' property of JSON returned by driver to response type: 
-- {
--     "error": "invalid argument",
--     "id": 5,
--     "message": "Expected \"actionSequence.actions\" to be an array, got [object Undefined] undefined",
--     "stacktrace": "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:199:5\nInvalidArgumentError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:401:5\nassert.that/<@chrome://remote/content/shared/webdriver/Assert.sys.mjs:581:13\nassert.array@chrome://remote/content/shared/webdriver/Assert.sys.mjs:533:41\nfromJSON@chrome://remote/content/shared/webdriver/Actions.sys.mjs:2816:17\nfromJSON@chrome://remote/content/shared/webdriver/Actions.sys.mjs:2666:49\nperformActions@chrome://remote/content/webdriver-bidi/modules/root/input.sys.mjs:281:50\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:260:33\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:410:32\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n",
--     "type": "error"
-- }
-- Error message: 
-- key "result" not found
inputCombinedActionsDemo :: BiDiDemo
inputCombinedActionsDemo =
  demo "Input IV - Combined Actions" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      testPage <- textAreaUrl

      logTxt "Navigate to text area page for keyboard testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = testPage, wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Locate the text area 1 field using CSS selector"
      textArea' <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#textArea"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Text area 1 field search result" textArea'
      pause

      -- Extract the element's shared reference for clicking
      let MkLocateNodesResult nodes = textArea'

          textAreaId :: SharedId
          textAreaId = case nodes of
            [textArea] -> fromJust textArea.sharedId
            _ -> error "Failed to locate text area 1"

      logTxt "Test 1: Combined keyboard and pointer actions - Click username field and type"
      clickAndType <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions
                    ( MkPointerSourceActions
                        { pointerId = "mouse1",
                          pointer = Just $ MkPointer {pointerType = Just MousePointer},
                          pointerActions =
                            [ PointerMove
                                { x = 5,
                                  y = 5,
                                  duration = Just 300,
                                  origin =
                                    Just $
                                      ElementOrigin $
                                        MkSharedReference
                                          { -- use a safe function instead in prod
                                            sharedId = textAreaId,
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
                    ),
                  KeySourceActions
                    ( MkKeySourceActions
                        { keyId = "keyboard1",
                          keyActions =
                            [ KeyPause {duration = Just 200},
                              KeyDown "u",
                              KeyUp "u",
                              KeyDown "s",
                              KeyUp "s",
                              KeyDown "e",
                              KeyUp "e",
                              KeyDown "r",
                              KeyUp "r"
                            ]
                        }
                    )
                ]
            }
      logShow "Click and type result" clickAndType
      pause

      logTxt "Test 2: Combined actions with none source - Pause all input"
      pauseAllInput <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ NoneSourceActions $
                    MkPauseAction $
                      Just 1000
                ]
            }
      logShow "Pause all input result" pauseAllInput
      pause

      logTxt "Test 3: Complex combination - Move mouse, type password, and submit"
      complexCombination <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 200,
                                y = 180,
                                duration = Just 250,
                                origin = Just ViewportOriginPointerType,
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
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyPause {duration = Just 300},
                            KeyDown "p",
                            KeyUp "p",
                            KeyDown "a",
                            KeyUp "a",
                            KeyDown "s",
                            KeyUp "s",
                            KeyDown "s",
                            KeyUp "s",
                            KeyPause {duration = Just 200},
                            KeyDown "\xE007", -- Enter
                            KeyUp "\xE007"
                          ]
                      }
                ]
            }
      logShow "Complex combination result" complexCombination
      pause

      closeContext utils cmds bc

-- >>> runDemo inputReleaseActionsDemo
inputReleaseActionsDemo :: BiDiDemo
inputReleaseActionsDemo =
  demo "Input V - Release Actions" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Navigate to simple page for release actions testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><body><h1>Release Actions Test</h1><input type='text' placeholder='Type here' style='font-size: 20px; padding: 10px;'></body></html>", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Perform some input actions to have input state"
      setupActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyDown "t",
                            KeyUp "t",
                            KeyDown "e",
                            KeyUp "e",
                            KeyDown "s",
                            KeyUp "s",
                            KeyDown "t",
                            KeyUp "t"
                          ]
                      },
                  PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 200,
                                y = 100,
                                duration = Just 200,
                                origin = Just ViewportOriginPointerType,
                                pointerCommonProperties = defaultPointerProps
                              }
                          ]
                      }
                ]
            }
      logShow "Setup actions result" setupActions
      pause

      logTxt "Test 2: Release all input actions state"
      releaseResult <- inputReleaseActions $ MkReleaseActions {context = bcToId bc}
      logShow "Release actions result" releaseResult
      pause

      logTxt "Test 3: Perform actions again after release to confirm state is reset"
      postReleaseActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard2", -- Different ID to show it's a fresh start
                        keyActions =
                          [ KeyDown "n",
                            KeyUp "n",
                            KeyDown "e",
                            KeyUp "e",
                            KeyDown "w",
                            KeyUp "w"
                          ]
                      }
                ]
            }
      logShow "Post-release actions result" postReleaseActions
      pause

      logTxt "Test 4: Release actions again to demonstrate multiple releases"
      releaseResult2 <- inputReleaseActions $ MkReleaseActions {context = bcToId bc}
      logShow "Second release actions result" releaseResult2
      pause

      closeContext utils cmds bc

-- >>> runDemo inputSetFilesDemo
inputSetFilesDemo :: BiDiDemo
inputSetFilesDemo =
  demo "Input VI - Set Files for File Upload" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Navigate to The Internet - File Upload page"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/upload", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Find the file input element using browsingContextLocateNodes"
      fileInputResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "input[type='file']"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "File input element search result" fileInputResult
      pause

      -- Extract the element's shared reference for use with setFiles
      case fileInputResult of
        MkLocateNodesResult nodes -> case nodes of
          (MkNodeRemoteValue {sharedId = Just (Core.MkSharedId elementId)} : _) -> do
            logTxt "Test 2: Create test files (using data URLs to simulate files)"
            let testFiles =
                  [ "/tmp/test1.txt", -- These would be actual file paths on the system
                    "/tmp/test2.txt"
                  ]

            logTxt "Test 3: Set files on the file input element"
            setFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = testFiles
                  }
            logShow "Set files result" setFilesResult
            pause

            logTxt "Test 4: Set a single file"
            setSingleFileResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = ["/tmp/single_file.txt"]
                  }
            logShow "Set single file result" setSingleFileResult
            pause

            logTxt "Test 5: Clear files by setting empty list"
            clearFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = []
                  }
            logShow "Clear files result" clearFilesResult
            pause

            logTxt "Test 6: Set files with different extensions"
            setVariousFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files =
                      [ "/tmp/document.pdf",
                        "/tmp/image.jpg",
                        "/tmp/spreadsheet.xlsx",
                        "/tmp/presentation.pptx"
                      ]
                  }
            logShow "Set various files result" setVariousFilesResult
            pause
          _ -> do
            logTxt "Could not find file input element or extract sharedId"
            pause

      closeContext utils cmds bc
