module BiDi.Demos.InputDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextId (..), Locator (..))
import WebDriverPreCore.BiDi.CoreTypes (NodeRemoteValue (..))
import WebDriverPreCore.BiDi.CoreTypes qualified as Core
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Protocol
import WebDriverPreCore.BiDi.Script qualified as Script
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

      logTxt "Navigate to The Internet - Form Authentication for keyboard testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/login", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Locate the username field using CSS selector"
      usernameFieldResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#username"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Username field search result" usernameFieldResult
      pause

      -- Extract the element's shared reference for clicking
      case usernameFieldResult of
        MkLocateNodesResult nodes -> case nodes of
          (MkNodeRemoteValue {sharedId = Just (Core.MkSharedId elementId)} : _) -> do
            logTxt "Focus the username field by clicking on it using element reference"
            focusUsernameField <-
              inputPerformActions $
                MkPerformActions
                  { context = bcToId bc,
                    actions =
                      [ PointerSourceActions $
                          MkPointerSourceActions
                            { pointerId = "mouse1",
                              pointer = Just $ MkPointer {pointerType = Just MousePointer},
                              pointerActions =
                                [ PointerMoveAction $
                                    MkPointerMoveAction
                                      { x = 0, -- Relative to element center
                                        y = 0, -- Relative to element center
                                        duration = Just 300,
                                        origin = Just $ ElementOriginRef $ MkElementOrigin $ Script.MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                                        pointerCommonProperties = defaultPointerProps
                                      },
                                  PointerDownAction $
                                    MkPointerDownAction
                                      { button = 0,
                                        pointerCommonProperties = defaultPointerProps
                                      },
                                  PointerUpAction $
                                    MkPointerUpAction
                                      { button = 0
                                      }
                                ]
                            }
                      ]
                  }
            logShow "Focus username field result" focusUsernameField
            pause
          _ -> do
            logTxt "Could not find username element or extract sharedId"
            pause

      logTxt "Test 1: Basic key actions - Type in username field"
      basicKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyPauseAction . MkPauseAction $ Just 100,
                            KeyDownAction $ MkKeyDownAction "t",
                            KeyUpAction $ MkKeyUpAction "t",
                            KeyDownAction $ MkKeyDownAction "o",
                            KeyUpAction $ MkKeyUpAction "o",
                            KeyDownAction $ MkKeyDownAction "m",
                            KeyUpAction $ MkKeyUpAction "m",
                            KeyDownAction $ MkKeyDownAction "s",
                            KeyUpAction $ MkKeyUpAction "s",
                            KeyDownAction $ MkKeyDownAction "m",
                            KeyUpAction $ MkKeyUpAction "m",
                            KeyDownAction $ MkKeyDownAction "i",
                            KeyUpAction $ MkKeyUpAction "i",
                            KeyDownAction $ MkKeyDownAction "t",
                            KeyUpAction $ MkKeyUpAction "t",
                            KeyDownAction $ MkKeyDownAction "h",
                            KeyUpAction $ MkKeyUpAction "h"
                          ]
                      }
                ]
            }
      logShow "Basic key actions result" basicKeyActions
      pause

      logTxt "Test 2: Special keys - Tab to password field and type password"
      specialKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ -- Tab key to move to password field
                            KeyDownAction $ MkKeyDownAction "\xE004", -- Tab
                            KeyUpAction $ MkKeyUpAction "\xE004",
                            KeyPauseAction . MkPauseAction $ Just 200,
                            -- Type password
                            KeyDownAction $ MkKeyDownAction "S",
                            KeyUpAction $ MkKeyUpAction "S",
                            KeyDownAction $ MkKeyDownAction "u",
                            KeyUpAction $ MkKeyUpAction "u",
                            KeyDownAction $ MkKeyDownAction "p",
                            KeyUpAction $ MkKeyUpAction "p",
                            KeyDownAction $ MkKeyDownAction "e",
                            KeyUpAction $ MkKeyUpAction "e",
                            KeyDownAction $ MkKeyDownAction "r",
                            KeyUpAction $ MkKeyUpAction "r",
                            KeyDownAction $ MkKeyDownAction "S",
                            KeyUpAction $ MkKeyUpAction "S",
                            KeyDownAction $ MkKeyDownAction "e",
                            KeyUpAction $ MkKeyUpAction "e",
                            KeyDownAction $ MkKeyDownAction "c",
                            KeyUpAction $ MkKeyUpAction "c",
                            KeyDownAction $ MkKeyDownAction "r",
                            KeyUpAction $ MkKeyUpAction "r",
                            KeyDownAction $ MkKeyDownAction "e",
                            KeyUpAction $ MkKeyUpAction "e",
                            KeyDownAction $ MkKeyDownAction "t",
                            KeyUpAction $ MkKeyUpAction "t",
                            KeyDownAction $ MkKeyDownAction "!",
                            KeyUpAction $ MkKeyUpAction "!"
                          ]
                      }
                ]
            }
      logShow "Special key actions result" specialKeyActions
      pause

      logTxt "Test 3: Modifier keys - Ctrl+A to select all in username field"
      modifierKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ -- Shift+Tab to go back to username field
                            KeyDownAction $ MkKeyDownAction "\xE008", -- Shift
                            KeyDownAction $ MkKeyDownAction "\xE004", -- Tab
                            KeyUpAction $ MkKeyUpAction "\xE004",
                            KeyUpAction $ MkKeyUpAction "\xE008",
                            KeyPauseAction . MkPauseAction $ Just 200,
                            -- Ctrl+A to select all
                            KeyDownAction $ MkKeyDownAction "\xE009", -- Ctrl
                            KeyDownAction $ MkKeyDownAction "a",
                            KeyUpAction $ MkKeyUpAction "a",
                            KeyUpAction $ MkKeyUpAction "\xE009",
                            KeyPauseAction . MkPauseAction $ Just 200,
                            -- Type new username
                            KeyDownAction $ MkKeyDownAction "a",
                            KeyUpAction $ MkKeyUpAction "a",
                            KeyDownAction $ MkKeyDownAction "d",
                            KeyUpAction $ MkKeyUpAction "d",
                            KeyDownAction $ MkKeyDownAction "m",
                            KeyUpAction $ MkKeyUpAction "m",
                            KeyDownAction $ MkKeyDownAction "i",
                            KeyUpAction $ MkKeyUpAction "i",
                            KeyDownAction $ MkKeyDownAction "n",
                            KeyUpAction $ MkKeyUpAction "n"
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

      logTxt "Navigate to The Internet - Checkboxes for pointer testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/checkboxes", wait = Just Complete}
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
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { x = 50,
                                  y = 150,
                                  duration = Just 500,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { button = 0, -- Left mouse button
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
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
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { x = 50,
                                  y = 170,
                                  duration = Just 750,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerPauseAction $ MkPauseAction $ Just 1000
                          ]
                      }
                ]
            }
      logShow "Pointer hover result" pointerHover
      pause

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
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { x = 50,
                                  y = 150,
                                  duration = Just 300,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            -- First click
                            PointerDownAction $
                              MkPointerDownAction
                                { button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { button = 0
                                },
                            PointerPauseAction $ MkPauseAction $ Just 100,
                            -- Second click
                            PointerDownAction $
                              MkPointerDownAction
                                { button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
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
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { x = 200,
                                  y = 300,
                                  duration = Just 400,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { button = 2, -- Right mouse button
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { button = 2
                                },
                            PointerPauseAction $ MkPauseAction $ Just 500
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

      logTxt "Navigate to The Internet - Infinite Scroll for wheel testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/infinite_scroll", wait = Just Complete}
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
inputCombinedActionsDemo :: BiDiDemo
inputCombinedActionsDemo =
  demo "Input IV - Combined Actions" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Navigate to The Internet - Login page for combined actions testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/login", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Combined keyboard and pointer actions - Click username field and type"
      clickAndType <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { x = 200,
                                  y = 150,
                                  duration = Just 300,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { button = 0
                                }
                          ]
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyPauseAction . MkPauseAction $ Just 200,
                            KeyDownAction $ MkKeyDownAction "u",
                            KeyUpAction $ MkKeyUpAction "u",
                            KeyDownAction $ MkKeyDownAction "s",
                            KeyUpAction $ MkKeyUpAction "s",
                            KeyDownAction $ MkKeyDownAction "e",
                            KeyUpAction $ MkKeyUpAction "e",
                            KeyDownAction $ MkKeyDownAction "r",
                            KeyUpAction $ MkKeyUpAction "r"
                          ]
                      }
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
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { x = 200,
                                  y = 180,
                                  duration = Just 250,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { button = 0
                                }
                          ]
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyPauseAction . MkPauseAction $ Just 300,
                            KeyDownAction $ MkKeyDownAction "p",
                            KeyUpAction $ MkKeyUpAction "p",
                            KeyDownAction $ MkKeyDownAction "a",
                            KeyUpAction $ MkKeyUpAction "a",
                            KeyDownAction $ MkKeyDownAction "s",
                            KeyUpAction $ MkKeyUpAction "s",
                            KeyDownAction $ MkKeyDownAction "s",
                            KeyUpAction $ MkKeyUpAction "s",
                            KeyPauseAction . MkPauseAction $ Just 200,
                            KeyDownAction $ MkKeyDownAction "\xE007", -- Enter
                            KeyUpAction $ MkKeyUpAction "\xE007"
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
                          [ KeyDownAction $ MkKeyDownAction "t",
                            KeyUpAction $ MkKeyUpAction "t",
                            KeyDownAction $ MkKeyDownAction "e",
                            KeyUpAction $ MkKeyUpAction "e",
                            KeyDownAction $ MkKeyDownAction "s",
                            KeyUpAction $ MkKeyUpAction "s",
                            KeyDownAction $ MkKeyDownAction "t",
                            KeyUpAction $ MkKeyUpAction "t"
                          ]
                      },
                  PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
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
                          [ KeyDownAction $ MkKeyDownAction "n",
                            KeyUpAction $ MkKeyUpAction "n",
                            KeyDownAction $ MkKeyDownAction "e",
                            KeyUpAction $ MkKeyUpAction "e",
                            KeyDownAction $ MkKeyDownAction "w",
                            KeyUpAction $ MkKeyUpAction "w"
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
                    element = Script.MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = testFiles
                  }
            logShow "Set files result" setFilesResult
            pause

            logTxt "Test 4: Set a single file"
            setSingleFileResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = Script.MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = ["/tmp/single_file.txt"]
                  }
            logShow "Set single file result" setSingleFileResult
            pause

            logTxt "Test 5: Clear files by setting empty list"
            clearFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = Script.MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = []
                  }
            logShow "Clear files result" clearFilesResult
            pause

            logTxt "Test 6: Set files with different extensions"
            setVariousFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = Script.MkSharedReference {sharedId = (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
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
