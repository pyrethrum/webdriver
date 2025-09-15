module BiDi.Demos.InputDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Protocol
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextId (..), Locator (..))
import WebDriverPreCore.BiDi.CoreTypes (NodeRemoteValue (..))
import qualified WebDriverPreCore.BiDi.CoreTypes as Core
import qualified WebDriverPreCore.BiDi.Script as Script
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
defaultPointerProps = MkPointerCommonProperties
  { width = Nothing,
    height = Nothing,
    pressure = Nothing,
    tangentialPressure = Nothing,
    twist = Nothing,
    altitudeAngle = Nothing,
    azimuthAngle = Nothing
  }

-- Helper function to convert CoreTypes SharedId to Script SharedId
coreToScriptSharedId :: Core.SharedId -> Script.SharedId
coreToScriptSharedId (Core.MkSharedId txt) = Script.MkShareId {Script.id = txt}

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

      logTxt "Test 1: Basic key actions - Type in username field"
      basicKeyActions <-
        inputPerformActions $
          MkPerformActions
            { context = bcToId bc,
              actions =
                [ KeySourceActions $
                    MkKeySourceActions
                      { keyType = "key",
                        keyId = "keyboard1",
                        keyActions =
                          [ KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 100},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "t"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "t"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "o"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "o"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "m"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "m"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "s"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "s"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "m"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "m"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "i"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "i"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "t"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "t"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "h"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "h"}
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
                      { keyType = "key",
                        keyId = "keyboard1",
                        keyActions =
                          [ -- Tab key to move to password field
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "\xE004"}, -- Tab
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "\xE004"},
                            KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 200},
                            -- Type password
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "S"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "S"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "u"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "u"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "p"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "p"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "e"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "e"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "r"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "r"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "S"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "S"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "e"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "e"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "c"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "c"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "r"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "r"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "e"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "e"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "t"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "t"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "!"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "!"}
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
                      { keyType = "key",
                        keyId = "keyboard1",
                        keyActions =
                          [ -- Shift+Tab to go back to username field
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "\xE008"}, -- Shift
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "\xE004"}, -- Tab
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "\xE004"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "\xE008"},
                            KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 200},
                            -- Ctrl+A to select all
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "\xE009"}, -- Ctrl
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "a"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "a"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "\xE009"},
                            KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 200},
                            -- Type new username
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "a"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "a"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "d"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "d"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "m"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "m"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "i"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "i"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "n"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "n"}
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
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 50,
                                  y = 150,
                                  duration = Just 500,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { pointerDownType = "pointerDown",
                                  button = 0, -- Left mouse button
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { pointerUpType = "pointerUp",
                                  button = 0
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
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 50,
                                  y = 170,
                                  duration = Just 750,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 1000}
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
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 50,
                                  y = 150,
                                  duration = Just 300,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            -- First click
                            PointerDownAction $
                              MkPointerDownAction
                                { pointerDownType = "pointerDown",
                                  button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { pointerUpType = "pointerUp",
                                  button = 0
                                },
                            PointerPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 100},
                            -- Second click
                            PointerDownAction $
                              MkPointerDownAction
                                { pointerDownType = "pointerDown",
                                  button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { pointerUpType = "pointerUp",
                                  button = 0
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
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 200,
                                  y = 300,
                                  duration = Just 400,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { pointerDownType = "pointerDown",
                                  button = 2, -- Right mouse button
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { pointerUpType = "pointerUp",
                                  button = 2
                                },
                            PointerPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 500}
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
                      { wheelType = "wheel",
                        wheelId = "wheel1",
                        wheelActions =
                          [ WheelScrollAction $
                              MkWheelScrollAction
                                { scrollType = "scroll",
                                  x = 400,
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
                      { wheelType = "wheel",
                        wheelId = "wheel1",
                        wheelActions =
                          [ WheelPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 1000},
                            WheelScrollAction $
                              MkWheelScrollAction
                                { scrollType = "scroll",
                                  x = 400,
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
                      { wheelType = "wheel",
                        wheelId = "wheel1",
                        wheelActions =
                          [ WheelScrollAction $
                              MkWheelScrollAction
                                { scrollType = "scroll",
                                  x = 400,
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
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 200,
                                  y = 150,
                                  duration = Just 300,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { pointerDownType = "pointerDown",
                                  button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { pointerUpType = "pointerUp",
                                  button = 0
                                }
                          ]
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyType = "key",
                        keyId = "keyboard1",
                        keyActions =
                          [ KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 200},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "u"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "u"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "s"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "s"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "e"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "e"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "r"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "r"}
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
                    MkPauseAction {pauseType = "pause", duration = Just 1000}
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
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 200,
                                  y = 180,
                                  duration = Just 250,
                                  origin = Just ViewportOriginPointerType,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerDownAction $
                              MkPointerDownAction
                                { pointerDownType = "pointerDown",
                                  button = 0,
                                  pointerCommonProperties = defaultPointerProps
                                },
                            PointerUpAction $
                              MkPointerUpAction
                                { pointerUpType = "pointerUp",
                                  button = 0
                                }
                          ]
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyType = "key",
                        keyId = "keyboard1",
                        keyActions =
                          [ KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 300},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "p"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "p"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "a"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "a"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "s"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "s"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "s"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "s"},
                            KeyPauseAction $ MkPauseAction {pauseType = "pause", duration = Just 200},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "\xE007"}, -- Enter
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "\xE007"}
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
                      { keyType = "key",
                        keyId = "keyboard1",
                        keyActions =
                          [ KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "t"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "t"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "e"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "e"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "s"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "s"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "t"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "t"}
                          ]
                      },
                  PointerSourceActions $
                    MkPointerSourceActions
                      { pointerType = "pointer",
                        pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMoveAction $
                              MkPointerMoveAction
                                { pointerMoveType = "pointerMove",
                                  x = 200,
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
                      { keyType = "key",
                        keyId = "keyboard2", -- Different ID to show it's a fresh start
                        keyActions =
                          [ KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "n"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "n"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "e"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "e"},
                            KeyDownAction $ MkKeyDownAction {keyDownType = "keyDown", value = "w"},
                            KeyUpAction $ MkKeyUpAction {keyUpType = "keyUp", value = "w"}
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
                    element = Script.MkSharedReference {sharedId = coreToScriptSharedId (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = testFiles
                  }
            logShow "Set files result" setFilesResult
            pause

            logTxt "Test 4: Set a single file"
            setSingleFileResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = Script.MkSharedReference {sharedId = coreToScriptSharedId (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = ["/tmp/single_file.txt"]
                  }
            logShow "Set single file result" setSingleFileResult
            pause

            logTxt "Test 5: Clear files by setting empty list"
            clearFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = Script.MkSharedReference {sharedId = coreToScriptSharedId (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
                    files = []
                  }
            logShow "Clear files result" clearFilesResult
            pause

            logTxt "Test 6: Set files with different extensions"
            setVariousFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bcToId bc,
                    element = Script.MkSharedReference {sharedId = coreToScriptSharedId (Core.MkSharedId elementId), handle = Nothing, extensions = Nothing},
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