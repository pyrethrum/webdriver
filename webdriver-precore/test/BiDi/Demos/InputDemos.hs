module BiDi.Demos.InputDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import Data.Maybe (fromJust)
import IOUtils (DemoUtils (..))
import TestData (checkboxesUrl, fileUrl, infiniteScrollUrl, textAreaUrl, uploadFilePath)
import WebDriverPreCore.BiDi.BrowsingContext (Locator (..))
import WebDriverPreCore.BiDi.CoreTypes (NodeRemoteValue (..))
import WebDriverPreCore.BiDi.CoreTypes qualified as Core
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log)


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
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      textAreaPageUrl <- textAreaUrl

      logTxt "Navigate to text area page for keyboard testing"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = textAreaPageUrl, wait = Just Complete}
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
                { context = bc,
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
            { context = bc,
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
            { context = bc,
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
      inputPerformActions
        $ MkPerformActions
          { context = bc,
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
            { context = bc,
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
    action :: DemoUtils -> BiDiActions -> IO ()
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
            { context = bc,
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
            { context = bc,
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
            { context = bc,
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
            { context = bc,
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
    action :: DemoUtils -> BiDiActions -> IO ()
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
            { context = bc,
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
            { context = bc,
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
            { context = bc,
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
    action :: DemoUtils -> BiDiActions -> IO ()
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
            { context = bc,
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
            { context = bc,
              actions =
                [ NoneSourceActions $
                    MkNoneSourceActions
                      { noneId = "none1",
                        noneActions = [MkPauseAction $ Just 1000]
                      }
                ]
            }
      logShow "Pause all input result" pauseAllInput
      pause

      logTxt "Locate the text area 2 field using CSS selector"
      textArea2' <-
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
      let MkLocateNodesResult nodes2 = textArea2'

          textAreaId2 :: SharedId
          textAreaId2 = case nodes2 of
            [textArea] -> fromJust textArea.sharedId
            _ -> error "Failed to locate text area 2"

      logTxt "Test 3: Complex combination - Move mouse, type password, and submit"
      complexCombination <-
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
                              { x = 200,
                                y = 180,
                                duration = Just 250,
                                origin =
                                  Just $
                                    ElementOrigin $
                                      MkSharedReference
                                        { sharedId = textAreaId2,
                                          handle = Nothing,
                                          extensions = Nothing
                                        },
                                pointerCommonProperties = defaultPointerProps
                              },
                            PointerDown
                              { button = 0,
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
    action :: DemoUtils -> BiDiActions -> IO ()
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

      logTxt "Test 1: Focus text area and perform some input actions to have input state"
      setupActions <-
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
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard1",
                        keyActions =
                          [ KeyPause {duration = Just 100},
                            KeyDown "t",
                            KeyUp "t",
                            KeyDown "e",
                            KeyUp "e",
                            KeyDown "s",
                            KeyUp "s",
                            KeyDown "t",
                            KeyUp "t"
                          ]
                      }
                ]
            }
      logShow "Setup actions result" setupActions
      pause

      logTxt "Test 2: Release all input actions state"
      releaseResult <- inputReleaseActions $ MkReleaseActions {context = bc}
      logShow "Release actions result" releaseResult
      pause

      logTxt "Test 3: Focus text area again and perform actions after release to confirm state is reset"
      postReleaseActions <-
        inputPerformActions $
          MkPerformActions
            { context = bc,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse2", -- Different ID to show it's a fresh start
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
                      },
                  KeySourceActions $
                    MkKeySourceActions
                      { keyId = "keyboard2", -- Different ID to show it's a fresh start
                        keyActions =
                          [ KeyPause {duration = Just 100},
                            KeyDown "n",
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
      releaseResult2 <- inputReleaseActions $ MkReleaseActions {context = bc}
      logShow "Second release actions result" releaseResult2
      pause

      closeContext utils cmds bc

-- >>> runDemo inputSetFilesDemo
inputSetFilesDemo :: BiDiDemo
inputSetFilesDemo =
  demo "Input VI - Set Files for File Upload" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      uploadUrl <- fileUrl "upload.html"

      logTxt "Navigate to upload.html test page"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = uploadUrl, wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Find the single file input element"
      singleFileInputResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#singleUpload"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Single file input element search result" singleFileInputResult
      pause

      logTxt "Test 2: Find the multiple file input element"
      multipleFileInputResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#multipleUpload"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Multiple file input element search result" multipleFileInputResult
      pause

      -- Extract the single file input's shared reference
      case singleFileInputResult of
        MkLocateNodesResult nodes -> case nodes of
          (MkNodeRemoteValue {sharedId = Just (Core.MkSharedId singleElementId)} : _) -> do
            logTxt "Test 3: Set a single file on the single file input"
            singleDocPath <- uploadFilePath "single_document.txt"
            setSingleFileResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId singleElementId), handle = Nothing, extensions = Nothing},
                    files = [singleDocPath]
                  }
            logShow "Set single file result" setSingleFileResult
            pause

            logTxt "Test 4: Clear the single file input"
            clearSingleFileResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId singleElementId), handle = Nothing, extensions = Nothing},
                    files = []
                  }
            logShow "Clear single file result" clearSingleFileResult
            pause
          _ -> do
            logTxt "Could not find single file input element or extract sharedId"
            pause

      -- Extract the multiple file input's shared reference
      case multipleFileInputResult of
        MkLocateNodesResult nodes -> case nodes of
          (MkNodeRemoteValue {sharedId = Just (Core.MkSharedId multipleElementId)} : _) -> do
            logTxt "Test 5: Set multiple files on the multiple file input"
            doc1Path <- uploadFilePath "document1.txt"
            doc2Path <- uploadFilePath "document2.pdf"
            imagePath <- uploadFilePath "image.jpg"
            setMultipleFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId multipleElementId), handle = Nothing, extensions = Nothing},
                    files = [doc1Path, doc2Path, imagePath]
                  }
            logShow "Set multiple files result" setMultipleFilesResult
            pause

            logTxt "Test 6: Set files with different extensions on multiple input"
            spreadsheetPath <- uploadFilePath "spreadsheet.xlsx"
            presentationPath <- uploadFilePath "presentation.pptx"
            archivePath <- uploadFilePath "archive.zip"
            videoPath <- uploadFilePath "video.mp4"
            setVariousFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId multipleElementId), handle = Nothing, extensions = Nothing},
                    files = [spreadsheetPath, presentationPath, archivePath, videoPath]
                  }
            logShow "Set various files result" setVariousFilesResult
            pause

            logTxt "Test 7: Clear the multiple file input"
            clearMultipleFilesResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId multipleElementId), handle = Nothing, extensions = Nothing},
                    files = []
                  }
            logShow "Clear multiple files result" clearMultipleFilesResult
            pause

            logTxt "Test 8: Set a single file on the multiple input (should work)"
            finalTestPath <- uploadFilePath "final_test.txt"
            setSingleOnMultipleResult <-
              inputSetFiles $
                MkSetFiles
                  { context = bc,
                    element = MkSharedReference {sharedId = (Core.MkSharedId multipleElementId), handle = Nothing, extensions = Nothing},
                    files = [finalTestPath]
                  }
            logShow "Set single file on multiple input result" setSingleOnMultipleResult
            pause
          _ -> do
            logTxt "Could not find multiple file input element or extract sharedId"
            pause
