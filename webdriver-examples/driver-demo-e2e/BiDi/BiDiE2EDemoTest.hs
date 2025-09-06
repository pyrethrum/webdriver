module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), mkDemoBiDiClientParams, mkFailBidiClientParams, withCommands)
import Data.Text (Text)
import Data.Word (Word64)
import IOUtils (DemoUtils (..))
import UnliftIO (async, cancel, wait)
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.Protocol
  ( Activate (..),
    BrowsingContext,
    CaptureScreenshot (..),
    ClipRectangle (..),
    Close (..),
    ContextTarget (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    GetTree (..),
    HandleUserPrompt (..),
    ImageFormat (..),
    Navigate (..),
    ScreenShotOrigin (..),
  )
import WebDriverPreCore.BiDi.Script (Evaluate (..), Target (..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

pauseMs :: Int
pauseMs = 0

-- >>> demo_parseUrl
-- "Right\n  MkBiDiUrl\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
demo_parseUrl :: Text
demo_parseUrl = txt $ parseUrl "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"

runDemo :: BiDiDemo -> IO ()
runDemo d =
  mkDemoBiDiClientParams pauseMs >>= \p -> withCommands p d.action

runFailDemo :: BiDiDemo -> Word64 -> Word64 -> Word64 -> IO ()
runFailDemo d failSendCount failGetCount failPrintCount = do
  mkFailBidiClientParams pauseMs failSendCount failGetCount failPrintCount >>= \p -> withCommands p d.action

-- example fail demos :: to be turned into tests later
sendFailDemo :: BiDiDemo -> IO ()
sendFailDemo d = runFailDemo d 2 0 0

getFailDemo :: BiDiDemo -> IO ()
getFailDemo d = runFailDemo d 0 2 0

printFailDemo :: BiDiDemo -> IO ()
printFailDemo d = runFailDemo d 0 0 3

data BiDiDemo = MkBiDiDemo
  { name :: Text,
    action :: DemoUtils -> Commands -> IO ()
  }

demo :: Text -> (DemoUtils -> Commands -> IO ()) -> BiDiDemo
demo name action = MkBiDiDemo {name, action}

-- TODO: Session find out about newSession Firefox threads

{-
##### BrowsingContext #####

4. browsingContextCreate :: DONE
1. browsingContextActivate : DONE
2. browsingContextCaptureScreenshot :: DONE
3. browsingContextClose :: DONE
5. browsingContextGetTree :: DONE*
6. browsingContextHandleUserPrompt :: DONE
7. browsingContextLocateNodes
8. browsingContextNavigate
9. browsingContextPrint
10. browsingContextReload
11. browsingContextSetViewport
12. browsingContextTraverseHistory

-- TODO when using script - get browsingContextGetTree with originalOpener

-}

-- ###########################################################################

-- Failure Demos :: TODO turn into tests ---

-- >>> printFailDemo browsingContextCreateActivateClose
-- *** Exception: Forced failure for testing: print (call #3)

-- >>> getFailDemo browsingContextCreateActivateClose

-- *** Exception: Forced failure for testing: get (call #2)

-- >>> sendFailDemo browsingContextCreateActivateClose

-- *** Exception: Forced failure for testing: send (call #2)

-- ###########################################################################

-- >>> runDemo browsingContextCreateActivateClose
browsingContextCreateActivateClose :: BiDiDemo
browsingContextCreateActivateClose =
  demo "Browsing Context - Create Activate Close" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "New browsing context - Tab"
      let bcParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate bcParams
      logShow "Browsing context - Tab" bc
      pause

      logTxt "New browsing context - Window"
      bcWin <- browsingContextCreate bcParams {createType = Window}
      logShow "Browsing context - Window" bcWin
      pause

      logTxt "New browsing context - Tab with reference context"
      bcWithContext <- browsingContextCreate bcParams {referenceContext = Just bc}
      logShow "Browsing context - Tab with reference context" bcWithContext
      pause

      logTxt "New browsing context - Background"
      bg <-
        browsingContextCreate
          bcParams
            { background = True,
              referenceContext = Just bcWin
            }
      logShow "Background browsing context created on front window" bg
      pause

      logTxt "New user context"
      uc <-
        browserCreateUserContext
          MkCreateUserContext
            { acceptInsecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User context created" uc
      pause

      logTxt "New browsing context - Window with user context"
      bcWinWithUC <-
        browsingContextCreate
          bcParams
            { createType = Window,
              userContext = Just uc
            }
      logShow "Browsing context - Window with user context" bcWinWithUC
      pause

      logTxt "Activate initial browsing context"
      o <- browsingContextActivate $ MkActivate bc
      logShow "Activate result" o
      pause

      logTxt "Close initial browsing context"
      co <-
        browsingContextClose $
          MkClose
            { context = bc,
              promptUnload = Nothing
            }
      logShow "Close result" co
      pause

newWindowContext :: DemoUtils -> Commands -> IO BrowsingContext
newWindowContext MkDemoUtils {..} MkCommands {..} = do
  logTxt "New browsing context - Window"
  bcWin <- browsingContextCreate bcParams {createType = Window}
  logShow "Browsing context - Window" bcWin
  pause
  pure bcWin
  where
    bcParams =
      MkCreate
        { createType = Tab,
          background = False,
          referenceContext = Nothing,
          userContext = Nothing
        }

closeContext :: DemoUtils -> Commands -> BrowsingContext -> IO ()
closeContext MkDemoUtils {..} MkCommands {..} bc = do
  logTxt "Close browsing context"
  co <- browsingContextClose $ MkClose {context = bc, promptUnload = Nothing}
  logShow "Close result" co
  pause

-- >>> runDemo browsingContextCaptureScreenshotClose
browsingContextCaptureScreenshotClose :: BiDiDemo
browsingContextCaptureScreenshotClose =
  demo "Browsing Context - Capture Screenshot / Close" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      logTxt "Capture screenshot - default"
      screenshot <- browsingContextCaptureScreenshot $ MkCaptureScreenshot bc Nothing Nothing Nothing
      logShow "Screenshot captured" screenshot
      pause

      logTxt "Capture screenshot - document, format png"
      screenshotDoc <-
        browsingContextCaptureScreenshot $
          MkCaptureScreenshot
            { context = bc,
              origin = (Just Document),
              format =
                Just $
                  MkImageFormat
                    { imageType = "png",
                      quality = Just 0.75
                    },
              clip = Nothing
            }
      logShow "Screenshot captured" screenshotDoc
      pause

      logTxt "Capture screenshot - viewport, clip"
      screenshotViewport <-
        browsingContextCaptureScreenshot $
          MkCaptureScreenshot
            { context = bc,
              origin = (Just Viewport),
              format = Nothing,
              clip = Just $ BoxClipRectangle {x = 0, y = 0, width = 300, height = 300}
            }
      logShow "Screenshot captured" screenshotViewport
      pause

      logTxt "Close browsing context - unload prompt False"
      co <- browsingContextClose $ MkClose {context = bc, promptUnload = Just False}
      logShow "Close result" co
      pause

-- >>> runDemo browsingContextClosePromptUnload
browsingContextClosePromptUnload :: BiDiDemo
browsingContextClosePromptUnload =
  demo "Browsing Context - Close with unload prompt" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      -- promptUnload doesn't seem to do anything ??
      logTxt "Close browsing context - unload prompt True"
      co <- browsingContextClose $ MkClose {context = bc, promptUnload = Just True}
      logShow "Close result" co
      pause

-- >>> runDemo browsingContextGetTree
browsingContextGetTree :: BiDiDemo
browsingContextGetTree =
  demo "Browsing Context - Get Tree" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      logTxt "Get browsing context tree - all"
      tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
      logShow "Browsing context tree" tree
      pause

      logTxt "Create opener browsing context (Window)"
      openerContext <- newWindowContext utils cmds

      logTxt "Create opened browsing context (Tab with opener reference)"
      openedContext <-
        browsingContextCreate $
          MkCreate
            { createType = Tab,
              background = False,
              referenceContext = Just openerContext,
              userContext = Nothing
            }
      logShow "Opened browsing context" openedContext
      pause

      logTxt "Get browsing context tree - WebDriver created contexts (originalOpener likely null)"
      treeWithOpener <-
        browsingContextGetTree
          MkGetTree
            { maxDepth = Nothing,
              root = Nothing -- Get all contexts to see the relationship
            }
      logShow "Browsing context tree - WebDriver created (originalOpener should be null)" treeWithOpener
      pause

      -- TODO: when using Scripts - get browsingContextGetTree with originalOpener
      logTxt "NOTE: originalOpener is null because WebDriver.create doesn't set opener relationships"
      logTxt "originalOpener is only set when web content opens windows via window.open(), not WebDriver commands"
      pause

      logTxt "IMPORTANT: referenceContext does NOT create parent-child relationships!"
      logTxt "Parent-child relationships only exist between main documents and their iframes"
      logTxt "All WebDriver-created tabs/windows are top-level contexts with no parent"
      pause

      logTxt "Demonstrating: All created contexts are top-level (no parent, no children)"
      logTxt "Get tree from 'opener' context - will show NO children (not a parent)"
      treeFromOpener <-
        browsingContextGetTree
          MkGetTree
            { maxDepth = Nothing,
              root = Just openerContext
            }
      logShow "Tree from opener context (no children expected)" treeFromOpener
      pause

      logTxt "Get tree from 'opened' context - will show NO parent (top-level)"
      treeFromOpened <-
        browsingContextGetTree
          MkGetTree
            { maxDepth = Nothing,
              root = Just openedContext
            }
      logShow "Tree from opened context (no parent expected)" treeFromOpened
      pause

      logTxt "Get full tree - shows all top-level contexts as siblings"
      fullTree <-
        browsingContextGetTree
          MkGetTree
            { maxDepth = Nothing,
              root = Nothing
            }
      logShow "Full tree (all contexts are top-level siblings)" fullTree
      pause

      -- TODO: when using IFrames - get parent-child relationships
      logTxt "To see parent-child relationships, you need:"
      logTxt "1. Navigate to a page with iframes"
      logTxt "2. The main document = parent, iframes = children"
      logTxt "3. referenceContext is only for tab positioning, not hierarchy"
      pause

-- TODO: remove all unneeded From / ToJSON instances

-- >>> runDemo browsingContextHandleUserPrompt
browsingContextHandleUserPrompt :: BiDiDemo
browsingContextHandleUserPrompt =
  demo "Browsing Context - Handle User Prompt" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      logTxt "Test 1: Create and handle an alert dialog"

      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "alert('Hello from Pyrethrum BiDi!')",
            target =
              ContextTarget $
                MkContextTarget
                  { context = bc,
                    sandbox = Nothing
                  },
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      -- Wait for the alert to be displayed (in a productions system, more sophisticated polling would be needed)
      sleep 500

      logTxt "Accept the alert dialog"
      acceptResult <-
        browsingContextHandleUserPrompt $
          MkHandleUserPrompt
            { context = bc,
              accept = Just True,
              userText = Nothing
            }
      logShow "Alert accept result" acceptResult
      pause

      logTxt "Test 2: Create and handle a confirm dialog"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "confirm('Hello from Pyrethrum BiDi. Do you want to continue?')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      -- Wait for the alert to be displayed (in a productions system, more sophisticated polling would be needed)
      sleep 500

      logTxt "Dismiss the confirm dialog"
      dismissResult <-
        browsingContextHandleUserPrompt $
          MkHandleUserPrompt
            { context = bc,
              accept = Just False,
              userText = Nothing
            }
      logShow "Confirm dismiss result" dismissResult
      pause

      logTxt "Test 3: Create and handle a prompt dialog with user input"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "prompt('What is your name?', 'Default Name')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      -- Wait for the alert to be displayed (in a productions system, more sophisticated polling would be needed)
      sleep 500

      logTxt "Accept prompt with custom text"
      promptResult <-
        browsingContextHandleUserPrompt $
          MkHandleUserPrompt
            { context = bc,
              accept = Just True,
              userText = Just "John Doe from BiDi"
            }
      logShow "Prompt accept with text result" promptResult
      pause

      logTxt "Test 4: Create and dismiss a prompt dialog"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "prompt('Enter your age:')",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pause

      -- Wait for the alert to be displayed (in a productions system, more sophisticated polling would be needed)
      sleep 500

      logTxt "Dismiss the prompt dialog"
      dismissPromptResult <-
        browsingContextHandleUserPrompt $
          MkHandleUserPrompt
            { context = bc,
              accept = Just False,
              userText = Nothing
            }
      logShow "Prompt dismiss result" dismissPromptResult
      pause
