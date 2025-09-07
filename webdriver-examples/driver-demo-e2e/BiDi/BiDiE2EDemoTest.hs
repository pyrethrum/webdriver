module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), mkDemoBiDiClientParams, mkFailBidiClientParams, withCommands)
import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import Data.Word (Word64)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.BrowsingContext (LocateNodes (..), LocateNodesResult (..), Locator (..), Print (..), PrintMargin (..), PrintPage (..), PrintResult (..), ReadinessState (..), SetViewport (..), Viewport (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..), NodeRemoteValue (..), SharedId (..))
import WebDriverPreCore.BiDi.Protocol
  ( Activate (..),
    BrowsingContext (..),
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
    Orientation (..),
    PageRange (..),
    Reload (..),
    ScreenShotOrigin (..),
    TraverseHistory (..),
  )
import WebDriverPreCore.BiDi.Script (Evaluate (..), SharedId (..), SharedReference (..), Target (..))
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

-- TODO: Session find out about newSession Firefox threads

{-
##### BrowsingContext #####

1. browsingContextCreate :: DONE
2. browsingContextActivate :: DONE
3. browsingContextCaptureScreenshot :: DONE
4. browsingContextClose :: DONE
5. browsingContextGetTree :: DONE*
6. browsingContextHandleUserPrompt :: DONE
7. browsingContextLocateNodes :: DONE
8. browsingContextNavigate :: DONE
9. browsingContextPrint :: DONE
10. browsingContextReload :: DONE
11. browsingContextSetViewport :: DONE
12. browsingContextTraverseHistory :: DONE

##### Script #####
1. scriptAddPreloadScript,
2. scriptCallFunction,
3. scriptDisown,
4. scriptEvaluate,
5. scriptGetRealms,
6. scriptRemovePreloadScript

-- TODO when using script - get browsingContextGetTree with originalOpener
  -- list not supported yet in geckodriver

-}

{-
TODO: stuff to add to haddock notes
- things u would add
  - more ergonomic types where there is a default eg. wait = Nothing

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

-- >>> runDemo browsingContextCreateActivateCloseDemo
browsingContextCreateActivateCloseDemo :: BiDiDemo
browsingContextCreateActivateCloseDemo =
  demo "Browsing Context - Create, Activate, Close" action
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

-- >>> runDemo browsingContextCaptureScreenshotCloseDemo
browsingContextCaptureScreenshotCloseDemo :: BiDiDemo
browsingContextCaptureScreenshotCloseDemo =
  demo "Browsing Context - Capture Screenshot, Close" action
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

-- >>> runDemo browsingContextClosePromptUnloadDemo
browsingContextClosePromptUnloadDemo :: BiDiDemo
browsingContextClosePromptUnloadDemo =
  demo "Browsing Context - Close with unload prompt" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      -- TODO :: promptUnload doesn't seem to do anything ??
      logTxt "Close browsing context - unload prompt True"
      co <- browsingContextClose $ MkClose {context = bc, promptUnload = Just True}
      logShow "Close result" co
      pause

-- >>> runDemo browsingContextGetTreeDemo
browsingContextGetTreeDemo :: BiDiDemo
browsingContextGetTreeDemo =
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

-- >>> runDemo browsingContextHandleUserPromptDemo
browsingContextHandleUserPromptDemo :: BiDiDemo
browsingContextHandleUserPromptDemo =
  demo "Browsing Context - Handle User Prompt, Evaluate Script" action
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
      -- Wait for the alert to be displayed (in a production automation more sophisticated polling must be used)
      pauseMinMs 500

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
      -- Wait for the alert to be displayed (in a production automation more sophisticated polling must be used)
      pauseMinMs 500

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
      -- Wait for the alert to be displayed (in a productions system, more sophisticated polling would be needed)
      pauseMinMs 500

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
      -- Wait for the alert to be displayed (in a productions system, more sophisticated polling would be needed)
      pauseMinMs 500

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

-- >>> runDemo browsingNavigateReloadTraverseHistoryDemo
browsingNavigateReloadTraverseHistoryDemo :: BiDiDemo
browsingNavigateReloadTraverseHistoryDemo =
  demo "Browsing Context - Navigate, Reload, Traverse History" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      logTxt "Navigate to The Internet - Main Page"
      navResult1 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/", wait = Nothing}
      logShow "Navigation result - Main page" navResult1
      pause

      logTxt "Navigate to Checkboxes page"
      navResult2 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/checkboxes", wait = Just Interactive}
      logShow "Navigation result - Checkboxes" navResult2
      pause

      logTxt "Navigate to JavaScript Alerts page"
      navResult3 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/javascript_alerts", wait = Just Complete}
      logShow "Navigation result - JavaScript Alerts" navResult3
      pause

      logTxt "Navigate to Inputs page"
      navResult4 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/inputs", wait = Just None}
      logShow "Navigation result - Inputs" navResult4
      pause

      logTxt "Reload current page (Inputs) - default options"
      reloadResult1 <- browsingContextReload $ MkReload {context = bc, ignoreCache = Nothing, wait = Nothing}
      logShow "Reload result - default" reloadResult1
      pause

      logTxt "Navigate to Login page"
      navResult5 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/login", wait = Nothing}
      logShow "Navigation result - Login" navResult5
      pause

      -- ignore cache not supported yet in geckodriver
      -- logTxt "Reload current page (Login) - ignore cache"
      -- reloadResult2 <- browsingContextReload $ MkReload {context = bc, ignoreCache = Just True, wait = Nothing}
      -- logShow "Reload result - ignore cache" reloadResult2
      -- pause

      logTxt "Navigate to Infinite Scroll page"
      navResult6 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/infinite_scroll", wait = Nothing}
      logShow "Navigation result - Infinite Scroll" navResult6
      pause

      logTxt "Reload current page (Infinite Scroll) - wait for complete"
      reloadResult3 <- browsingContextReload $ MkReload {context = bc, ignoreCache = Nothing, wait = Just Complete}
      logShow "Reload result - wait complete" reloadResult3
      pause

      logTxt "Test history traversal - Go back 1 step (to Login)"
      historyResult1 <- browsingContextTraverseHistory $ MkTraverseHistory {context = bc, delta = MkJSInt (-1)}
      logShow "History traversal result - back 1" historyResult1
      pause

      logTxt "Go back 2 more steps (to JavaScript Alerts)"
      historyResult2 <- browsingContextTraverseHistory $ MkTraverseHistory {context = bc, delta = MkJSInt (-2)}
      logShow "History traversal result - back 2" historyResult2
      pause

      logTxt "Go back 1 more step (to Checkboxes)"
      historyResult3 <- browsingContextTraverseHistory $ MkTraverseHistory {context = bc, delta = MkJSInt (-1)}
      logShow "History traversal result - back 1" historyResult3
      pause

      logTxt "Go forward 1 step (to JavaScript Alerts)"
      historyResult4 <- browsingContextTraverseHistory $ MkTraverseHistory {context = bc, delta = MkJSInt 1}
      logShow "History traversal result - forward 1" historyResult4
      pause

      logTxt "Go forward 2 steps (to Login)"
      historyResult5 <- browsingContextTraverseHistory $ MkTraverseHistory {context = bc, delta = MkJSInt 2}
      logShow "History traversal result - forward 2" historyResult5
      pause

      logTxt "Go forward 1 step (to Infinite Scroll)"
      historyResult6 <- browsingContextTraverseHistory $ MkTraverseHistory {context = bc, delta = MkJSInt 1}
      logShow "History traversal result - forward 1" historyResult6
      pause

      logTxt "Final navigation - back to main page"
      navResultFinal <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/", wait = Just Complete}
      logShow "Navigation result - back to main" navResultFinal
      pause

      closeContext utils cmds bc

-- >>> runDemo browsingContextLocateNodesDemo

-- *** Exception: Error executing BiDi command: MkCommand

--   { method = "browsingContext.locateNodes"
--   , params =
--       MkLocateNodes
--         { context =
--             MkBrowsingContext
--               { context = "2e6c74d3-7ff5-4c46-ac8e-4abe6f51d1f3" }
--         , locator =
--             Context { context = MkBrowsingContext { context = "19327352833" } }
--         , maxNodeCount = Nothing
--         , serializationOptions = Nothing
--         , startNodes = Nothing
--         }
--   , extended = Nothing
--   }
-- With JSON:
-- {
--     "id": 18,
--     "method": "browsingContext.locateNodes",
--     "params": {
--         "context": "2e6c74d3-7ff5-4c46-ac8e-4abe6f51d1f3",
--         "locator": {
--             "context": "19327352833",
--             "type": "context"
--         }
--     }
-- }
-- Failed to decode the 'result' property of JSON returned by driver to response type:
-- {
--     "error": "invalid argument",
--     "id": 18,
--     "message": "Expected \"locator.type\" to be one of accessibility,css,innerText,xpath, got [object String] \"context\"",
--     "stacktrace": "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:199:5\nInvalidArgumentError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:401:5\nassert.that/<@chrome://remote/content/shared/webdriver/Assert.sys.mjs:581:13\nlocateNodes@chrome://remote/content/webdriver-bidi/modules/root/browsingContext.sys.mjs:1052:6\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:260:33\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:410:32\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n",
--     "type": "error"
-- }
-- Error message:
-- key "result" not found
browsingContextLocateNodesDemo :: BiDiDemo
browsingContextLocateNodesDemo =
  demo "Browsing Context - Locate Nodes with All Selectors and Options" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      logTxt "Navigate to The Internet - Login Page for comprehensive selector testing"
      navResult1 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/login", wait = Just Complete}
      logShow "Navigation result - Login page" navResult1
      pause

      -- Test CSS Selector
      logTxt "Test 1: CSS Selector - Find username input field"
      cssResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "#username"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "CSS Selector result - #username" cssResult
      pause

      logTxt "Test 2: CSS Selector - Find all input elements"
      cssAllInputs <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "input"},
              maxNodeCount = Just (MkJSUInt 5),
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "CSS Selector result - all inputs (max 5)" cssAllInputs
      pause

      -- Test XPath Selector
      logTxt "Test 3: XPath Selector - Find password field by attribute"
      xpathResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = XPath {value = "//input[@name='password']"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "XPath Selector result - password field" xpathResult
      pause

      logTxt "Test 4: XPath Selector - Find login button by text content"
      xpathButtonResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = XPath {value = "//button[contains(text(), 'Login')]"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "XPath Selector result - login button" xpathButtonResult
      pause

      -- Test InnerText Selector
      {- TODO: InnerText not supported yet in geckodriver
      logTxt "Test 5: InnerText Selector - Find element with exact text 'Login'"
      innerTextFullResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator =
                InnerText
                  { value = "Login",
                    ignoreCase = Nothing,
                    matchType = Just Full,
                    maxDepth = Nothing
                  },
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "InnerText Selector result - exact 'Login'" innerTextFullResult
      pause

      logTxt "Test 6: InnerText Selector - Find elements containing 'log' (partial, ignore case)"
      innerTextPartialResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator =
                InnerText
                  { value = "log",
                    ignoreCase = Just True,
                    matchType = Just Partial,
                    maxDepth = Just (MkJSUInt 3)
                  },
              maxNodeCount = Just (MkJSUInt 10),
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "InnerText Selector result - partial 'log' (ignore case)" innerTextPartialResult
      pause
      -}

      -- Test SerializationOptions with different depths
      logTxt "Test 7: CSS Selector with SerializationOptions - Deep DOM traversal"
      cssWithSerializationResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "form"},
              maxNodeCount = Nothing,
              serializationOptions =
                Just $
                  object
                    [ "maxDomDepth" .= (3 :: Int),
                      "maxObjectDepth" .= Null,
                      "includeShadowTree" .= ("none" :: Text)
                    ],
              startNodes = Nothing
            }
      logShow "CSS Selector with SerializationOptions - form with depth 3" cssWithSerializationResult
      pause

      logTxt "Test 8: CSS Selector with different SerializationOptions - Shallow traversal"
      cssShallowResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "body"},
              maxNodeCount = Nothing,
              serializationOptions =
                Just $
                  object
                    [ "maxDomDepth" .= (1 :: Int),
                      "maxObjectDepth" .= (1 :: Int),
                      "includeShadowTree" .= ("open" :: Text)
                    ],
              startNodes = Nothing
            }
      logShow "CSS Selector with SerializationOptions - body with depth 1" cssShallowResult
      pause

      -- Navigate to a page with more accessibility content for better testing
      logTxt "Navigate to The Internet - Frames page for accessibility and context testing"
      navResult2 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/frames", wait = Just Complete}
      logShow "Navigation result - Frames page" navResult2
      pause

      -- Test Accessibility Selector
      logTxt "Test 9: Accessibility Selector - Find elements by role"
      accessibilityRoleResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator =
                Accessibility
                  { name = Nothing,
                    role = Just "link"
                  },
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Accessibility Selector result - role 'link'" accessibilityRoleResult
      pause

      logTxt "Test 10: Accessibility Selector - Find elements by accessible name"
      accessibilityNameResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator =
                Accessibility
                  { name = Just "iFrame",
                    role = Nothing
                  },
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Accessibility Selector result - name 'iFrame'" accessibilityNameResult
      pause

      logTxt "Test 11: Accessibility Selector - Find elements by both name and role"
      accessibilityBothResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator =
                Accessibility
                  { name = Just "Nested Frames",
                    role = Just "link"
                  },
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Accessibility Selector result - name 'Nested Frames' and role 'link'" accessibilityBothResult
      pause

      -- Test startNodes functionality
      logTxt "Test 12: Demonstrating startNodes - First find a parent element, then search within it"
      parentResult <-
        browsingContextLocateNodes $
          MkLocateNodes
            { context = bc,
              locator = CSS {value = "div.example"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      logShow "Parent element search - div.example" parentResult
      pause

      -- Extract sharedId from first result to use as startNode
      case parentResult of
        MkLocateNodesResult nodes -> case nodes of
          (MkNodeRemoteValue {sharedId = Just (MkSharedId nodeId)} : _) -> do
            logTxt "Test 13: Using startNodes - Search for links within the parent element"
            startNodesResult <-
              browsingContextLocateNodes $
                MkLocateNodes
                  { context = bc,
                    locator = CSS {value = "a"},
                    maxNodeCount = Nothing,
                    serializationOptions = Nothing,
                    startNodes = Just [MkSharedReference {sharedId = MkShareId {id = nodeId}, handle = Nothing, extensions = Nothing}]
                  }
            logShow "StartNodes result - links within div.example" startNodesResult
            pause

            logTxt "Test 14: Using startNodes with different selector - Find all text content"
            startNodesTextResult <-
              browsingContextLocateNodes $
                MkLocateNodes
                  { context = bc,
                    locator = XPath {value = ".//text()[normalize-space()]"},
                    maxNodeCount = Just (MkJSUInt 5),
                    serializationOptions =
                      Just $
                        object
                          [ "maxDomDepth" .= (2 :: Int),
                            "maxObjectDepth" .= Null,
                            "includeShadowTree" .= ("none" :: Text)
                          ],
                    startNodes = Just [MkSharedReference {sharedId = MkShareId {id = nodeId}, handle = Nothing, extensions = Nothing}]
                  }
            logShow "StartNodes with XPath - text nodes within parent (max 5)" startNodesTextResult
            pause
          _ -> do
            logTxt "No sharedId found in first result, skipping startNodes test"
            pause

      -- Navigate to a simpler page for Context selector test
      logTxt "Navigate to The Internet - Nested Frames for Context selector testing"
      navResult3 <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/nested_frames", wait = Just Complete}
      logShow "Navigation result - Nested Frames page" navResult3
      pause

      -- Get child browsing contexts (frames) for Context selector
      logTxt "Get browsing context tree to find child frames"
      treeResult <- browsingContextGetTree $ MkGetTree Nothing Nothing
      logShow "Browsing context tree" treeResult
      pause

-- Test Context Selector (this requires child browsing contexts like iframes)
{- TODO: Context selector not supported yet in geckodriver
  case treeResult of
    MkGetTreeResult infoList -> do
      -- Look for any child contexts in the tree
      let allChildContexts = concatMap extractChildContexts infoList
      case allChildContexts of
        (childContext : _) -> do
          logTxt "Test 15: Context Selector - Find container element of iframe"
          contextResult <-
            browsingContextLocateNodes $
              MkLocateNodes
                { context = bc,
                  locator = Context {context = childContext},
                  maxNodeCount = Nothing,
                  serializationOptions = Nothing,
                  startNodes = Nothing
                }
          logShow "Context Selector result - iframe container" contextResult
          pause
        [] -> do
          logTxt "No child browsing contexts found, skipping Context selector test"
          logTxt "Note: Context selector finds the DOM element that contains an iframe"
          pause

  pause

-- Helper function to extract child contexts from browse context tree
extractChildContexts :: Info -> [BrowsingContext]
extractChildContexts =
  ( maybe
      []
      \childInfos ->
        ((.context) <$> childInfos)
          <> concatMap extractChildContexts childInfos
  )
    . (.children)
  -}

-- >>> runDemo browsingContextPrintAndSetViewportDemo
browsingContextPrintAndSetViewportDemo :: BiDiDemo
browsingContextPrintAndSetViewportDemo =
  demo "Browsing Context - Print and Set Viewport" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- newWindowContext utils cmds

      logTxt "Navigate to The Internet - Main Page for content"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      -- Test Print with different parameters
      logTxt "Test 1: Print - Default settings"
      printDefault <- browsingContextPrint $ MkPrint {context = bc, background = Nothing, margin = Nothing, orientation = Nothing, page = Nothing, pageRanges = Nothing, scale = Nothing, shrinkToFit = Nothing}
      logTxt $ "Print result - default settings: " <> case printDefault of MkPrintResult {base64Text} -> base64Text
      pause

      logTxt "Test 2: Print - Portrait orientation with custom margins"
      printPortraitMargins <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Just False,
              margin =
                Just $
                  MkPrintMargin
                    { bottom = Just 2.0,
                      left = Just 1.5,
                      right = Just 1.5,
                      top = Just 2.0
                    },
              orientation = Just Portrait,
              page = Nothing,
              pageRanges = Nothing,
              scale = Nothing,
              shrinkToFit = Nothing
            }
      logTxt $ "Print result - portrait with margins: " <> case printPortraitMargins of MkPrintResult {base64Text} -> base64Text
      pause

      logTxt "Test 3: Print - Landscape orientation with custom page size"
      printLandscapePage <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Just True,
              margin = Nothing,
              orientation = Just Landscape,
              page =
                Just $
                  MkPrintPage
                    { height = Just 29.7, -- A4 height in cm
                      width = Just 21.0 -- A4 width in cm
                    },
              pageRanges = Nothing,
              scale = Nothing,
              shrinkToFit = Nothing
            }
      logTxt $ "Print result - landscape with page size: " <> case printLandscapePage of MkPrintResult {base64Text} -> base64Text
      pause

      logTxt "Test 4: Print - Custom scale and shrink to fit"
      printScaled <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Nothing,
              margin = Nothing,
              orientation = Nothing,
              page = Nothing,
              pageRanges = Nothing,
              scale = Just 0.8,
              shrinkToFit = Just True
            }
      logTxt $ "Print result - scaled 80%: " <> case printScaled of MkPrintResult {base64Text} -> base64Text
      pause

      -- Test SetViewport with different parameters
      logTxt "Test 5: Set Viewport - Standard desktop size"
      setViewportDesktop <-
        browsingContextSetViewport $
          MkSetViewport
            { context = Just bc,
              viewport =
                Just $
                  Just $
                    MkViewport
                      { width = MkJSUInt 1920,
                        height = MkJSUInt 1080
                      },
              devicePixelRatio = Nothing,
              userContexts = Nothing
            }
      logShow "Set viewport result - desktop" setViewportDesktop
      pause

      logTxt "Test 6: Set Viewport - Mobile size with device pixel ratio"
      setViewportMobile <-
        browsingContextSetViewport $
          MkSetViewport
            { context = Just bc,
              viewport =
                Just $
                  Just $
                    MkViewport
                      { width = MkJSUInt 375,
                        height = MkJSUInt 812
                      },
              devicePixelRatio = Just (Just 2.0),
              userContexts = Nothing
            }
      logShow "Set viewport result - mobile" setViewportMobile
      pause

      logTxt "Test 7: Set Viewport - Tablet size"
      setViewportTablet <-
        browsingContextSetViewport $
          MkSetViewport
            { context = Just bc,
              viewport =
                Just $
                  Just $
                    MkViewport
                      { width = MkJSUInt 768,
                        height = MkJSUInt 1024
                      },
              devicePixelRatio = Nothing,
              userContexts = Nothing
            }
      logShow "Set viewport result - tablet" setViewportTablet
      pause

      logTxt "Test 8: Set Viewport - Reset to null (default)"
      setViewportNull <-
        browsingContextSetViewport $
          MkSetViewport
            { context = Just bc,
              viewport = Just Nothing, -- Explicitly set to null
              devicePixelRatio = Just Nothing, -- Reset device pixel ratio too
              userContexts = Nothing
            }
      logShow "Set viewport result - reset to null" setViewportNull
      pause

      logTxt "Test 9: Print after viewport changes - Should reflect new dimensions"
      printAfterViewport <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Nothing,
              margin = Nothing,
              orientation = Just Portrait,
              page = Nothing,
              pageRanges = Nothing,
              scale = Just 1.0,
              shrinkToFit = Just False
            }
      logTxt $ "Print result - after viewport changes: " <> case printAfterViewport of MkPrintResult {base64Text} -> base64Text
      pause

      -- Navigate to a longer page to demonstrate pageRanges effectively
      logTxt "Navigate to a longer page for pageRanges demonstration"
      navResultLong <- browsingContextNavigate $ MkNavigate {context = bc, url = "https://the-internet.herokuapp.com/large", wait = Just Complete}
      logShow "Navigation result - large page" navResultLong
      pause

      logTxt "Test 10: Print with pageRanges - Only first page"
      printFirstPage <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Nothing,
              margin = Nothing,
              orientation = Nothing,
              page = Nothing,
              pageRanges = Just [Page 1],
              scale = Nothing,
              shrinkToFit = Nothing
            }
      logTxt $ "Print result - first page only: " <> case printFirstPage of MkPrintResult {base64Text} -> base64Text
      pause

      logTxt "Test 11: Print with pageRanges - Pages 1-2"
      printPageRange <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Nothing,
              margin = Nothing,
              orientation = Nothing,
              page = Nothing,
              pageRanges = Just $ [Range {fromPage = 1, toPage = 2}],
              scale = Nothing,
              shrinkToFit = Nothing
            }
      logTxt $ "Print result - pages 1-2: " <> case printPageRange of MkPrintResult {base64Text} -> base64Text
      pause

      logTxt "Test 12: Print with pageRanges - Specific pages (1,3)"
      printSpecificPages <-
        browsingContextPrint $
          MkPrint
            { context = bc,
              background = Nothing,
              margin = Nothing,
              orientation = Nothing,
              page = Nothing,
              pageRanges = Just [Range 1 2, Page 3],
              scale = Nothing,
              shrinkToFit = Nothing
            }
      logTxt $ "Print result - pages 1 and 3: " <> case printSpecificPages of MkPrintResult {base64Text} -> base64Text
      pause

      closeContext utils cmds bc
