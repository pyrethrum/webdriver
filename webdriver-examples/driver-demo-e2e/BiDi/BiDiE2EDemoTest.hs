module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), mkDemoBiDiClientParams, mkFailBidiClientParams, withCommands)
import Control.Exception (Exception, throwIO)
import Data.Aeson (ToJSON (..), Value (Null), object, (.=))
import Data.Text (Text, isInfixOf)
import Data.Word (Word64)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.BrowsingContext (Locator (..), PrintMargin (..), PrintPage (..), Viewport (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..), NodeRemoteValue (..), SharedId (..))
import WebDriverPreCore.BiDi.Script (Channel (..), ChannelProperties (..), ChannelValue (..), EvaluateResult (..), PrimitiveProtocolValue (..), RemoteValue (..))
import WebDriverPreCore.BiDi.Protocol
  ( Activate (MkActivate),
    AddPreloadScript (..),
    AddPreloadScriptResult (..),
    BrowsingContext,
    CaptureScreenshot
      ( MkCaptureScreenshot,
        clip,
        context,
        format,
        origin
      ),
    ClipRectangle (BoxClipRectangle, height, width, x, y),
    Close (MkClose, context, promptUnload),
    ContextTarget (MkContextTarget, context, sandbox),
    Create
      ( MkCreate,
        background,
        createType,
        referenceContext,
        userContext
      ),
    CreateType (Tab, Window),
    CreateUserContext
      ( MkCreateUserContext,
        acceptInsecureCerts,
        proxy,
        unhandledPromptBehavior
      ),
    Evaluate
      ( MkEvaluate,
        awaitPromise,
        expression,
        resultOwnership,
        serializationOptions,
        target
      ),

    GetTree (MkGetTree, maxDepth, root),
    GetTreeResult (MkGetTreeResult),
    HandleUserPrompt (MkHandleUserPrompt, accept, context, userText),
    ImageFormat (MkImageFormat, imageType, quality),
    IncludeShadowTree (All, Open, ShadowTreeNone),
    Info (context),
    LocateNodes
      ( MkLocateNodes,
        context,
        locator,
        maxNodeCount,
        serializationOptions,
        startNodes
      ),
    LocateNodesResult (MkLocateNodesResult), 
    Navigate (MkNavigate, context, url, wait),
    Orientation (Landscape, Portrait),
    PageRange (Page, Range, fromPage, toPage),
    Print
      ( MkPrint,
        background,
        context,
        margin,
        orientation,
        page,
        pageRanges,
        scale,
        shrinkToFit
      ),
    PrintResult (MkPrintResult, base64Text),
    ReadinessState (Complete, Interactive, None),
    Reload (MkReload, context, ignoreCache, wait),
    RemovePreloadScript (MkRemovePreloadScript),
    ResultOwnership (Root),
    Sandbox (MkSandbox),
    ScreenShotOrigin (Document, Viewport),
    SerializationOptions
      ( MkSerializationOptions,
        includeShadowTree,
        maxDomDepth,
        maxObjectDepth
      ),
    SetViewport
      ( MkSetViewport,
        context,
        devicePixelRatio,
        userContexts,
        viewport
      ),
    SharedId (MkShareId, id),
    SharedReference (MkSharedReference, extensions, handle, sharedId),
    Target (ContextTarget),
    TraverseHistory (MkTraverseHistory, context, delta),
  )
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
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

rootContext :: DemoUtils -> Commands -> IO BrowsingContext
rootContext MkDemoUtils {..} MkCommands {..} = do
  logTxt "Get root browsing context"
  tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
  logShow "Browsing context tree" tree
  case tree of
    MkGetTreeResult (info : _) -> pure $ info.context
    _ -> error "No browsing contexts found"

-- | Custom exception for text validation failures
data TextValidationError = TextValidationError
  { expectedText :: Text,
    actualText :: Text
  }
  deriving (Show)

instance Exception TextValidationError

-- | Check if expected text is present in DOM, throw error if not found
chkText :: DemoUtils -> Commands -> BrowsingContext -> Text -> IO ()
chkText MkDemoUtils {..} MkCommands {..} bc expectedText = do
  logTxt $ "Checking DOM contains: " <> expectedText
  
  -- Get the full DOM text content
  domResult <- scriptEvaluate $ 
    MkEvaluate
      { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
        target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
        awaitPromise = False,
        resultOwnership = Nothing,
        serializationOptions = Nothing
      }
  
  case domResult of
    EvaluateResultSuccess {result = PrimitiveValue (StringValue actualText)} -> do
      if expectedText `isInfixOf` actualText
        then logTxt $ "✓ Found expected text: " <> expectedText
        else do
          logTxt $ "✗ Expected text not found!"
          logTxt $ "Expected: " <> expectedText
          logTxt $ "Actual DOM text: " <> actualText
          throwIO $ TextValidationError {expectedText, actualText}
    
    EvaluateResultSuccess {result = otherResult} -> do
      logShow "Unexpected result type" otherResult
      throwIO $ TextValidationError {expectedText, actualText = "Non-string result"}
    
    EvaluateResultException {exceptionDetails} -> do
      logShow "Script evaluation failed" exceptionDetails
      throwIO $ TextValidationError {expectedText, actualText = "Script evaluation failed"}

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
4. scriptEvaluate :: DONE
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
-- ############################  Browsing Context ############################
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
      bc <- rootContext utils cmds

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
  demo "Browsing Context - Close with Unload Prompt" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

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
      bc <- rootContext utils cmds

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
      bc <- rootContext utils cmds

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

-- >>> runDemo browsingContextLocateNodesDemo
browsingContextLocateNodesDemo :: BiDiDemo
browsingContextLocateNodesDemo =
  demo "Browsing Context - Locate Nodes with All Selectors and Options" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

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
      bc <- rootContext utils cmds

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

-- ###########################################################################
-- ################################# Script ##################################
-- ###########################################################################

-- >>> runDemo scriptEvaluateAllPrimitiveTypesDemo
scriptEvaluateAllPrimitiveTypesDemo :: BiDiDemo
scriptEvaluateAllPrimitiveTypesDemo =
  demo "Script - Evaluate All PrimitiveProtocolValue Types" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      let baseEval =
            MkEvaluate
              { expression = "alert('Hello from Pyrethrum BiDi!')",
                target =
                  ContextTarget $
                    MkContextTarget
                      { context = bc,
                        sandbox = Nothing
                      },
                awaitPromise = True,
                resultOwnership = Nothing,
                serializationOptions = Nothing
              }

      logTxt "Test 1: Undefined evaluation - returns UndefinedValue"
      r1 <- scriptEvaluate $ baseEval {expression = "undefined"}
      logShow "Script evaluation result - undefined" r1
      pause

      logTxt "Test 2: Null evaluation - returns NullValue"
      r2 <- scriptEvaluate $ baseEval {expression = "null"}
      logShow "Script evaluation result - null" r2
      pause

      logTxt "Test 3: String evaluation - returns StringValue"
      r3 <- scriptEvaluate $ baseEval {expression = "'Hello from BiDi Script!'"}
      logShow "Script evaluation result - string" r3
      pause

      logTxt "Test 4: String evaluation with escape characters"
      r4 <- scriptEvaluate $ baseEval {expression = "'Line 1\\nLine 2\\tTabbed'"}
      logShow "Script evaluation result - string with escapes" r4
      pause

      logTxt "Test 5: Number evaluation - integer"
      r5 <- scriptEvaluate $ baseEval {expression = "42"}
      logShow "Script evaluation result - number (integer)" r5
      pause

      logTxt "Test 6: Number evaluation - float"
      r6 <- scriptEvaluate $ baseEval {expression = "3.14159"}
      logShow "Script evaluation result - number (float)" r6
      pause

      logTxt "Test 7: Number evaluation - negative"
      r7 <- scriptEvaluate $ baseEval {expression = "-123.456"}
      logShow "Script evaluation result - number (negative)" r7
      pause

      logTxt "Test 8: Number evaluation - zero"
      r8 <- scriptEvaluate $ baseEval {expression = "0"}
      logShow "Script evaluation result - number (zero)" r8
      pause

      logTxt "Test 9: Special Number - NaN"
      r9 <- scriptEvaluate $ baseEval {expression = "NaN"}
      logShow "Script evaluation result - NaN" r9
      pause

      logTxt "Test 10: Special Number - Negative Zero"
      r10 <- scriptEvaluate $ baseEval {expression = "-0"}
      logShow "Script evaluation result - negative zero" r10
      pause

      logTxt "Test 11: Special Number - Infinity"
      r11 <- scriptEvaluate $ baseEval {expression = "Infinity"}
      logShow "Script evaluation result - Infinity" r11
      pause

      logTxt "Test 12: Special Number - Negative Infinity"
      r12 <- scriptEvaluate $ baseEval {expression = "-Infinity"}
      logShow "Script evaluation result - Negative Infinity" r12
      pause

      logTxt "Test 13: Special Number - Division by zero (Infinity)"
      r13 <- scriptEvaluate $ baseEval {expression = "1 / 0"}
      logShow "Script evaluation result - 1/0 = Infinity" r13
      pause

      logTxt "Test 14: Special Number - Invalid operation (NaN)"
      r14 <- scriptEvaluate $ baseEval {expression = "Math.sqrt(-1)"}
      logShow "Script evaluation result - sqrt(-1) = NaN" r14
      pause

      logTxt "Test 15: Boolean evaluation - true"
      r15 <- scriptEvaluate $ baseEval {expression = "true"}
      logShow "Script evaluation result - boolean true" r15
      pause

      logTxt "Test 16: Boolean evaluation - false"
      r16 <- scriptEvaluate $ baseEval {expression = "false"}
      logShow "Script evaluation result - boolean false" r16
      pause

      logTxt "Test 17: Boolean evaluation - truthy expression"
      r17 <- scriptEvaluate $ baseEval {expression = "!!'hello'"}
      logShow "Script evaluation result - !!string = true" r17
      pause

      logTxt "Test 18: Boolean evaluation - falsy expression"
      r18 <- scriptEvaluate $ baseEval {expression = "!!0"}
      logShow "Script evaluation result - !!0 = false" r18
      pause

      logTxt "Test 19: BigInt evaluation - small BigInt"
      r19 <- scriptEvaluate $ baseEval {expression = "42n"}
      logShow "Script evaluation result - BigInt 42n" r19
      pause

      logTxt "Test 20: BigInt evaluation - large BigInt"
      r20 <- scriptEvaluate $ baseEval {expression = "9007199254740991n"}
      logShow "Script evaluation result - BigInt (Number.MAX_SAFE_INTEGER)" r20
      pause

      logTxt "Test 21: BigInt evaluation - very large BigInt"
      r21 <- scriptEvaluate $ baseEval {expression = "12345678901234567890123456789012345678901234567890n"}
      logShow "Script evaluation result - very large BigInt" r21
      pause

      logTxt "Test 22: BigInt evaluation - negative BigInt"
      r22 <- scriptEvaluate $ baseEval {expression = "-9007199254740991n"}
      logShow "Script evaluation result - negative BigInt" r22
      pause

      logTxt "Test 23: BigInt evaluation - zero BigInt"
      r23 <- scriptEvaluate $ baseEval {expression = "0n"}
      logShow "Script evaluation result - BigInt zero" r23
      pause

      logTxt "Test 24: BigInt evaluation - BigInt arithmetic"
      r24 <- scriptEvaluate $ baseEval {expression = "BigInt(123) * BigInt(456)"}
      logShow "Script evaluation result - BigInt arithmetic" r24
      pause

      logTxt "Test 25: Complex expression evaluation - mixed types in comparison"
      r25 <- scriptEvaluate $ baseEval {expression = "typeof 'string' === 'string'"}
      logShow "Script evaluation result - typeof comparison" r25
      pause

      logTxt "Test 26: Complex expression evaluation - Number.isNaN"
      r26 <- scriptEvaluate $ baseEval {expression = "Number.isNaN(NaN)"}
      logShow "Script evaluation result - Number.isNaN(NaN)" r26
      pause

      logTxt "Test 27: Complex expression evaluation - Number.isFinite"
      r27 <- scriptEvaluate $ baseEval {expression = "Number.isFinite(42)"}
      logShow "Script evaluation result - Number.isFinite(42)" r27
      pause

      logTxt "Test 28: Empty string evaluation"
      r28 <- scriptEvaluate $ baseEval {expression = "''"}
      logShow "Script evaluation result - empty string" r28
      pause

      logTxt "Test 29: Unicode string evaluation"
      r29 <- scriptEvaluate $ baseEval {expression = "'Hello 🌍 World! αβγ 中文'"}
      logShow "Script evaluation result - unicode string" r29
      pause

      logTxt "Test 30: Mathematical constants"
      r30 <- scriptEvaluate $ baseEval {expression = "Math.PI"}
      logShow "Script evaluation result - Math.PI" r30
      pause

-- >>> runDemo scriptEvaluateAdvancedDemo
scriptEvaluateAdvancedDemo :: BiDiDemo
scriptEvaluateAdvancedDemo =
  demo "Script - Evaluate Advanced Types and Edge Cases" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds
      let baseEval =
            MkEvaluate
              { expression = "",
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

      logTxt "Advanced Test 1: Array evaluation (non-primitive)"
      a1 <- scriptEvaluate $ baseEval {expression = "[1, 2, 3, 'hello', true, null]"}
      logShow "Script evaluation result - array" a1
      pause

      logTxt "Advanced Test 2: Object evaluation (non-primitive)"
      a2 <- scriptEvaluate $ baseEval {expression = "{ name: 'BiDi', version: 1.0, active: true }"}
      logShow "Script evaluation result - object" a2
      pause

      logTxt "Advanced Test 3: Function evaluation (non-primitive)"
      a3 <- scriptEvaluate $ baseEval {expression = "function greet(name) { return 'Hello, ' + name; }"}
      logShow "Script evaluation result - function" a3
      pause

      logTxt "Advanced Test 4: Date evaluation (non-primitive)"
      a4 <- scriptEvaluate $ baseEval {expression = "new Date('2024-01-15T10:30:00Z')"}
      logShow "Script evaluation result - date" a4
      pause

      logTxt "Advanced Test 5: RegExp evaluation (non-primitive)"
      a5 <- scriptEvaluate $ baseEval {expression = "/^[a-z]+$/gi"}
      logShow "Script evaluation result - regexp" a5
      pause

      logTxt "Advanced Test 6: Promise evaluation - resolved (awaitPromise=False)"
      a6 <- scriptEvaluate $ baseEval {expression = "Promise.resolve('resolved value')"}
      logShow "Script evaluation result - promise (not awaited)" a6
      pause

      logTxt "Advanced Test 7: Promise evaluation - resolved (awaitPromise=True)"
      a7 <- scriptEvaluate $ baseEval {expression = "Promise.resolve('resolved value')", awaitPromise = True}
      logShow "Script evaluation result - promise (awaited)" a7
      pause

      logTxt "Advanced Test 8: Symbol evaluation (non-primitive)"
      a8 <- scriptEvaluate $ baseEval {expression = "Symbol('test-symbol')"}
      logShow "Script evaluation result - symbol" a8
      pause

      logTxt "Advanced Test 9: Error evaluation"
      a9 <- scriptEvaluate $ baseEval {expression = "new Error('Test error message')"}
      logShow "Script evaluation result - error object" a9
      pause

      logTxt "Advanced Test 10: Throw error evaluation (should produce exception result)"
      a10 <- scriptEvaluate $ baseEval {expression = "throw new Error('Intentional test error')"}
      logShow "Script evaluation result - thrown error" a10
      pause

      logTxt "Advanced Test 11: DOM element evaluation (if available)"
      a11 <- scriptEvaluate $ baseEval {expression = "document.body || 'no document.body'"}
      logShow "Script evaluation result - DOM element or fallback" a11
      pause

      logTxt "Advanced Test 12: Window proxy evaluation"
      a12 <- scriptEvaluate $ baseEval {expression = "window"}
      logShow "Script evaluation result - window proxy" a12
      pause

      logTxt "Advanced Test 13: Map evaluation (ES6 collection)"
      a13 <- scriptEvaluate $ baseEval {expression = "new Map([['key1', 'value1'], ['key2', 42]])"}
      logShow "Script evaluation result - Map" a13
      pause

      logTxt "Advanced Test 14: Set evaluation (ES6 collection)"
      a14 <- scriptEvaluate $ baseEval {expression = "new Set([1, 2, 3, 1, 2])"}
      logShow "Script evaluation result - Set" a14
      pause

      logTxt "Advanced Test 15: WeakMap evaluation (ES6 collection)"
      a15 <- scriptEvaluate $ baseEval {expression = "new WeakMap()"}
      logShow "Script evaluation result - WeakMap" a15
      pause

      logTxt "Advanced Test 16: WeakSet evaluation (ES6 collection)"
      a16 <- scriptEvaluate $ baseEval {expression = "new WeakSet()"}
      logShow "Script evaluation result - WeakSet" a16
      pause

      logTxt "Advanced Test 17: Generator function evaluation"
      a17 <- scriptEvaluate $ baseEval {expression = "function* gen() { yield 1; yield 2; }"}
      logShow "Script evaluation result - generator function" a17
      pause

      logTxt "Advanced Test 18: Generator evaluation"
      a18 <- scriptEvaluate $ baseEval {expression = "(function* gen() { yield 1; yield 2; })()"}
      logShow "Script evaluation result - generator" a18
      pause

      logTxt "Advanced Test 19: Proxy evaluation"
      a19 <- scriptEvaluate $ baseEval {expression = "new Proxy({}, { get: (target, prop) => 'proxied: ' + prop })"}
      logShow "Script evaluation result - proxy" a19
      pause

      logTxt "Advanced Test 20: ArrayBuffer evaluation (typed arrays)"
      a20 <- scriptEvaluate $ baseEval {expression = "new ArrayBuffer(16)"}
      logShow "Script evaluation result - ArrayBuffer" a20
      pause

      logTxt "Advanced Test 21: Typed array evaluation (Uint8Array)"
      a21 <- scriptEvaluate $ baseEval {expression = "new Uint8Array([1, 2, 3, 4, 5])"}
      logShow "Script evaluation result - Uint8Array" a21
      pause

      logTxt "Advanced Test 22: Complex expression with mixed primitive types"
      a22 <- scriptEvaluate $ baseEval {expression = "({ str: 'text', num: 42, bool: true, nul: null, undef: undefined, big: 123n })"}
      logShow "Script evaluation result - object with all primitive types" a22
      pause

      logTxt "Advanced Test 23: Serialization options test - limited depth"
      a23 <-
        scriptEvaluate $
          baseEval
            { expression = "({ level1: { level2: { level3: { deep: 'value' } } } })",
              serializationOptions =
                Just $
                  MkSerializationOptions
                    { maxDomDepth = Just (Just (MkJSUInt 2)),
                      maxObjectDepth = Just (Just (MkJSUInt 1)),
                      includeShadowTree = Just ShadowTreeNone
                    }
            }
      logShow "Script evaluation result - limited serialization depth" a23
      pause

      logTxt "Advanced Test 24: Result ownership test"
      a24 <-
        scriptEvaluate $
          baseEval
            { expression = "({ data: 'for ownership test' })",
              resultOwnership = Just Root
            }
      logShow "Script evaluation result - with ownership" a24
      pause

      logTxt "Advanced Test 25: Sandbox evaluation"
      a25 <-
        scriptEvaluate $
          baseEval
            { expression = "typeof sandbox_test_var",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Just $ MkSandbox "test-sandbox"}
            }
      logShow "Script evaluation result - in sandbox" a25
      pause

-- TODO: Move to test  - this is special because of nested maybe in type
-- >>> runDemo serializationOptionsDemo
serializationOptionsDemo :: BiDiDemo
serializationOptionsDemo =
  demo "Serialization Options - Various Configurations" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} _cmds = do
      let logJSON hdr = log (hdr <> ":\n") . jsonToText . toJSON

      logJSON "JSON for Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxDomDepth serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just (Just (MkJSUInt 3)),
            maxObjectDepth = Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxDomDepth Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxObjectDepth serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Just (Just (MkJSUInt 2)),
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxObjectDepth Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Just Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for includeShadowTree None serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Just ShadowTreeNone
          }
      pause

      logJSON "JSON for includeShadowTree Open serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Just Open
          }
      pause

      logJSON "JSON for includeShadowTree All serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Just All
          }
      pause

      logJSON "JSON for all options set serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just (Just (MkJSUInt 5)),
            maxObjectDepth = Just (Just (MkJSUInt 3)),
            includeShadowTree = Just Open
          }
      pause

      logJSON "JSON for all options Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just Nothing,
            maxObjectDepth = Just Nothing,
            includeShadowTree = Just ShadowTreeNone
          }
      pause

-- >>> runDemo scriptPreloadScriptDemo
-- *** Exception: Error executing BiDi command: MkCommand
--   { method = "script.addPreloadScript"
--   , params =
--       MkAddPreloadScript
--         { functionDeclaration =
--             "function(channelArg) {   window.addEventListener('load', function() {     var channelInfo = document.createElement('div');     channelInfo.id = 'channel-info';     channelInfo.style.cssText = 'background: lightgreen; padding: 10px; margin: 5px; border: 2px solid green;';     channelInfo.innerHTML = '<strong>Channel Argument:</strong> Received channel for communication';     document.body.appendChild(channelInfo);         // Store channel reference for potential communication     if (channelArg && typeof channelArg === 'object') {       window.testChannel = channelArg;       console.log('Channel argument received:', channelArg);     }   }); }"
--         , arguments =
--             Just
--               [ MkChannelValue
--                   { typ = "channel"
--                   , value =
--                       MkChannelProperties
--                         { channel = "test-channel"
--                         , serializationOptions = Nothing
--                         , ownership = Nothing
--                         }
--                   }
--               ]
--         , contexts =
--             Just
--               [ MkBrowsingContext
--                   { context = "c9bc3566-f05a-435a-9e38-d3bbb537d9df" }
--               ]
--         , sandbox = Nothing
--         }
--   , extended = Nothing
--   }
-- With JSON: 
-- {
--     "id": 5,
--     "method": "script.addPreloadScript",
--     "params": {
--         "arguments": [
--             {
--                 "type": "channel",
--                 "value": {
--                     "channel": "test-channel",
--                     "ownership": null,
--                     "serializationOptions": null
--                 }
--             }
--         ],
--         "contexts": [
--             "c9bc3566-f05a-435a-9e38-d3bbb537d9df"
--         ],
--         "functionDeclaration": "function(channelArg) {   window.addEventListener('load', function() {     var channelInfo = document.createElement('div');     channelInfo.id = 'channel-info';     channelInfo.style.cssText = 'background: lightgreen; padding: 10px; margin: 5px; border: 2px solid green;';     channelInfo.innerHTML = '<strong>Channel Argument:</strong> Received channel for communication';     document.body.appendChild(channelInfo);         // Store channel reference for potential communication     if (channelArg && typeof channelArg === 'object') {       window.testChannel = channelArg;       console.log('Channel argument received:', channelArg);     }   }); }"
--     }
-- }
-- Failed to decode the 'result' property of JSON returned by driver to response type: 
-- {
--     "error": "invalid argument",
--     "id": 5,
--     "message": "Expected \"options\" to be an object, got [object Null] null",
--     "stacktrace": "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:199:5\nInvalidArgumentError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:401:5\nassert.that/<@chrome://remote/content/shared/webdriver/Assert.sys.mjs:581:13\nassert.object@chrome://remote/content/shared/webdriver/Assert.sys.mjs:454:10\nsetDefaultAndAssertSerializationOptions@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:966:15\n#assertChannelArgument@chrome://remote/content/webdriver-bidi/modules/root/script.sys.mjs:768:10\naddPreloadScript/<@chrome://remote/content/webdriver-bidi/modules/root/script.sys.mjs:198:34\naddPreloadScript@chrome://remote/content/webdriver-bidi/modules/root/script.sys.mjs:193:22\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:260:33\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:410:32\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n",
--     "type": "error"
-- }
-- Error message: 
-- key "result" not found
scriptPreloadScriptDemo :: BiDiDemo
scriptPreloadScriptDemo =
  -- TODO: add  ? userContexts: [+browser.UserContext],
  demo "Script - Comprehensive MkAddPreloadScript Properties Demo" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      {- This demo comprehensively demonstrates all MkAddPreloadScript properties:
         1. functionDeclaration: JavaScript function to execute on page load
         2. arguments: Maybe [ChannelValue] - Channel-based communication arguments  
         3. contexts: Maybe [BrowsingContext] - Specific contexts vs global (Nothing)
         4. sandbox: Maybe Text - Script isolation in named sandboxes vs no isolation (Nothing)
         
         Additional features demonstrated:
         - DOM validation using chkText function for verification
         - Multiple browsing contexts with context-specific scripts
         - Script removal and cleanup verification
         - Sandbox isolation testing
         - Channel arguments for script communication
      -}
      bc <- rootContext utils cmds
      let chkDOM = chkText utils cmds bc

      logTxt "Navigate to a simple test page"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Preload Script Test</title></head><body><h1>Test Page</h1><p id='content'>Original content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Basic preload script with functionDeclaration and contexts"
      preloadScript1 <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var indicator = document.getElementById('preload-indicator'); \
                \    if (indicator) { \
                \      indicator.innerHTML = '<p style=\"color: green; font-weight: bold;\">✓ Basic Preload Script executed!</p>'; \
                \    } \
                \    var content = document.getElementById('content'); \
                \    if (content) { \
                \      content.style.backgroundColor = 'lightblue'; \
                \      content.innerHTML = 'Content modified by basic preload script!'; \
                \    } \
                \  }); \
                \}",
              arguments = Nothing,
              contexts = Just [bc],
              sandbox = Nothing
            }
      logShow "Basic preload script added" preloadScript1
      pause

      logTxt "Test 2: Preload script with sandbox isolation"
      preloadScriptSandbox <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.sandboxedVariable = 'This should be isolated in sandbox'; \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var notice = document.createElement('div'); \
                \    notice.id = 'sandbox-notice'; \
                \    notice.style.cssText = 'background: yellow; padding: 10px; margin: 5px; border: 2px solid orange;'; \
                \    notice.innerHTML = '<strong>Sandboxed Script:</strong> Executed in isolated environment'; \
                \    document.body.appendChild(notice); \
                \  }); \
                \}",
              arguments = Nothing,
              contexts = Just [bc],
              sandbox = Just "isolated-sandbox"
            }
      logShow "Sandboxed preload script added" preloadScriptSandbox
      pause

      logTxt "Test 3: Preload script with arguments (ChannelValue for communication)"
      let channel = Channel "test-channel"
          channelValue = MkChannelValue 
            { typ = "channel",
              value = MkChannelProperties
                { channel = channel,
                  serializationOptions = Nothing,
                  ownership = Nothing
                }
            }
      
      preloadScriptWithArgs <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function(channelArg) { \
                \  window.addEventListener('load', function() { \
                \    var channelInfo = document.createElement('div'); \
                \    channelInfo.id = 'channel-info'; \
                \    channelInfo.style.cssText = 'background: lightgreen; padding: 10px; margin: 5px; border: 2px solid green;'; \
                \    channelInfo.innerHTML = '<strong>Channel Argument:</strong> Received channel for communication'; \
                \    document.body.appendChild(channelInfo); \
                \    \
                \    // Store channel reference for potential communication \
                \    if (channelArg && typeof channelArg === 'object') { \
                \      window.testChannel = channelArg; \
                \      console.log('Channel argument received:', channelArg); \
                \    } \
                \  }); \
                \}",
              arguments = Just [channelValue],
              contexts = Just [bc],
              sandbox = Nothing
            }
      logShow "Preload script with channel arguments added" preloadScriptWithArgs
      pause

      logTxt "Test 4: Preload script with all contexts (global scope)"
      preloadScript2 <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.PRELOADED_DATA = { \
                \    message: 'Hello from global preload script!', \
                \    timestamp: new Date().toISOString() \
                \  }; \
                \  console.log('Global Preload Script: Added global data', window.PRELOADED_DATA); \
                \}",
              arguments = Nothing,
              contexts = Nothing,  -- Apply to all contexts
              sandbox = Nothing
            }
      logShow "Global preload script added" preloadScript2
      pause

      logTxt "Test 5: Navigate to a new page to see all preload scripts in action"  
      navResult2 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>New Page</title></head><body><h1>New Test Page</h1><p id='content'>Fresh content</p><div id='preload-indicator'></div><div id='data-display'></div></body></html>", wait = Just Complete}
      logShow "Navigation result to new page" navResult2
      pause

      -- Wait a bit for the preload scripts to execute
      pauseMinMs 1500

      logTxt "Test 6: Check if basic preload script executed (DOM modifications)"
      chkDOM "✓ Basic Preload Script executed!"
      chkDOM "Content modified by basic preload script!"
      pause

      logTxt "Test 7: Check if sandboxed script executed (isolated execution)"
      chkDOM "Sandboxed Script:"
      chkDOM "Executed in isolated environment"
      pause

      logTxt "Test 8: Check if channel argument script executed"
      chkDOM "Channel Argument:"
      chkDOM "Received channel for communication"
      pause

      logTxt "Test 9: Check if global preload script executed (all contexts)"
      -- Check if global data was added by evaluating a script that outputs to DOM
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "if (window.PRELOADED_DATA) { \
                         \  document.body.innerHTML += '<div id=\"global-data-check\">Global data present: ' + JSON.stringify(window.PRELOADED_DATA) + '</div>'; \
                         \} else { \
                         \  document.body.innerHTML += '<div id=\"global-data-check\">No global data found</div>'; \
                         \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseMinMs 500
      chkDOM "Global data present:"
      chkDOM "Hello from global preload script!"
      pause

      logTxt "Test 10: Verify sandbox isolation by checking if sandboxed variables are isolated"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "var isolationTest = 'main context variable'; \
                         \if (typeof window.sandboxedVariable !== 'undefined') { \
                         \  document.body.innerHTML += '<div>WARNING: Sandbox leak detected - sandboxedVariable accessible</div>'; \
                         \} else { \
                         \  document.body.innerHTML += '<div>✓ Sandbox isolation confirmed - sandboxedVariable not accessible</div>'; \
                         \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseMinMs 500
      chkDOM "✓ Sandbox isolation confirmed"
      pause

      logTxt "Test 11: Create a new browsing context to test multiple contexts behavior"
      newContext <- newWindowContext utils cmds
      
      logTxt "Test 12: Add a preload script specific to the new context only"
      preloadScriptNewContext <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.addEventListener('load', function() { \
                \    var body = document.body; \
                \    if (body) { \
                \      var notice = document.createElement('div'); \
                \      notice.style.cssText = 'position: fixed; top: 10px; right: 10px; background: pink; padding: 10px; border: 2px solid red; z-index: 9999;'; \
                \      notice.innerHTML = '<strong>New Context Script!</strong><br/>Only in new window'; \
                \      body.appendChild(notice); \
                \    } \
                \  }); \
                \}",
              arguments = Nothing,
              contexts = Just [newContext],  -- Only apply to new context
              sandbox = Nothing
            }
      logShow "Context-specific preload script added" preloadScriptNewContext
      pause

      logTxt "Test 13: Navigate to new context to see context-specific script behavior"
      -- Navigate both contexts to observe differences
      navResult3 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Original Context</title></head><body><h1>Original Context Page</h1><p id='content'>Original context content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation result - original context" navResult3
      pause

      navResultNew <- browsingContextNavigate $ MkNavigate {context = newContext, url = "data:text/html,<html><head><title>New Context</title></head><body><h1>New Context Page</h1><p id='content'>New context content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation result - new context" navResultNew
      pause

      -- Wait for all preload scripts to execute
      pauseMinMs 1500

      logTxt "Test 14: Verify original context has basic, sandboxed, channel, and global scripts"
      chkDOM "✓ Basic Preload Script executed!"
      chkDOM "Sandboxed Script:"
      chkDOM "Channel Argument:"
      
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "if (window.PRELOADED_DATA) { \
                         \  document.body.innerHTML += '<div>Original context global data confirmed</div>'; \
                         \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseMinMs 500
      chkDOM "Original context global data confirmed"
      pause

      logTxt "Test 15: Check new context has only global and context-specific scripts"
      -- Switch to new context for validation
      browsingContextActivate $ MkActivate newContext
      pauseMinMs 500
      
      -- Check new context content using scripts that add to DOM
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "if (window.PRELOADED_DATA) { \
                         \  document.body.innerHTML += '<div>New context has global data</div>'; \
                         \} else { \
                         \  document.body.innerHTML += '<div>New context missing global data</div>'; \
                         \} \
                         \if (document.querySelector('div[style*=\"position: fixed\"]')) { \
                         \  document.body.innerHTML += '<div>New context has fixed position element</div>'; \
                         \}",
            target = ContextTarget $ MkContextTarget {context = newContext, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseMinMs 500
      
      -- For new context validation, we need to get DOM content from the new context
      domContentNew <- scriptEvaluate $ 
        MkEvaluate
          { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
            target = ContextTarget $ MkContextTarget {context = newContext, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      
      case domContentNew of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue newContextText)} -> do
          logTxt $ "New context DOM content: " <> newContextText
          if "New context has global data" `isInfixOf` newContextText
            then logTxt "✓ New context has global script effects"
            else logTxt "✗ New context missing global script effects"
          if "New Context Script!" `isInfixOf` newContextText
            then logTxt "✓ New context has context-specific script effects"
            else logTxt "✗ New context missing context-specific script effects"
        _ -> logTxt "Could not read new context DOM content"
      pause

      logTxt "Test 16: Remove basic preload script and test selective removal"
      let MkAddPreloadScriptResult script1Id = preloadScript1
      removeResult1 <- scriptRemovePreloadScript $ MkRemovePreloadScript script1Id
      logShow "Removed basic preload script" removeResult1
      pause

      logTxt "Test 17: Navigate original context to verify basic script removed but others remain"
      browsingContextActivate $ MkActivate bc
      pauseMinMs 500
      
      navResult4 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>After Basic Removal</title></head><body><h1>After Basic Script Removal</h1><p id='content'>Content after basic removal</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation after basic script removal" navResult4
      pause

      -- Wait for remaining preload scripts
      pauseMinMs 1500

      logTxt "Test 18: Verify sandboxed, channel, and global scripts still work"      
      chkDOM "Sandboxed Script:"
      chkDOM "Channel Argument:"
      
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "if (window.PRELOADED_DATA) { \
                         \  document.body.innerHTML += '<div>Global script still active after basic removal</div>'; \
                         \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseMinMs 500
      chkDOM "Global script still active after basic removal"
      pause

      logTxt "Test 19: Remove all remaining preload scripts for complete cleanup"
      let MkAddPreloadScriptResult script2Id = preloadScript2
      removeResult2 <- scriptRemovePreloadScript $ MkRemovePreloadScript script2Id
      logShow "Removed global preload script" removeResult2
      pause

      let MkAddPreloadScriptResult scriptSandboxId = preloadScriptSandbox
      removeResultSandbox <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptSandboxId
      logShow "Removed sandbox preload script" removeResultSandbox
      pause

      let MkAddPreloadScriptResult scriptArgsId = preloadScriptWithArgs
      removeResultArgs <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptArgsId
      logShow "Removed channel arguments preload script" removeResultArgs
      pause

      let MkAddPreloadScriptResult scriptNewContextId = preloadScriptNewContext
      removeResultNewContext <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptNewContextId
      logShow "Removed new context preload script" removeResultNewContext
      pause

      logTxt "Test 20: Final navigation to verify complete cleanup"
      navResult5 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Clean Page</title></head><body><h1>Clean Page - No Preload Scripts</h1><p id='content'>Clean content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Final navigation - clean page" navResult5
      pause

      -- Wait to ensure no preload scripts run
      pauseMinMs 1000

      logTxt "Test 21: Final verification - no preload script effects should be present"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression = "if (!window.PRELOADED_DATA && !document.querySelector('div[style*=\"position: fixed\"]') && !document.querySelector('#sandbox-notice') && !document.querySelector('#channel-info')) { \
                         \  document.body.innerHTML += '<div>✓ Complete cleanup confirmed: No preload effects</div>'; \
                         \} else { \
                         \  document.body.innerHTML += '<div>⚠ Warning: Some preload effects still detected</div>'; \
                         \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseMinMs 500
      chkDOM "✓ Complete cleanup confirmed: No preload effects"
      pause

      logTxt "Test 22: Cleanup - close the new context"
      closeContext utils cmds newContext
