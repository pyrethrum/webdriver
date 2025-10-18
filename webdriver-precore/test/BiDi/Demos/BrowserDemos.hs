module BiDi.Demos.BrowserDemos where

import BiDi.BiDiRunner (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Browser
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)

{-
Browser Module Commands (6 total):

1. browser.close - Terminates all WebDriver sessions and cleans up automation state ✓
2. browser.createUserContext - Creates a user context with optional settings ✓
3. browser.getClientWindows - Returns a list of client windows with their properties ✓
4. browser.getUserContexts - Returns a list of user contexts ✓
5. browser.removeUserContext - Closes a user context and all navigables in it ✓
6. browser.setClientWindowState - Sets client window state (fullscreen, maximized, minimized, normal) ✓

Implemented demos:
- browserCloseDemo - Demonstrate browser.close functionality (⚠️ TERMINATES SESSION)
- browserCreateUserContextDemo - Create user contexts with different configurations
- browserGetClientWindowsDemo - Get and display client window information
- browserGetUserContextsDemo - List all user contexts
- browserRemoveUserContextDemo - Remove specific user contexts
- browserSetClientWindowStateDemo - Change window states (maximize, minimize, etc.)
- browserCompleteWorkflowDemo - Comprehensive demo showing user context and window management workflow
-}

-- >>> runDemo browserGetClientWindowsDemo
browserGetClientWindowsDemo :: BiDiDemo
browserGetClientWindowsDemo =
  demo "Browser - Get Client Windows" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Getting current client windows"
      clientWindows <- browserGetClientWindows
      logShow "Client windows" clientWindows
      pause

-- >>> runDemo browserCreateUserContextDemo  
browserCreateUserContextDemo :: BiDiDemo
browserCreateUserContextDemo =
  demo "Browser - Create User Context" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Creating basic user context"
      let basicUserContext = MkCreateUserContext 
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      uc1 <- browserCreateUserContext basicUserContext
      logShow "Basic user context created" uc1
      pause

      logTxt "Creating user context with insecure certs"
      let secureUserContext = basicUserContext { insecureCerts = Just True }
      uc2 <- browserCreateUserContext secureUserContext
      logShow "User context with insecure certs" uc2
      pause

-- >>> runDemo browserGetUserContextsDemo
browserGetUserContextsDemo :: BiDiDemo
browserGetUserContextsDemo =
  demo "Browser - Get User Contexts" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Getting all user contexts"
      userContexts <- browserGetUserContexts
      logShow "User contexts" userContexts
      pause

-- >>> runDemo browserSetClientWindowStateDemo
browserSetClientWindowStateDemo :: BiDiDemo
browserSetClientWindowStateDemo =
  demo "Browser - Set Client Window State" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Getting current client windows to find one to modify"
      clientWindows <- browserGetClientWindows
      logShow "Available client windows" clientWindows
      
      -- TODO: not supported expectation
      {- Not supported in geckodriver yet 
        [] -> logTxt "No client windows found to demonstrate state changes"
        (window:_) -> do
          let clientWindow = window.clientWindow
          pause

          logTxt "Maximizing window"
          let maximizeState = MkSetClientWindowState 
                { clientWindow = clientWindow,
                  windowState = ClientWindowNamedState NamedMaximized
                }
          maxResult <- browserSetClientWindowState maximizeState
          logShow "Maximized window result" maxResult
          pause

          logTxt "Restoring window to normal"
          let normalState = MkSetClientWindowState 
                { clientWindow = clientWindow,
                  windowState = ClientWindowRectState $ MkRectState 
                    { state = NormalState,
                      width = Just 800,
                      height = Just 600,
                      x = Just 100,
                      y = Just 100
                    }
                }
          normalResult <- browserSetClientWindowState normalState
          logShow "Normal window result" normalResult
          pause
      -}
-- >>> runDemo browserRemoveUserContextDemo
browserRemoveUserContextDemo :: BiDiDemo
browserRemoveUserContextDemo =
  demo "Browser - Remove User Context" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Creating a user context to remove"
      let userContextParams = MkCreateUserContext 
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      uc <- browserCreateUserContext userContextParams
      logShow "Created user context" uc
      pause

      logTxt "Removing the user context"
      let removeParams = MkRemoveUserContext { userContext = uc }
      removeResult <- browserRemoveUserContext removeParams
      logShow "Remove user context result" removeResult
      pause

-- >>> runDemo browserCompleteWorkflowDemo
browserCompleteWorkflowDemo :: BiDiDemo
browserCompleteWorkflowDemo =
  demo "Browser - Complete Workflow" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      
      logTxt "Step 1: Get initial state"
      initialWindows <- browserGetClientWindows
      initialContexts <- browserGetUserContexts
      logShow "Initial client windows" initialWindows
      logShow "Initial user contexts" initialContexts
      pause

      logTxt "Step 2: Create multiple user contexts"
      let userContext1 = MkCreateUserContext 
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      uc1 <- browserCreateUserContext userContext1
      logShow "User context 1 created" uc1

      let userContext2 = userContext1 { insecureCerts = Just True }
      uc2 <- browserCreateUserContext userContext2
      logShow "User context 2 created (with insecure certs)" uc2
      pause

      logTxt "Step 3: Create browsing contexts in different user contexts"
      let bcParams = MkCreate
            { createType = Window,
              background = False,
              referenceContext = Nothing,
              userContext = Just uc1
            }
      bc1 <- browsingContextCreate bcParams
      logShow "Browsing context in user context 1" bc1

      let bcParams2 = MkCreate
            { createType = Window,
              background = False,
              referenceContext = Nothing,
              userContext = Just uc2
            }
      bc2 <- browsingContextCreate bcParams2
      logShow "Browsing context in user context 2" bc2
      pause

      logTxt "Step 4: Check updated state"
      updatedWindows <- browserGetClientWindows
      updatedContexts <- browserGetUserContexts
      logShow "Updated client windows" updatedWindows
      logShow "Updated user contexts" updatedContexts
      pause

      -- todo After errors unknown command expectation
      {- Not supported in geckodriver yet 
      logTxt "Step 5: Demonstrate window state management"
       
      case updatedWindows.clientWindows of
        [] -> logTxt "No windows to manage"
        (window:_) -> do
          let clientWindow = window.clientWindow
          
          logTxt "Maximizing first window"
          let maximizeState = MkSetClientWindowState 
                { clientWindow = clientWindow,
                  windowState = ClientWindowNamedState NamedMaximized
                }
          wm <- browserSetClientWindowState maximizeState
          logShow "Window maximized" wm
          pause
      -}
      logTxt "Step 6: Cleanup - Close browsing contexts"
      bcc <- browsingContextClose $ MkClose 
        { context = bc1,
          promptUnload = Nothing
        }
      logShow "Browsing context 1 closed" bcc

      bcc2 <- browsingContextClose $ MkClose 
        { context = bc2,
          promptUnload = Nothing
        }
      logShow "Browsing context 2 closed" bcc2
      pause

      logTxt "Step 7: Cleanup - Remove user contexts"
      ruc <- browserRemoveUserContext $ MkRemoveUserContext { userContext = uc1 }
      logShow "User context 1 removed" ruc

      ruc2 <- browserRemoveUserContext $ MkRemoveUserContext { userContext = uc2 }
      logShow "User context 2 removed" ruc2
      pause

      logTxt "Step 8: Final state check"
      finalWindows <- browserGetClientWindows
      finalContexts <- browserGetUserContexts
      logShow "Final client windows" finalWindows
      logShow "Final user contexts" finalContexts

-- >>> runDemo browserCloseDemo
browserCloseDemo :: BiDiDemo
browserCloseDemo =
  demo "Browser - Close (CAUTION: Terminates Session)" action
  where
    -- will fail with: "Closing the browser in a session started with WebDriver classic is not supported."
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do

      logTxt "Getting final browser state before closing..."
      finalWindows <- browserGetClientWindows
      finalContexts <- browserGetUserContexts
      logShow "Final client windows before close" finalWindows
      logShow "Final user contexts before close" finalContexts
      pause
      
      logTxt "Executing browser.close - this will end the session"
      logTxt "This will fail because the session was created with webdriver http not BiDi"
      closeResult <- browserClose
      logShow "Browser close result" closeResult
      
