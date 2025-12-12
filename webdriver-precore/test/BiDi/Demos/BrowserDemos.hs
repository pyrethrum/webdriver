module BiDi.Demos.BrowserDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoActions (..))
import WebDriverPreCore.BiDi.Protocol
  ( Close (..),
    Create (..),
    CreateType (..),
    CreateUserContext (..),
    DownloadBehaviour (..),
    NamedState (..),
    RemoveUserContext (..),
    SetClientWindowState (..),
    SetDownloadBehavior (..),
    GetClientWindowsResult (..),
    ClientWindowInfo (..),
    WindowState (..),
    RectState (..)
  )
import Prelude hiding (log, putStrLn)

-- >>> runDemo browserGetClientWindowsDemo
browserGetClientWindowsDemo :: BiDiDemo
browserGetClientWindowsDemo =
  demo "Browser - Get Client Windows" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Getting current client windows"
      clientWindows <- browserGetClientWindows
      logShow "Client windows" clientWindows
      pause

-- >>> runDemo browserCreateUserContextDemo
browserCreateUserContextDemo :: BiDiDemo
browserCreateUserContextDemo =
  demo "Browser - Create User Context" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Creating basic user context"
      let basicUserContext =
            MkCreateUserContext
              { insecureCerts = Nothing,
                proxy = Nothing,
                unhandledPromptBehavior = Nothing
              }
      uc1 <- browserCreateUserContext basicUserContext
      logShow "Basic user context created" uc1
      pause

      logTxt "Creating user context with insecure certs"
      let secureUserContext = basicUserContext {insecureCerts = Just True}
      uc2 <- browserCreateUserContext secureUserContext
      logShow "User context with insecure certs" uc2
      pause

-- >>> runDemo browserGetUserContextsDemo
browserGetUserContextsDemo :: BiDiDemo
browserGetUserContextsDemo =
  demo "Browser - Get User Contexts" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Getting all user contexts"
      userContexts <- browserGetUserContexts
      logShow "User contexts" userContexts
      pause

-- >>> runDemo browserSetClientWindowStateDemo
-- *** Exception: BiDIError (ProtocolException {error = UnknownCommand, description = "A command could not be executed because the remote end is not aware of it", message = "browser.setClientWindowState", stacktrace = Just "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nUnknownCommandError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:944:5\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:420:13\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n", errorData = Nothing, response = Object (fromList [("error",String "unknown command"),("id",Number 2.0),("message",String "browser.setClientWindowState"),("stacktrace",String "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nUnknownCommandError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:944:5\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:420:13\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n"),("type",String "error")])})
browserSetClientWindowStateDemo :: BiDiDemo
browserSetClientWindowStateDemo =
  demo "Browser - Set Client Window State" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Getting current client windows to find one to modify"
      clientWindows <- browserGetClientWindows
      logShow "Available client windows" clientWindows

      -- TODO: not supported expectation
      let window = case clientWindows.clientWindows of
            [] -> error "No client windows found"
            x : _ -> x

          clientWindow = window.clientWindow
      pause

      logTxt "Maximizing window"
      let maximizeState =
            MkSetClientWindowState
              { clientWindow = clientWindow,
                windowState = ClientWindowNamedState MaximizedState
              }
      maxResult <- browserSetClientWindowState maximizeState
      logShow "Maximized window result" maxResult
      pause

      logTxt "Restoring window to normal"
      let normalState =
            MkSetClientWindowState
              { clientWindow = clientWindow,
                windowState =
                  ClientWindowRectState $
                    MkRectState
                      { 
                        width = Just 800,
                        height = Just 600,
                        x = Just 100,
                        y = Just 100
                      }
              }
      normalResult <- browserSetClientWindowState normalState
      logShow "Normal window result" normalResult
      pause

-- >>> runDemo browserRemoveUserContextDemo
browserRemoveUserContextDemo :: BiDiDemo
browserRemoveUserContextDemo =
  demo "Browser - Remove User Context" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Creating a user context to remove"
      let userContextParams =
            MkCreateUserContext
              { insecureCerts = Nothing,
                proxy = Nothing,
                unhandledPromptBehavior = Nothing
              }
      uc <- browserCreateUserContext userContextParams
      logShow "Created user context" uc
      pause

      logTxt "Removing the user context"
      let removeParams = MkRemoveUserContext {userContext = uc}
      removeResult <- browserRemoveUserContext removeParams
      logShow "Remove user context result" removeResult
      pause

-- >>> runDemo browserCompleteWorkflowDemo
browserCompleteWorkflowDemo :: BiDiDemo
browserCompleteWorkflowDemo =
  demo "Browser - Complete Workflow" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Step 1: Get initial state"
      initialWindows <- browserGetClientWindows
      initialContexts <- browserGetUserContexts
      logShow "Initial client windows" initialWindows
      logShow "Initial user contexts" initialContexts
      pause

      logTxt "Step 2: Create multiple user contexts"
      let userContext1 =
            MkCreateUserContext
              { insecureCerts = Nothing,
                proxy = Nothing,
                unhandledPromptBehavior = Nothing
              }
      uc1 <- browserCreateUserContext userContext1
      logShow "User context 1 created" uc1

      let userContext2 = userContext1 {insecureCerts = Just True}
      uc2 <- browserCreateUserContext userContext2
      logShow "User context 2 created (with insecure certs)" uc2
      pause

      logTxt "Step 3: Create browsing contexts in different user contexts"
      let bcParams =
            MkCreate
              { createType = Window,
                background = False,
                referenceContext = Nothing,
                userContext = Just uc1
              }
      bc1 <- browsingContextCreate bcParams
      logShow "Browsing context in user context 1" bc1

      let bcParams2 =
            MkCreate
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
          let clientWindow = window.clientWindowJ

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
      bcc <-
        browsingContextClose $
          MkClose
            { context = bc1,
              promptUnload = Nothing
            }
      logShow "Browsing context 1 closed" bcc

      bcc2 <-
        browsingContextClose $
          MkClose
            { context = bc2,
              promptUnload = Nothing
            }
      logShow "Browsing context 2 closed" bcc2
      pause

      logTxt "Step 7: Cleanup - Remove user contexts"
      ruc <- browserRemoveUserContext $ MkRemoveUserContext {userContext = uc1}
      logShow "User context 1 removed" ruc

      ruc2 <- browserRemoveUserContext $ MkRemoveUserContext {userContext = uc2}
      logShow "User context 2 removed" ruc2
      pause

      logTxt "Step 8: Final state check"
      finalWindows <- browserGetClientWindows
      finalContexts <- browserGetUserContexts
      logShow "Final client windows" finalWindows
      logShow "Final user contexts" finalContexts

-- >>> runDemo browserSetDownloadBehaviorDemo
-- *** Exception: Error executing BiDi command: With JSON: 
-- {
--     "id": 1,
--     "method": "browser.setDownloadBehavior",
--     "params": {
--         "downloadBehavior": {
--             "destinationFolder": "/tmp/downloads",
--             "type": "allowed"
--         }
--     }
-- }
-- BiDi driver error: 
-- MkDriverError
--   { id = Just 1
--   , error = UnknownCommand
--   , description = "The command sent is not known"
--   , message = "browser.setDownloadBehavior"
--   , stacktrace =
--       Just
--         "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nUnknownCommandError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:944:5\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:407:13\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n"
--   , extensions = MkEmptyResult { extensible = fromList [] }
--   }
browserSetDownloadBehaviorDemo :: BiDiDemo
browserSetDownloadBehaviorDemo =
  demo "Browser - Set Download Behavior (since https://www.w3.org/TR/2025/WD-webdriver-bidi-20250918)" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      logTxt "Setting download behavior to allow downloads to /tmp/downloads"
      let allowedDownload =
            MkSetDownloadBehavior
              { downloadBehavior = Just $ AllowedDownload {destinationFolder = "/tmp/downloads"},
                userContexts = Nothing
              }
      result <- browserSetDownloadBehavior allowedDownload
      logShow "Allow download result" result
      pause

      logTxt "Setting download behavior to deny downloads"
      let deniedDownload =
            MkSetDownloadBehavior
              { downloadBehavior = Just DeniedDownload,
                userContexts = Nothing
              }
      result2 <- browserSetDownloadBehavior deniedDownload
      logShow "Deny download result" result2
      pause

      logTxt "Clearing download behavior (reset to default)"
      let clearDownload =
            MkSetDownloadBehavior
              { downloadBehavior = Nothing,
                userContexts = Nothing
              }
      result3 <- browserSetDownloadBehavior clearDownload
      logShow "Clear download behavior result" result3
      pause

-- >>> runDemo browserCloseDemo
-- *** Exception: Error executing BiDi command: With JSON: 
-- {
--     "id": 3,
--     "method": "browser.close",
--     "params": {}
-- }
-- BiDi driver error: 
-- MkDriverError
--   { id = Just 3
--   , error = UnsupportedOperation
--   , description = "The operation requested is not supported"
--   , message =
--       "Closing the browser in a session started with WebDriver classic is not supported. Use the WebDriver classic \"Delete Session\" command instead which will also close the browser."
--   , stacktrace =
--       Just
--         "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nUnsupportedOperationError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:978:5\nclose@chrome://remote/content/webdriver-bidi/modules/root/browser.sys.mjs:120:13\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:282:33\nexecute@chrome://remote/content/shared/webdriver/Session.sys.mjs:410:32\nonPacket@chrome://remote/content/webdriver-bidi/WebDriverBiDiConnection.sys.mjs:236:37\nonMessage@chrome://remote/content/server/WebSocketTransport.sys.mjs:127:18\nhandleEvent@chrome://remote/content/server/WebSocketTransport.sys.mjs:109:14\n"
--   , extensions = MkEmptyResult { extensible = fromList [] }
--   }
browserCloseDemo :: BiDiDemo
browserCloseDemo =
  demo "Browser - Close (CAUTION: Terminates Session)" action
  where
    -- will fail with: "Closing the browser in a session started with WebDriver classic is not supported."
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
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



