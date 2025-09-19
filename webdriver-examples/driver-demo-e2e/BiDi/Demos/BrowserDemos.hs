module BiDi.Demos.BrowserDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Browser
import WebDriverPreCore.BiDi.Capabilities (ProxyConfiguration, UserPromptHandler)
import WebDriverPreCore.BiDi.CoreTypes (UserContext (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log, putStrLn)

{-
Browser Module Commands (6 total):

1. browser.close - Terminates all WebDriver sessions and cleans up automation state
2. browser.createUserContext - Creates a user context with optional settings
3. browser.getClientWindows - Returns a list of client windows with their properties
4. browser.getUserContexts - Returns a list of user contexts
5. browser.removeUserContext - Closes a user context and all navigables in it
6. browser.setClientWindowState - Sets client window state (fullscreen, maximized, minimized, normal)

TODO: Implement demos for each command:
- browserCloseDemo - Demonstrate browser.close functionality
- browserCreateUserContextDemo - Create user contexts with different configurations
- browserGetClientWindowsDemo - Get and display client window information
- browserGetUserContextsDemo - List all user contexts
- browserRemoveUserContextDemo - Remove specific user contexts
- browserSetClientWindowStateDemo - Change window states (maximize, minimize, etc.)
- browserCompleteWorkflowDemo - Comprehensive demo showing user context and window management workflow
-}

-- TODO: browserCloseDemo
-- Demonstrates browser.close command
-- This command terminates all WebDriver sessions and cleans up automation state
-- Note: This should be used carefully as it closes the entire browser

-- TODO: browserCreateUserContextDemo  
-- Demonstrates browser.createUserContext command
-- Shows creating user contexts with different configurations:
-- - Basic user context creation
-- - User context with custom proxy settings
-- - User context with unhandled prompt behavior
-- - User context with accept insecure certificates setting

-- TODO: browserGetClientWindowsDemo
-- Demonstrates browser.getClientWindows command
-- Shows retrieving information about all client windows:
-- - Window dimensions (width, height)
-- - Window position (x, y coordinates) 
-- - Window state (fullscreen, maximized, minimized, normal)
-- - Active status

-- TODO: browserGetUserContextsDemo
-- Demonstrates browser.getUserContexts command
-- Shows listing all available user contexts in the browser
-- Useful for understanding the current browser state

-- TODO: browserRemoveUserContextDemo
-- Demonstrates browser.removeUserContext command
-- Shows removing specific user contexts:
-- - Remove individual user contexts
-- - Handle cases where context doesn't exist
-- - Cleanup after user context operations

-- TODO: browserSetClientWindowStateDemo
-- Demonstrates browser.setClientWindowState command
-- Shows changing window states:
-- - Maximize window
-- - Minimize window
-- - Set to fullscreen
-- - Restore to normal
-- - Set custom window dimensions and position

-- TODO: browserCompleteWorkflowDemo
-- Comprehensive demo combining multiple browser commands:
-- - Create multiple user contexts
-- - Manage client windows across contexts
-- - Change window states
-- - Clean up user contexts
-- - Demonstrate practical browser automation scenarios