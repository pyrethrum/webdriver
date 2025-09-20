module BiDi.Demos.WebExtensionDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.WebExtensions
import WebDriverPreCore.Internal.Utils (txt)
import Control.Exception (SomeException, catch)
import Prelude hiding (log, putStrLn)
import qualified Data.Aeson.KeyMap as AKM
import TestPages (demoExtensionDirPath, demoExtensionZipPath)

{-
WebExtension Module Commands (2 total):

1. webExtension.install - Installs a web extension in the remote end
2. webExtension.uninstall - Uninstalls a web extension from the remote end

WebExtension Module Types:
- webExtension.Extension - Web extension id within a remote end
- webExtension.ExtensionData - Union type for extension data sources
- webExtension.ExtensionPath - Extension specified by filesystem path
- webExtension.ExtensionArchivePath - Extension specified by archive path  
- webExtension.ExtensionBase64Encoded - Extension specified by base64 data

Key Concepts:
- Extension installation from various sources (path, archive, base64)
- Extension lifecycle management
- Temporary vs persistent installation
- Extension ID management and tracking
- Zip archive extraction and validation
- Cross-platform file system considerations

Complexity factors:
- Multiple installation methods (path, archivePath, base64)
- Zip archive processing and validation
- File system path resolution and security
- Extension manifest validation
- Installation error handling
- Platform-specific extension behavior
- Temporary installation cleanup
-}

-- >>> runDemo webExtensionInstallPathDemo
webExtensionInstallPathDemo :: BiDiDemo
webExtensionInstallPathDemo =
  demo "WebExtension - Install from Path" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Test 1: Install extension from filesystem path"
      exPath <- demoExtensionDirPath
      result <- webExtensionInstall $ ExtensionPath exPath
      logShow "Extension installed from path" result
      pause

      logTxt "Test 2: Uninstall the extension"
      uninstallResult <- webExtensionUninstall $ MkWebExtensionUninstall result.extension
      logShow "Extension uninstalled" uninstallResult
      pause

-- >>> runDemo webExtensionInstallArchiveDemo
webExtensionInstallArchiveDemo :: BiDiDemo
webExtensionInstallArchiveDemo =
  demo "WebExtension - Install from Archive" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Test 1: Install extension from zip archive"
      zipPath <- demoExtensionZipPath
      result1 <- webExtensionInstall $ ExtensionArchivePath zipPath
      logShow "Extension installed from archive" result1
      pause



-- >>> runDemo webExtensionInstallBase64Demo
webExtensionInstallBase64Demo :: BiDiDemo
webExtensionInstallBase64Demo =
  demo "WebExtension - Install from Base64" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "Test 1: Install extension from base64 encoded data"
      -- This would be a real base64 encoded extension in practice
      let base64Data = "UEsDBAoAAAAAAIdWJlMAAAAAAAAAAAAAAAAJAAAAbWFuaWZlc3QupGV1c3Rpb24udXN0aW1lemluZSAiMS4wIg=="
      let base64Extension = ExtensionBase64Encoded base64Data
      result1 <- webExtensionInstall base64Extension
      logShow "Extension installed from base64" result1
      pause

      logTxt "Test 2: Install extension from base64 (alternative data)"
      let altBase64Data = "UEsDBAoAAAAAAIdWJlMAAAAAAAAAAAAAAAAJAAAAbWFuaWZlc3QuanNvbntcIm5hbWVcIjpcIkRlbW8gRXh0ZW5zaW9uXCIsXCJ2ZXJzaW9uXCI6XCIxLjBcIn0="
      let altBase64Extension = ExtensionBase64Encoded altBase64Data
      result2 <- webExtensionInstall altBase64Extension
      logShow "Alternative base64 extension installed" result2
      pause

-- -- >>> runDemo webExtensionUninstallDemo
-- webExtensionUninstallDemo :: BiDiDemo
-- webExtensionUninstallDemo =
--   demo "WebExtension - Uninstall Extension" action
--   where
--     action :: DemoUtils -> Commands -> IO ()
--     action MkDemoUtils {..} MkCommands {..} = do
--       logTxt "First, install an extension to demonstrate uninstallation"
--       let testExtension = ExtensionPath "/tmp/test-extension"
--       installResult <- webExtensionInstall testExtension
--       logShow "Test extension installed" installResult
      
--       let extensionId = (.extension) installResult
--       pause

--       logTxt "Test 1: Uninstall the extension"
--       uninstallResult <- webExtensionUninstall extensionId
--       logShow "Extension uninstalled" uninstallResult
--       pause

--       logTxt "Test 2: Attempt to uninstall non-existent extension"
--       let fakeExtension = MkWebExtension "non-existent-extension-id"
--       webExtensionUninstall fakeExtension 
--        `catch` \(e :: SomeException) -> do
--          logShow "Expected error for non-existent extension: " e
--          pure AKM.empty

--       pause

-- >>> runDemo webExtensionValidationDemo
-- webExtensionValidationDemo :: BiDiDemo
-- webExtensionValidationDemo =
--   demo "WebExtension - Extension Validation" action
--   where
--     action :: DemoUtils -> Commands -> IO ()
--     action MkDemoUtils {..} MkCommands {..} = do
--       logTxt "Test 1: Valid extension installation"
--       let validExtension = ExtensionPath "/valid/extension/path"
--       validResult <- webExtensionInstall validExtension `catch` \(e :: SomeException) -> do
--         logTxt $ "Installation error (expected for demo): " <> txt e
--         -- Return demo result
--         pure $ WebExtensionInstallResult { extension = MkWebExtension "demo-extension-id" }
--       logShow "Valid extension result" validResult
--       pause

--       logTxt "Test 2: Invalid extension path"
--       let invalidExtension = ExtensionPath "/non/existent/path"
--       invalidResult <- webExtensionInstall invalidExtension `catch` \(e :: SomeException) -> do
--         logTxt $ "Installation failed as expected: " <> txt e
--         pure $ WebExtensionInstallResult { extension = MkWebExtension "failed-extension" }
--       logShow "Invalid extension handling" invalidResult
--       pause

--       logTxt "Test 3: Malformed base64 data"
--       let malformedBase64 = ExtensionBase64Encoded "invalid-base64-data!!!"
--       malformedResult <- webExtensionInstall malformedBase64 `catch` \(e :: SomeException) -> do
--         logTxt $ "Base64 error (expected): " <> txt e
--         pure $ WebExtensionInstallResult { extension = MkWebExtension "malformed-extension" }
--       logShow "Malformed base64 handling" malformedResult
--       pause

-- -- >>> runDemo webExtensionCompleteLifecycleDemo
-- webExtensionCompleteLifecycleDemo :: BiDiDemo
-- webExtensionCompleteLifecycleDemo =
--   demo "WebExtension - Complete Lifecycle" action
--   where
--     action :: DemoUtils -> Commands -> IO ()
--     action MkDemoUtils {..} MkCommands {..} = do
--       logTxt "Step 1: Install extension from path"
--       let pathExtension = ExtensionPath "/demo/extension"
--       installResult1 <- webExtensionInstall pathExtension `catch` \(e :: SomeException) -> do
--         logTxt $ "Install demo path: " <> txt e
--         pure $ WebExtensionInstallResult { extension = MkWebExtension "demo-path-ext" }
--       logShow "Path extension installed" installResult1
      
--       let pathExtId = (.extension) installResult1
--       pause

--       logTxt "Step 2: Install extension from archive"
--       let archiveExtension = ExtensionArchivePath "/demo/extension.zip"
--       installResult2 <- webExtensionInstall archiveExtension `catch` \(e :: SomeException) -> do
--         logTxt $ "Install demo archive: " <> txt e
--         pure $ WebExtensionInstallResult { extension = MkWebExtension "demo-archive-ext" }
--       logShow "Archive extension installed" installResult2
      
--       let archiveExtId = (.extension) installResult2
--       pause

--       logTxt "Step 3: Install extension from base64"
--       let base64Extension = ExtensionBase64Encoded "UEsDBAoAAAAAAIdWJlMAAAA="
--       installResult3 <- webExtensionInstall base64Extension `catch` \(e :: SomeException) -> do
--         logTxt $ "Install demo base64: " <> txt e
--         pure $ WebExtensionInstallResult { extension = MkWebExtension "demo-base64-ext" }
--       logShow "Base64 extension installed" installResult3
      
--       let base64ExtId = (.extension) installResult3
--       pause

--       logTxt "Step 4: Uninstall all extensions"
--       uninstall1 <- webExtensionUninstall pathExtId 
--       logShow "First extension uninstalled" uninstall1

--       uninstall2 <- webExtensionUninstall archiveExtId 
--       logShow "Second extension uninstalled" uninstall2

--       uninstall3 <- webExtensionUninstall base64ExtId 
--       logShow "Third extension uninstalled" uninstall3
--       pause

--       logTxt "Extension lifecycle demo complete"
