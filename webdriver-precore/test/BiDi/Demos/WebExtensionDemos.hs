module BiDi.Demos.WebExtensionDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import Control.Exception (SomeException, catch)
import Data.Aeson.KeyMap qualified as AKM
import IOUtils (DemoUtils (..))
import TestData (demoExtensionAsBase64, demoExtensionDirPath, demoExtensionZipPath)
import WebDriverPreCore.BiDi.WebExtensions
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

{-
WebExtension Module Commands (2 total):
1. webExtension.install - Installs a web extension in the remote end :: DONE
2. webExtension.uninstall - Uninstalls a web extension from the remote end :: DONE
-}

-- >>> runDemo webExtensionInstallPathDemo
webExtensionInstallPathDemo :: BiDiDemo
webExtensionInstallPathDemo =
  demo "WebExtension - Install from Path" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkBiDiActions {..} = do
      logTxt "Test 1: Install extension from filesystem path"
      exPath <- demoExtensionDirPath
      result <- webExtensionInstall $ ExtensionPath exPath
      logShow "Extension installed from path" result
      pause

      logTxt "Test 2: Uninstall the extension"
      uninstallResult <- webExtensionUninstall $ MkWebExtensionUninstall result.extension
      logShow "Extension uninstalled" uninstallResult
      pause

      logTxt "Test 3: Attempt to uninstall non-existent extension"
      webExtensionUninstall (MkWebExtensionUninstall $ MkWebExtensionID "non-existent-extension-id")
        `catch` \(e :: SomeException) -> do
          logShow "Expected error for non-existent extension: " e
          pure AKM.empty
      pause

-- >>> runDemo webExtensionInstallArchiveDemo
webExtensionInstallArchiveDemo :: BiDiDemo
webExtensionInstallArchiveDemo =
  demo "WebExtension - Install from Archive" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkBiDiActions {..} = do
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
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkBiDiActions {..} = do
      logTxt "Test 1: Install extension from base64 encoded data"

      base64Data <- demoExtensionAsBase64
      result1 <- webExtensionInstall $ ExtensionBase64Encoded base64Data
      logShow "Extension installed from base64" result1
      pause

-- >>> runDemo webExtensionValidationDemo
webExtensionValidationDemo :: BiDiDemo
webExtensionValidationDemo =
  demo "WebExtension - Extension Validation" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} MkBiDiActions {..} = do
      logTxt "Test 1: Invalid extension path"
      let invalidExtension = ExtensionPath "/non/existent/path"
      invalidResult <-
        webExtensionInstall invalidExtension `catch` \(e :: SomeException) -> do
          logTxt $ "Installation failed as expected: " <> txt e
          pure $ WebExtensionInstallResult {extension = MkWebExtensionID "failed-extension"}
      logShow "Invalid extension handling" invalidResult
      pause

      logTxt "Test 2: Malformed base64 data"
      let malformedBase64 = ExtensionBase64Encoded "invalid-base64-data!!!"
      malformedResult <-
        webExtensionInstall malformedBase64 `catch` \(e :: SomeException) -> do
          logTxt $ "Base64 error (expected): " <> txt e
          pure $ WebExtensionInstallResult {extension = MkWebExtensionID "malformed-extension"}
      logShow "Malformed base64 handling" malformedResult
      pause
