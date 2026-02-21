module WebDriverPreCore.Test.TestData
  ( testFilesDir,
    testPath,
    fileUrl,
    uploadFilePath,
    demoExtensionDirPath,
    demoExtensionZipPath,
    demoExtensionAsBase64,
    textAreaUrl,
    checkboxesUrl,
    infiniteScrollUrl,
    promptUrl,
    fragmentUrl,
    downloadUrl,
    slowLoadUrl,
    downloadLinkUrl,
    consoleLogUrl,
    scriptRealmUrl,
    badJavaScriptUrl,
    uploadUrl,
    navigation1Url,
    navigation2Url,
    navigation3Url,
    navigation4Url,
    navigation5Url,
    navigation6Url,
    loginUrl,
    framesUrl,
    nestedFramesUrl,
    contentPageUrl,
    indexUrl,
    shadowDomUrl,
    inputsUrl,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text, pack, unpack)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import WebDriverPreCore.HTTP.Protocol (URL (..))
import WebDriverPreCore.Test.IOUtils (findWebDriverRoot)

testFilesDir :: MonadIO m => m FilePath
testFilesDir = do
  currentDir <- liftIO getCurrentDirectory
  case findWebDriverRoot currentDir of
    Just root -> pure $ root </> testFilesSubDir
    Nothing ->
      error $
        "Could not find 'webdriver' root directory from: "
          <> currentDir
          <> "\n tests are expected to be run from the 'webdriver' directory or "
          <> testFilesSubDir
  where
    testFilesSubDir = "webdriver-precore" </> "test-resources" </> "testFiles"

testPath :: MonadIO m => FilePath -> m Text
testPath filename =
  pack . (</> filename) <$> testFilesDir

fileUrl :: MonadIO m => FilePath -> m URL
fileUrl fp = MkUrl . ((<>) "file://") <$> testPath fp

-- | Get absolute file path for upload test files
uploadFilePath :: MonadIO m => FilePath -> m Text
uploadFilePath filename = do
  testDir <- testFilesDir
  pure . pack $ testDir </> "uploadFiles" </> filename

demoExtensionDirPath :: MonadIO m => m Text
demoExtensionDirPath = testPath "demoExtension"

demoExtensionZipPath :: MonadIO m => m Text
demoExtensionZipPath = testPath "demoExtension.zip"

demoExtensionAsBase64 :: MonadIO m => m Text
demoExtensionAsBase64 = do
  zipPath <- demoExtensionZipPath
  zipContent <- liftIO $ BS.readFile (unpack zipPath)
  pure $ B64T.extractBase64 $ B64.encodeBase64 zipContent

textAreaUrl :: MonadIO m => m URL
textAreaUrl = fileUrl "textArea.html"

checkboxesUrl :: MonadIO m => m URL
checkboxesUrl = fileUrl "checkboxes.html"

infiniteScrollUrl :: MonadIO m => m URL
infiniteScrollUrl = fileUrl "infiniteScroll.html"

promptUrl :: MonadIO m => m URL
promptUrl = fileUrl "prompt.html"

fragmentUrl :: MonadIO m => m URL
fragmentUrl = fileUrl "fragment.html"

downloadUrl :: MonadIO m => m URL
downloadUrl = fileUrl "download.html"

slowLoadUrl :: MonadIO m => m URL
slowLoadUrl = fileUrl "slowLoad.html"

downloadLinkUrl :: MonadIO m => m URL
downloadLinkUrl = fileUrl "downloadLink.html"

consoleLogUrl :: MonadIO m => m URL
consoleLogUrl = fileUrl "consoleLog.html"

scriptRealmUrl :: MonadIO m => m URL
scriptRealmUrl = fileUrl "scriptRealm.html"

badJavaScriptUrl :: MonadIO m => m URL
badJavaScriptUrl = fileUrl "badJavaScript.html"

uploadUrl :: MonadIO m => m URL
uploadUrl = fileUrl "upload.html"

navigation1Url :: MonadIO m => m URL
navigation1Url = fileUrl "navigation1.html"

navigation2Url :: MonadIO m => m URL
navigation2Url = fileUrl "navigation2.html"

navigation3Url :: MonadIO m => m URL
navigation3Url = fileUrl "navigation3.html"

navigation4Url :: MonadIO m => m URL
navigation4Url = fileUrl "navigation4.html"

navigation5Url :: MonadIO m => m URL
navigation5Url = fileUrl "navigation5.html"

navigation6Url :: MonadIO m => m URL
navigation6Url = fileUrl "navigation6.html"

loginUrl :: MonadIO m => m URL
loginUrl = fileUrl "login.html"

framesUrl :: MonadIO m => m URL
framesUrl = fileUrl "frames.html"

nestedFramesUrl :: MonadIO m => m URL
nestedFramesUrl = fileUrl "nestedFrames.html"

contentPageUrl :: MonadIO m => m URL
contentPageUrl = fileUrl "contentPage.html"

indexUrl :: MonadIO m => m URL
indexUrl = fileUrl "index.html"

shadowDomUrl :: MonadIO m => m URL
shadowDomUrl = fileUrl "shadowDom.html"

inputsUrl :: MonadIO m => m URL
inputsUrl = fileUrl "inputs.html"
