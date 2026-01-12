module TestData where

import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text, pack, unpack)
import IOUtils (findWebDriverRoot)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import WebDriverPreCore.HTTP.Protocol (URL (..))

testFilesDir :: IO FilePath
testFilesDir = do
  currentDir <- getCurrentDirectory
  case findWebDriverRoot currentDir of
    Just root -> pure $ root </> testFilesSubDir
    Nothing ->
      error $
        "Could not find 'webdriver' root directory from: "
          <> currentDir
          <> "\n tests are expected to be run from the 'webdriver' directory or "
          <> testFilesSubDir
  where
    testFilesSubDir = "webdriver-precore" </> "test" </> "testFiles"

testPath :: FilePath -> IO Text
testPath filename =
  pack . (</> filename) <$> testFilesDir

fileUrl :: FilePath -> IO URL
fileUrl fp =  MkUrl <$> ((<>) "file://") <$> testPath fp

-- | Get absolute file path for upload test files
uploadFilePath :: FilePath -> IO Text
uploadFilePath filename = do
  testDir <- testFilesDir
  pure . pack $ testDir </> "uploadFiles" </> filename

demoExtensionDirPath :: IO Text
demoExtensionDirPath = testPath "demoExtension"

demoExtensionZipPath :: IO Text
demoExtensionZipPath = testPath "demoExtension.zip"

demoExtensionAsBase64 :: IO Text
demoExtensionAsBase64 = do
  zipPath <- demoExtensionZipPath
  zipContent <- BS.readFile (unpack zipPath)
  pure $ B64T.extractBase64 $ B64.encodeBase64 zipContent

textAreaUrl :: IO URL
textAreaUrl = fileUrl "textArea.html"

checkboxesUrl :: IO URL
checkboxesUrl = fileUrl "checkboxes.html"

infiniteScrollUrl :: IO URL
infiniteScrollUrl = fileUrl "infiniteScroll.html"

promptUrl :: IO URL
promptUrl = fileUrl "prompt.html"

fragmentUrl :: IO URL
fragmentUrl = fileUrl "fragment.html"

downloadUrl :: IO URL
downloadUrl = fileUrl "download.html"

slowLoadUrl :: IO URL
slowLoadUrl = fileUrl "slowLoad.html"

downloadLinkUrl :: IO URL
downloadLinkUrl = fileUrl "downloadLink.html"

consoleLogUrl :: IO URL
consoleLogUrl = fileUrl "consoleLog.html"

scriptRealmUrl :: IO URL
scriptRealmUrl = fileUrl "scriptRealm.html"

badJavaScriptUrl :: IO URL
badJavaScriptUrl = fileUrl "badJavaScript.html"

uploadUrl :: IO URL
uploadUrl = fileUrl "upload.html"

navigation1Url :: IO URL
navigation1Url = fileUrl "navigation1.html"

navigation2Url :: IO URL
navigation2Url = fileUrl "navigation2.html"

navigation3Url :: IO URL
navigation3Url = fileUrl "navigation3.html"

navigation4Url :: IO URL
navigation4Url = fileUrl "navigation4.html"

navigation5Url :: IO URL
navigation5Url = fileUrl "navigation5.html"

navigation6Url :: IO URL
navigation6Url = fileUrl "navigation6.html"

loginUrl :: IO URL
loginUrl = fileUrl "login.html"

framesUrl :: IO URL
framesUrl = fileUrl "frames.html"

nestedFramesUrl :: IO URL
nestedFramesUrl = fileUrl "nestedFrames.html"

contentPageUrl :: IO URL
contentPageUrl = fileUrl "contentPage.html"

indexUrl :: IO URL
indexUrl = fileUrl "index.html"

shadowDomUrl :: IO URL
shadowDomUrl = fileUrl "shadowDom.html"

inputsUrl :: IO URL
inputsUrl = fileUrl "inputs.html"