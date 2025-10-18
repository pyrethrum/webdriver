module TestData where

import Data.Base64.Types qualified as B64T
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text, pack, unpack)
import IOUtils (findWebDriverRoot)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.FilePath ((</>))
import WebDriverPreCore.Internal.Utils (db)
import Prelude


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
    testFilesSubDir = "webdriver-precore" </> "test" </> "TestFiles"

testPath :: FilePath -> IO Text
testPath filename = 
   pack . (</> filename) <$> testFilesDir


fileUrl :: FilePath -> IO Text
fileUrl fp = fmap ((<>) "file://") (testPath fp)

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

textAreaUrl :: IO Text
textAreaUrl = fileUrl "textArea.html"

checkboxesUrl :: IO Text
checkboxesUrl = fileUrl "checkboxes.html"

infiniteScrollUrl :: IO Text
infiniteScrollUrl = fileUrl "infiniteScroll.html"

promptUrl :: IO Text
promptUrl = fileUrl "prompt.html"

fragmentUrl :: IO Text
fragmentUrl = fileUrl "fragment.html"

downloadUrl :: IO Text
downloadUrl = fileUrl "download.html"

slowLoadUrl :: IO Text
slowLoadUrl = fileUrl "slowLoad.html"

downloadLinkUrl :: IO Text
downloadLinkUrl = fileUrl "downloadLink.html"

consoleLogUrl :: IO Text
consoleLogUrl = fileUrl "consoleLog.html"

scriptRealmUrl :: IO Text
scriptRealmUrl = fileUrl "scriptRealm.html"

badJavaScriptUrl :: IO Text
badJavaScriptUrl = fileUrl "badJavaScript.html"

uploadUrl :: IO Text
uploadUrl = fileUrl "upload.html"

navigation1Url :: IO Text
navigation1Url = fileUrl "navigation1.html"

navigation2Url :: IO Text
navigation2Url = fileUrl "navigation2.html"

navigation3Url :: IO Text
navigation3Url = fileUrl "navigation3.html"

navigation4Url :: IO Text
navigation4Url = fileUrl "navigation4.html"

navigation5Url :: IO Text
navigation5Url = fileUrl "navigation5.html"

navigation6Url :: IO Text
navigation6Url = fileUrl "navigation6.html"

loginUrl :: IO Text
loginUrl = fileUrl "login.html"

framesUrl :: IO Text
framesUrl = fileUrl "frames.html"

nestedFramesUrl :: IO Text
nestedFramesUrl = fileUrl "nestedFrames.html"

contentPageUrl :: IO Text
contentPageUrl = fileUrl "contentPage.html"