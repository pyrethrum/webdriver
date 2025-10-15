module TestData where

import Data.Text (Text, pack, unpack)
import System.Directory (canonicalizePath)
import Prelude
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Base64.Types qualified as B64T

testDir :: FilePath
testDir = "webdriver-examples/driver-demo-e2e/TestFiles/"

testPath :: FilePath -> IO Text
testPath filename = pack <$> canonicalizePath (testDir <> filename)


fileUrl :: FilePath -> IO Text
fileUrl = fmap ((<>) "file://") . testPath

-- | Get absolute file path for upload test files
uploadFilePath :: FilePath -> IO Text
uploadFilePath filename = pack <$> canonicalizePath (testDir <> "uploadFiles/" <> filename)


demoExtensionDirPath :: IO Text
demoExtensionDirPath = testPath "demoExtension/"

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