module TestPages where

import Data.Text (Text, pack)
import System.Directory (canonicalizePath)
import Prelude

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

textAreaUrl :: IO Text
textAreaUrl = fileUrl "textArea.html"

checkboxesUrl :: IO Text
checkboxesUrl = fileUrl "checkboxes.html"

infiniteScrollUrl :: IO Text
infiniteScrollUrl = fileUrl "infiniteScroll.html"