{-|
Module: WebDriverPreCore.TestResources
Description: Embedded test resources for WebDriver tests

This module provides access to test resources (HTML files, etc.) that are
embedded at compile time using file-embed.
-}
module WebDriverPreCore.TestResources
  ( -- * HTML Test Files
    getTestFile,
    getTestFileText,
    testFileNames,
    
    -- * Individual Test Files
    indexHtml,
    contentPageHtml,
    checkboxesHtml,
    framesHtml,
    nestedFramesHtml,
    nestedFramesTopHtml,
    inputsHtml,
    loginHtml,
    promptHtml,
    shadowDomHtml,
    textAreaHtml,
    uploadHtml,
    navigationHtml1,
    navigationHtml2,
    navigationHtml3,
    navigationHtml4,
    navigationHtml5,
    navigationHtml6,
    fragmentHtml,
    slowLoadHtml,
    infiniteScrollHtml,
    consoleLogHtml,
    badJavaScriptHtml,
    downloadHtml,
    downloadLinkHtml,
    scriptRealmHtml,
    demoCss,
    
    -- * Demo Extension
    demoExtensionZip,

    -- * Attribute Test
    attributeTestHtml,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, embedFileRelative, makeRelativeToProject)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

-- | Get a test file by name as ByteString
getTestFile :: String -> Maybe ByteString
getTestFile name = lookup name testFiles

-- | Get a test file by name as Text
getTestFileText :: String -> Maybe Text
getTestFileText = fmap decodeUtf8 . getTestFile

-- | List all available test file names
testFileNames :: [String]
testFileNames = fst <$> testFiles

-- | All embedded test files
testFiles :: [(String, ByteString)]
testFiles = $(makeRelativeToProject "test-resources/testFiles" >>= embedDir)

-- Individual file accessors for common files
indexHtml :: ByteString
indexHtml = $(embedFileRelative "test-resources/testFiles/index.html")

contentPageHtml :: ByteString
contentPageHtml = $(embedFileRelative "test-resources/testFiles/contentPage.html")

checkboxesHtml :: ByteString
checkboxesHtml = $(embedFileRelative "test-resources/testFiles/checkboxes.html")

framesHtml :: ByteString
framesHtml = $(embedFileRelative "test-resources/testFiles/frames.html")

nestedFramesHtml :: ByteString
nestedFramesHtml = $(embedFileRelative "test-resources/testFiles/nestedFrames.html")

nestedFramesTopHtml :: ByteString
nestedFramesTopHtml = $(embedFileRelative "test-resources/testFiles/nestedFramesTop.html")

inputsHtml :: ByteString
inputsHtml = $(embedFileRelative "test-resources/testFiles/inputs.html")

loginHtml :: ByteString
loginHtml = $(embedFileRelative "test-resources/testFiles/login.html")

promptHtml :: ByteString
promptHtml = $(embedFileRelative "test-resources/testFiles/prompt.html")

shadowDomHtml :: ByteString
shadowDomHtml = $(embedFileRelative "test-resources/testFiles/shadowDom.html")

textAreaHtml :: ByteString
textAreaHtml = $(embedFileRelative "test-resources/testFiles/textArea.html")

uploadHtml :: ByteString
uploadHtml = $(embedFileRelative "test-resources/testFiles/upload.html")

navigationHtml1 :: ByteString
navigationHtml1 = $(embedFileRelative "test-resources/testFiles/navigation1.html")

navigationHtml2 :: ByteString
navigationHtml2 = $(embedFileRelative "test-resources/testFiles/navigation2.html")

navigationHtml3 :: ByteString
navigationHtml3 = $(embedFileRelative "test-resources/testFiles/navigation3.html")

navigationHtml4 :: ByteString
navigationHtml4 = $(embedFileRelative "test-resources/testFiles/navigation4.html")

navigationHtml5 :: ByteString
navigationHtml5 = $(embedFileRelative "test-resources/testFiles/navigation5.html")

navigationHtml6 :: ByteString
navigationHtml6 = $(embedFileRelative "test-resources/testFiles/navigation6.html")

fragmentHtml :: ByteString
fragmentHtml = $(embedFileRelative "test-resources/testFiles/fragment.html")

slowLoadHtml :: ByteString
slowLoadHtml = $(embedFileRelative "test-resources/testFiles/slowLoad.html")

infiniteScrollHtml :: ByteString
infiniteScrollHtml = $(embedFileRelative "test-resources/testFiles/infiniteScroll.html")

consoleLogHtml :: ByteString
consoleLogHtml = $(embedFileRelative "test-resources/testFiles/consoleLog.html")

badJavaScriptHtml :: ByteString
badJavaScriptHtml = $(embedFileRelative "test-resources/testFiles/badJavaScript.html")

downloadHtml :: ByteString
downloadHtml = $(embedFileRelative "test-resources/testFiles/download.html")

downloadLinkHtml :: ByteString
downloadLinkHtml = $(embedFileRelative "test-resources/testFiles/downloadLink.html")

scriptRealmHtml :: ByteString
scriptRealmHtml = $(embedFileRelative "test-resources/testFiles/scriptRealm.html")

demoCss :: ByteString
demoCss = $(embedFileRelative "test-resources/testFiles/demo.css")

demoExtensionZip :: ByteString
demoExtensionZip = $(embedFileRelative "test-resources/testFiles/demoExtension.zip")

attributeTestHtml :: ByteString
attributeTestHtml = $(embedFileRelative "test-resources/testFiles/attributeTest.html")
