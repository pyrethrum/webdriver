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
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, embedDir)
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
testFiles = $(embedDir "test-resources/testFiles")

-- Individual file accessors for common files
indexHtml :: ByteString
indexHtml = $(embedFile "test-resources/testFiles/index.html")

contentPageHtml :: ByteString
contentPageHtml = $(embedFile "test-resources/testFiles/contentPage.html")

checkboxesHtml :: ByteString
checkboxesHtml = $(embedFile "test-resources/testFiles/checkboxes.html")

framesHtml :: ByteString
framesHtml = $(embedFile "test-resources/testFiles/frames.html")

nestedFramesHtml :: ByteString
nestedFramesHtml = $(embedFile "test-resources/testFiles/nestedFrames.html")

nestedFramesTopHtml :: ByteString
nestedFramesTopHtml = $(embedFile "test-resources/testFiles/nestedFramesTop.html")

inputsHtml :: ByteString
inputsHtml = $(embedFile "test-resources/testFiles/inputs.html")

loginHtml :: ByteString
loginHtml = $(embedFile "test-resources/testFiles/login.html")

promptHtml :: ByteString
promptHtml = $(embedFile "test-resources/testFiles/prompt.html")

shadowDomHtml :: ByteString
shadowDomHtml = $(embedFile "test-resources/testFiles/shadowDom.html")

textAreaHtml :: ByteString
textAreaHtml = $(embedFile "test-resources/testFiles/textArea.html")

uploadHtml :: ByteString
uploadHtml = $(embedFile "test-resources/testFiles/upload.html")

navigationHtml1 :: ByteString
navigationHtml1 = $(embedFile "test-resources/testFiles/navigation1.html")

navigationHtml2 :: ByteString
navigationHtml2 = $(embedFile "test-resources/testFiles/navigation2.html")

navigationHtml3 :: ByteString
navigationHtml3 = $(embedFile "test-resources/testFiles/navigation3.html")

navigationHtml4 :: ByteString
navigationHtml4 = $(embedFile "test-resources/testFiles/navigation4.html")

navigationHtml5 :: ByteString
navigationHtml5 = $(embedFile "test-resources/testFiles/navigation5.html")

navigationHtml6 :: ByteString
navigationHtml6 = $(embedFile "test-resources/testFiles/navigation6.html")

fragmentHtml :: ByteString
fragmentHtml = $(embedFile "test-resources/testFiles/fragment.html")

slowLoadHtml :: ByteString
slowLoadHtml = $(embedFile "test-resources/testFiles/slowLoad.html")

infiniteScrollHtml :: ByteString
infiniteScrollHtml = $(embedFile "test-resources/testFiles/infiniteScroll.html")

consoleLogHtml :: ByteString
consoleLogHtml = $(embedFile "test-resources/testFiles/consoleLog.html")

badJavaScriptHtml :: ByteString
badJavaScriptHtml = $(embedFile "test-resources/testFiles/badJavaScript.html")

downloadHtml :: ByteString
downloadHtml = $(embedFile "test-resources/testFiles/download.html")

downloadLinkHtml :: ByteString
downloadLinkHtml = $(embedFile "test-resources/testFiles/downloadLink.html")

scriptRealmHtml :: ByteString
scriptRealmHtml = $(embedFile "test-resources/testFiles/scriptRealm.html")

demoCss :: ByteString
demoCss = $(embedFile "test-resources/testFiles/demo.css")

demoExtensionZip :: ByteString
demoExtensionZip = $(embedFile "test-resources/testFiles/demoExtension.zip")
