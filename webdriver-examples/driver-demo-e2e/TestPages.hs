module TestPages

where

import Data.Text (Text)
import NeatInterpolation
import Prelude

-- | Create a data URI test page by prepending "data:text/html," to the given HTML content
mkPage :: Text -> Text
mkPage = (<>) "data:text/html,"

-- | Example of how base64-encoded data URI would look (conceptual)
-- mkBase64Page :: Text -> Text
-- mkBase64Page htmlContent = 
--   let base64Content = encodeToBase64 htmlContent  -- pseudocode
--   in "data:text/html;base64," <> base64Content

css :: Text
css =
  [text|body{font-family:Arial,sans-serif;max-width:800px;margin:40px auto;padding:20px;background-color:#f5f5f5}.container{background-color:white;padding:30px;border-radius:8px;box-shadow:0 2px 10px rgba(0,0,0,0.1)}h1{color:#333;margin-bottom:20px}label{display:block;margin-bottom:8px;font-weight:bold;color:#555}textarea{width:100%;height:200px;padding:12px;border:2px solid #ddd;border-radius:4px;font-size:14px;resize:vertical}textarea:focus{outline:none;border-color:#4CAF50}|]

-- Simple test page to verify data URI works
simpleTextAreaPage :: Text
simpleTextAreaPage = mkPage [text|<html><body><h1>Test Page</h1><textarea id="textArea" placeholder="Type here"></textarea><textarea id="textArea2" placeholder="Type here too"></textarea></body></html>|]

-- Text area page with inlined CSS  
simpleTextAreaPage2 :: Text
simpleTextAreaPage2 = mkPage [text|<!DOCTYPE html><html><head><title>Test Page</title><style>body{font-family:Arial,sans-serif;max-width:800px;margin:40px auto;padding:20px;background-color:#f5f5f5}.container{background-color:white;padding:30px;border-radius:8px;box-shadow:0 2px 10px rgba(0,0,0,0.1)}h1{color:#333;margin-bottom:20px}label{display:block;margin-bottom:8px;font-weight:bold;color:#555}textarea{width:100%;height:200px;padding:12px;border:2px solid #ddd;border-radius:4px;font-size:14px;resize:vertical}textarea:focus{outline:none;border-color:#4CAF50}</style></head><body><div class="container"><h1>Test Page</h1><label for="textArea">Enter your text:</label><textarea id="textArea" name="textArea" placeholder="Type something here..."></textarea><label for="textArea2">Enter your text:</label><textarea id="textArea2" name="textArea2" placeholder="Type something else here..."></textarea></div></body></html>|]

-- Base64 encoded version of the simple page (ready to test)
simpleTextAreaPageBase64 :: Text
simpleTextAreaPageBase64 = "data:text/html;base64,PGh0bWw+PGJvZHk+PGgxPlRlc3QgUGFnZTwvaDE+PHRleHRhcmVhIGlkPSJ0ZXh0QXJlYSIgcGxhY2Vob2xkZXI9IlR5cGUgaGVyZSI+PC90ZXh0YXJlYT48dGV4dGFyZWEgaWQ9InRleHRBcmVhMiIgcGxhY2Vob2xkZXI9IlR5cGUgaGVyZSB0b28iPjwvdGV4dGFyZWE+PC9ib2R5PjwvaHRtbD4K"

textAreaPage :: Text
textAreaPage =
  mkPage [text|<!DOCTYPE html><html><head><title>Test Page</title><style>${css}</style></head><body><div class="container"><h1>Test Page</h1><label for="textArea">Enter your text:</label><textarea id="textArea" name="textArea" placeholder="Type something here..."></textarea><label for="textArea2">Enter your text:</label><textarea id="textArea2" name="textArea2" placeholder="Type something else here..."></textarea></div></body></html>|]

-- | Example showing how base64 encoding would work
-- This would help with:
-- 1. Character encoding issues (special chars, CSS syntax)
-- 2. URL length limitations in some WebDriver implementations  
-- 3. Browser parsing edge cases with complex data URIs
-- 
-- Format: data:text/html;base64,<base64-encoded-html>
-- 
-- Benefits of base64 for data URIs:
-- - Handles any HTML content safely
-- - No need for URL encoding
-- - Consistent across all browsers
-- - Bypasses some WebDriver/Geckodriver length restrictions
--
-- textAreaPageBase64 :: Text  
-- textAreaPageBase64 = "data:text/html;base64," <> base64EncodedHtml
--   where base64EncodedHtml = encodeBase64 fullHtmlWithCSS

-- | Length comparison - URL encoded vs Base64 for problematic data URIs:
--
-- Current failing approach (URL encoded):
-- Length: ~2000+ characters
-- Issues: Complex CSS, special characters, browser limits
--
-- Base64 approach would be:
-- Length: ~1600 characters (25% reduction)  
-- Benefits: Cleaner, more reliable, consistent parsing
