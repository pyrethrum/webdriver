module TestPagesBase64 where

import Data.Text (Text)
import NeatInterpolation
import Prelude

-- | Create a regular data URI (for comparison)
mkPage :: Text -> Text
mkPage = (<>) "data:text/html,"

css :: Text
css =
  [text|body{font-family:Arial,sans-serif;max-width:800px;margin:40px auto;padding:20px;background-color:#f5f5f5}.container{background-color:white;padding:30px;border-radius:8px;box-shadow:0 2px 10px rgba(0,0,0,0.1)}h1{color:#333;margin-bottom:20px}label{display:block;margin-bottom:8px;font-weight:bold;color:#555}textarea{width:100%;height:200px;padding:12px;border:2px solid #ddd;border-radius:4px;font-size:14px;resize:vertical}textarea:focus{outline:none;border-color:#4CAF50}|]

-- Full HTML content for base64 encoding
fullHtmlContent :: Text
fullHtmlContent = [text|<!DOCTYPE html><html><head><title>Test Page</title><style>${css}</style></head><body><div class="container"><h1>Test Page</h1><label for="textArea">Enter your text:</label><textarea id="textArea" name="textArea" placeholder="Type something here..."></textarea><label for="textArea2">Enter your text:</label><textarea id="textArea2" name="textArea2" placeholder="Type something else here..."></textarea></div></body></html>|]

-- Base64-encoded version of the complex page (pre-computed)
textAreaPageBase64 :: Text
textAreaPageBase64 = "data:text/html;base64,PCFET0NUWVBFIGh0bWw+PGh0bWw+PGhlYWQ+PHRpdGxlPlRlc3QgUGFnZTwvdGl0bGU+PHN0eWxlPmJvZHl7Zm9udC1mYW1pbHk6QXJpYWwsc2Fucy1zZXJpZjttYXgtd2lkdGg6ODAwcHg7bWFyZ2luOjQwcHggYXV0bztwYWRkaW5nOjIwcHg7YmFja2dyb3VuZC1jb2xvcjojZjVmNWY1fS5jb250YWluZXJ7YmFja2dyb3VuZC1jb2xvcjp3aGl0ZTtwYWRkaW5nOjMwcHg7Ym9yZGVyLXJhZGl1czo4cHg7Ym94LXNoYWRvdzowIDJweCAxMHB4IHJnYmEoMCwwLDAsMC4xKX1oMXtjb2xvcjojMzMzO21hcmdpbi1ib3R0b206MjBweH1sYWJlbHtkaXNwbGF5OmJsb2NrO21hcmdpbi1ib3R0b206OHB4O2ZvbnQtd2VpZ2h0OmJvbGQ7Y29sb3I6IzU1NX10ZXh0YXJlYXt3aWR0aDoxMDAlO2hlaWdodDoyMDBweDtwYWRkaW5nOjEycHg7Ym9yZGVyOjJweCBzb2xpZCAjZGRkO2JvcmRlci1yYWRpdXM6NHB4O2ZvbnQtc2l6ZToxNHB4O3Jlc2l6ZTp2ZXJ0aWNhbH10ZXh0YXJlYTpmb2N1c3tvdXRsaW5lOm5vbmU7Ym9yZGVyLWNvbG9yOiM0Q0FGNTB9PC9zdHlsZT48L2hlYWQ+PGJvZHk+PGRpdiBjbGFzcz0iY29udGFpbmVyIj48aDE+VGVzdCBQYWdlPC9oMT48bGFiZWwgZm9yPSJ0ZXh0QXJlYSI+RW50ZXIgeW91ciB0ZXh0OjwvbGFiZWw+PHRleHRhcmVhIGlkPSJ0ZXh0QXJlYSIgbmFtZT0idGV4dEFyZWEiIHBsYWNlaG9sZGVyPSJUeXBlIHNvbWV0aGluZyBoZXJlLi4uIj48L3RleHRhcmVhPjxsYWJlbCBmb3I9InRleHRBcmVhMiI+RW50ZXIgeW91ciB0ZXh0OjwvbGFiZWw+PHRleHRhcmVhIGlkPSJ0ZXh0QXJlYTIiIG5hbWU9InRleHRBcmVhMiIgcGxhY2Vob2xkZXI9IlR5cGUgc29tZXRoaW5nIGVsc2UgaGVyZS4uLiI+PC90ZXh0YXJlYT48L2Rpdj48L2JvZHk+PC9odG1sPg=="

-- Simple page for comparison (no base64)
simpleTextAreaPage :: Text
simpleTextAreaPage = mkPage [text|<html><body><h1>Test Page</h1><textarea id="textArea" placeholder="Type here"></textarea><textarea id="textArea2" placeholder="Type here too"></textarea></body></html>|]

-- Simple page with base64 encoding (pre-computed)
simpleTextAreaPageBase64 :: Text
simpleTextAreaPageBase64 = "data:text/html;base64,PGh0bWw+PGJvZHk+PGgxPlRlc3QgUGFnZTwvaDE+PHRleHRhcmVhIGlkPSJ0ZXh0QXJlYSIgcGxhY2Vob2xkZXI9IlR5cGUgaGVyZSI+PC90ZXh0YXJlYT48dGV4dGFyZWEgaWQ9InRleHRBcmVhMiIgcGxhY2Vob2xkZXI9IlR5cGUgaGVyZSB0b28iPjwvdGV4dGFyZWE+PC9ib2R5PjwvaHRtbD4="