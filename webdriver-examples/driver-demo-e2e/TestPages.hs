module TestPages
  ( mkTestPage
  ) where

import Data.Text (Text)
import Prelude


-- | Create a data URI test page by prepending "data:text/html," to the given HTML content
mkTestPage :: Text -> Text
mkTestPage = (<>) "data:text/html,"

{-- 
textAreaPage :: Text
 [text|
<!DOCTYPE html>
<html>
<head>
  <title>Test Page</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      max-width: 800px;
      margin: 40px auto;
      padding: 20px;
      background-color: #f5f5f5;
    }
    .container {
      background-color: white;
      padding: 30px;
      border-radius: 8px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    }
    h1 {
      color: #333;
      margin-bottom: 20px;
    }
    label {
      display: block;
      margin-bottom: 8px;
      font-weight: bold;
      color: #555;
    }
    textarea {
      width: 100%;
      height: 200px;
      padding: 12px;
      border: 2px solid #ddd;
      border-radius: 4px;
      font-size: 14px;
      resize: vertical;
    }
    textarea:focus {
      outline: none;
      border-color: #4CAF50;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>Test Page</h1>
    <label for="testTextArea">Enter your text:</label>
    <textarea id="testTextArea" name="testTextArea" placeholder="Type something here..."></textarea>
  </div>
</body>
</html>|]

-}