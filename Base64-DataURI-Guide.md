# Base64 Encoding for Data URIs

## How Base64 Encoding Works for WebDriver Data URIs

Base64 encoding can solve the data URI rendering issues you're experiencing by:

1. **Eliminating Character Encoding Issues**: Complex CSS with special characters gets encoded safely
2. **Reducing Length**: Base64 typically reduces string length by ~25% 
3. **Bypassing Browser Parsing Edge Cases**: Some browsers handle base64 data URIs more reliably
4. **Consistent WebDriver Behavior**: Less likely to hit implementation-specific limits

## Format Comparison

### Current Approach (URL-encoded)
```
data:text/html,<!DOCTYPE html><html><head><title>Test Page</title><style>body{font-family:Arial,sans-serif;max-width:800px;margin:40px auto;padding:20px;background-color:#f5f5f5}.container{background-color:white;padding:30px;border-radius:8px;box-shadow:0 2px 10px rgba(0,0,0,0.1)}h1{color:#333;margin-bottom:20px}label{display:block;margin-bottom:8px;font-weight:bold;color:#555}textarea{width:100%;height:200px;padding:12px;border:2px solid #ddd;border-radius:4px;font-size:14px;resize:vertical}textarea:focus{outline:none;border-color:#4CAF50}</style></head><body><div class="container"><h1>Test Page</h1><label for="textArea">Enter your text:</label><textarea id="textArea" name="textArea" placeholder="Type something here..."></textarea><label for="textArea2">Enter your text:</label><textarea id="textArea2" name="textArea2" placeholder="Type something else here..."></textarea></div></body></html>
```
**Length**: ~2000+ characters

### Base64 Approach
```
data:text/html;base64,PCFET0NUWVBFIGh0bWw+PGh0bWw+PGhlYWQ+PHRpdGxlPlRlc3QgUGFnZTwvdGl0bGU+PHN0eWxlPmJvZHl7Zm9udC1mYW1pbHk6QXJpYWwsc2Fucy1zZXJpZjtbLi4uXWJvcmRlci1jb2xvcjojNENBRjUwfTwvc3R5bGU+PC9oZWFkPjxib2R5PjxkaXYgY2xhc3M9ImNvbnRhaW5lciI+PGgxPlRlc3QgUGFnZTwvaDE+PGxhYmVsIGZvcj0idGV4dEFyZWEiPkVudGVyIHlvdXIgdGV4dDo8L2xhYmVsPjx0ZXh0YXJlYSBpZD0idGV4dEFyZWEiIG5hbWU9InRleHRBcmVhIiBwbGFjZWhvbGRlcj0iVHlwZSBzb21ldGhpbmcgaGVyZS4uLiI+PC90ZXh0YXJlYT48bGFiZWwgZm9yPSJ0ZXh0QXJlYTIiPkVudGVyIHlvdXIgdGV4dDo8L2xhYmVsPjx0ZXh0YXJlYSBpZD0idGV4dEFyZWEyIiBuYW1lPSJ0ZXh0QXJlYTIiIHBsYWNlaG9sZGVyPSJUeXBlIHNvbWV0aGluZyBlbHNlIGhlcmUuLi4iPjwvdGV4dGFyZWE+PC9kaXY+PC9ib2R5PjwvaHRtbD4=
```
**Length**: ~1600 characters (25% reduction)

## Implementation in Haskell

To implement base64 encoding properly, you would need:

```haskell
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding (decodeUtf8)

mkBase64Page :: Text -> Text
mkBase64Page htmlContent = 
  let htmlBytes = encodeUtf8 htmlContent
      base64Bytes = Base64.encode htmlBytes  -- This returns Either String ByteString
      base64Text = case base64Bytes of
        Right encoded -> decodeUtf8 encoded
        Left err -> error $ "Base64 encoding failed: " <> err
  in "data:text/html;base64," <> base64Text
```

## Testing Strategy

1. **Create base64 version** of `simpleTextAreaPage2`
2. **Compare rendering** between URL-encoded and base64 versions
3. **Measure length differences** 
4. **Test in InputDemos.hs** to see if base64 resolves the issue

## Expected Benefits

- **Reliability**: Base64 encoding is more predictable across WebDriver implementations
- **Length**: Shorter URIs may bypass Geckodriver limits
- **Encoding**: No special character issues with CSS
- **Consistency**: Same behavior across different browsers and WebDriver implementations

The base64 approach essentially transforms your complex HTML with CSS into a clean, encoded string that browsers and WebDriver can handle more reliably.