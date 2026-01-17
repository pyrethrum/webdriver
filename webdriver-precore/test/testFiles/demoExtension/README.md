# BiDiDemoExtension

A simple web extension designed for WebDriver BiDi testing and automation.

## Features

- **Visible indicator**: Adds a green "BiDiDemoExtension Active" badge to every webpage
- **Background script**: Logs extension activity to console
- **Content script**: Injects testing utilities into web pages
- **Browser action**: Clickable toolbar icon with popup
- **Storage**: Tracks installation and usage data
- **WebDriver-friendly**: Exposes testing functions for automation

## Observable Behaviors

When installed, this extension will:

1. **Add visual indicator**: Green badge appears in top-right corner of every webpage
2. **Console logging**: Extension activity is logged to browser console
3. **Global functions**: Adds `window.getBiDiDemoExtensionStatus()` function to pages
4. **Storage data**: Sets extension status in browser storage
5. **Toolbar icon**: Shows "B" icon in browser toolbar with popup

## Installation

### Chrome/Edge
1. Open `chrome://extensions/` or `edge://extensions/`
2. Enable "Developer mode"
3. Click "Load unpacked"
4. Select the `demoExtension` folder

### Firefox
1. Open `about:debugging`
2. Click "This Firefox"
3. Click "Load Temporary Add-on"
4. Select any file in the `demoExtension` folder

## Testing with WebDriver

### Check Extension Status
```javascript
// Execute script to check if extension is active
const status = await driver.executeScript("return window.getBiDiDemoExtensionStatus()");
console.log(status);
// Returns: { active: true, name: "BiDiDemoExtension", hasIndicator: true, ... }
```

### Verify Visual Indicator
```javascript
// Find the extension indicator element
const indicator = await driver.findElement(By.id("bidi-demo-extension-indicator"));
const text = await indicator.getText();
// Should return: "BiDiDemoExtension Active"
```

### Check Browser Storage
```javascript
// Check extension storage (requires appropriate permissions)
const storage = await driver.executeScript(`
  return new Promise(resolve => {
    chrome.storage.local.get(['bidi_demo_extension_active'], resolve);
  });
`);
```

### Interact with Extension
```javascript
// Click the toolbar button (if visible)
// Note: This requires finding the extension button which varies by browser
```

## Files Structure

- `manifest.json` - Extension configuration
- `background.js` - Background script (service worker)
- `content.js` - Content script injected into pages
- `popup.html` - Toolbar button popup UI
- `popup.js` - Popup functionality
- `icons/` - Extension icons (SVG format)

## Manifest Version

This extension uses Manifest V2 for broad compatibility. For Manifest V3 compatibility:
- Change `manifest_version` to 3
- Update `background` to use `service_worker`
- Update API calls from `chrome.*` to `browser.*` where needed

## Console Output

Expected console messages when extension is working:
- "BiDiDemoExtension: Background script loaded"
- "BiDiDemoExtension: Extension installed"
- "BiDiDemoExtension: Content script loaded on: [URL]"