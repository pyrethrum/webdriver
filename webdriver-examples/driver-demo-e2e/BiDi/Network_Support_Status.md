# WebDriver BiDi Network Module Support Status

## Current Implementation Status (as of September 2025)

The WebDriver BiDi network module is relatively new and support varies across different WebDriver implementations:

### Firefox (Geckodriver)
- **Status**: ⚠️ Partial network module support (in active development)
- **Supported Commands**: Some network events and basic intercepts (experimental)
- **Unsupported Commands**: Most network manipulation commands (addDataCollector, continueRequest, etc.)
- **Key Tracking Bugs**: 
  - [Bug 1971763 - Support network.addDataCollector](https://bugzilla.mozilla.org/show_bug.cgi?id=1971763) (REOPENED - partial implementation)
  - [Bug 1826191 - Support network.addIntercept](https://bugzilla.mozilla.org/show_bug.cgi?id=1826191) (REOPENED - experimental support)
  - [Search all network BiDi bugs](https://bugzilla.mozilla.org/buglist.cgi?quicksearch=webdriver%20bidi%20network)

### Chrome/Chromium (Chromedriver)
- **Status**: ⚠️ Partial support
- **Supported Commands**: Some network events and basic intercepts
- **Unsupported**: Most advanced network manipulation commands
- **Version**: Check latest Chromedriver release notes for updates

### Safari (Safaridriver)
- **Status**: ❌ No network module support
- **Network Commands**: WebDriver BiDi support is still experimental

## Workarounds

Until network module support is implemented in WebDriver implementations:

1. **Use CDP (Chrome DevTools Protocol)** for Chrome/Chromium
2. **Use WebDriver Classic** for basic network functionality
3. **Wait for driver updates** - network module support is actively being developed

## Testing the Demos

The network demos in this codebase are designed to:
- ✅ Demonstrate correct parameter structures according to WebDriver BiDi spec
- ✅ Compile successfully with proper type safety
- ✅ Work when WebDriver implementations add support
- ⚠️ Currently fail with "unknown command" errors on most drivers

## Checking Support

Use the `NetworkSupportTest.hs` demo to check which commands are supported in your WebDriver implementation.