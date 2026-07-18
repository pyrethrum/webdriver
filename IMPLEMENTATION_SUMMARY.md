# WebDriver BiDi Specification Updates - June 29, 2026

## Summary

Implemented new features from the WebDriver BiDi specification (June 29, 2026) as described in [bidi-spec-diff-2026-01-09-26-06-29.md](webdriver-precore/src/WebDriverPreCore/BiDi/bidi-spec-diff-2026-01-09-26-06-29.md):

1. **Content Security Policy Bypass** - Allow tests to bypass CSP restrictions
2. **Screencast Recording Capability** - Record browser sessions as video files
3. **Scrollbar Type Emulation** - Override scrollbar appearance for consistent cross-platform testing

## Changes Made

---

## Content Security Policy Bypass Implementation

### 1. Command Parameter Types (BrowsingContext.hs)

Added new command parameter type:

- **`SetBypassCSP`**: Parameters for bypassing Content Security Policy
  - `bypass`: Either `Just True` (enable bypass) or `Nothing` (disable bypass / send null)
  - `contexts`: Optional array of specific browsing contexts to affect
  - `userContexts`: Optional array of user contexts to affect

**Custom ToJSON Instance**: Implements special handling for the `bypass` field to correctly encode:
  - `Just True` → `"bypass": true`
  - `Nothing` → `"bypass": null`
  - `Just False` → Error (not allowed by spec)

### 2. Command Enum (Command.hs)

Added new command constructor to the `KnownCommand` enum:
- `BrowsingContextSetBypassCSP`

Updated:
- `FromJSON` instance to parse `"browsingContext.setBypassCSP"`
- `knownCommandToText` function to convert command constructor to string representation

### 3. API Functions (API.hs)

Added new API function:

```haskell
browsingContextSetBypassCSP :: SetBypassCSP -> Command ()
```

Follows the established pattern:
- Returns `Command ()` (EmptyResult)
- Include specification reference to the W3C spec
- Added to spec: 29 June 2026 - Working Draft
- Spec URL: `browsingContext.setBypassCSP`

### 4. Test Actions (test/BiDi/Actions.hs)

Added action function to the test suite:
- `browsingContextSetBypassCSP :: SetBypassCSP -> IO ()`

### Specification Reference

- **Spec version**: W3C WebDriver BiDi Working Draft, 29 June 2026
- **Command**: [browsingContext.setBypassCSP](https://www.w3.org/TR/2026/WD-webdriver-bidi-20260629/#command-browsingContext-setBypassCSP)

### Use Cases

1. **Test Script Injection**: Inject test scripts into pages with strict CSP
2. **Browser Extension Testing**: Enable extension interactions during testing
3. **External Resources**: Load test fixtures from external domains
4. **CSP Debugging**: Debug CSP-related issues without modifying the application

### Security Considerations

This command should only be used in automated testing environments. Bypassing CSP in production could expose applications to XSS and other security vulnerabilities.

--# Specification Reference

- **Spec version**: W3C WebDriver BiDi Working Draft, 29 June 2026
- **Commands**: 
  - [browsingContext.startScreencast](https://www.w3.org/TR/2026/WD-webdriver-bidi-20260629/#command-browsingContext-startScreencast)
  - [browsingContext.stopScreencast](https://www.w3.org/TR/2026/WD-webdriver-bidi-20260629/#command-browsingContext-stopScreencast)

#
- **`StartScreencast`**: Parameters for starting a screencast recording
  - `context`: The browsing context to record
  - `mimeType`: Optional MIME type for the recording
  - `video`: Optional `MediaTrackConstraints` for video settings
  - `audio`: Optional boolean for audio capture (default: false)

- **`StopScreencast`**: Parameters for stopping a screencast
  - `screencast`: The screencast identifier to stop

- **`MediaTrackConstraints`**: Video recording constraints
  - `width`: Optional width in pixels
  - `height`: Optional height in pixels  
  - `frameRate`: Optional frame rate

- **`Screencast`**: Newtype wrapper for screencast identifier (Text)

### 2. Result Types (BrowsingContext.hs)

Added result types:

- **`StartScreencastResult`**: Result from starting a screencast
  - `screencast`: The screencast identifier
  - `path`: File path where recording is being saved

- **`StopScreencastResult`**: Result from stopping a screencast
  - `path`: Final file path of the recording
  - `error`: Optional error message if the recording encountered issues

### 3. Command Enum (Command.hs)

Added two new command constructors to the `KnownCommand` enum:
- `BrowsingContextStartScreencast`
- `BrowsingContextStopScreencast`

Updated:
- `FromJSON` instance to parse `"browsingContext.startScreencast"` and `"browsingContext.stopScreencast"`
- `knownCommandToText` function to convert command constructors to their string representations

### 4. API Functions (API.hs)

Added two new API functions:

```haskell
browsingContextStartScreencast :: StartScreencast -> Command StartScreencastResult
browsingContextStopScreencast :: StopScreencast -> Command StopScreencastResult
```

Both functions follow the established pattern:
- Include specification references to the W3C spec
- Added to spec: 29 June 2026 - Working Draft
- Spec URL: `browsingContext.startScreencast` and `browsingContext.stopScreencast`

### 5. Test Actions (test/BiDi/Actions.hs)

Added action functions to the test suite:
- `browsingContextStartScreencast :: StartScreencast -> IO StartScreencastResult`
- `browsingContextStopScreencast :: StopScreencast -> IO StopScreencastResult`

## Specification Reference

- **Spec version**: W3C WebDriver BiDi Working Draft, 29 June 2026
- **Commands**: 
  - [browsingContext.startScreencast](https://www.w3.org/TR/2026/WD-webdriver-bidi-20260629/#command-browsingContext-startScreencast)
  - [browsingContext.stopScreencast](https://www.w3.org/TR/2026/WD-webdriver-bidi-20260629/#command-browsingContext-stopScreencast)

## Use Cases

1. **Test Recording**: Record browser sessions for debugging failed tests
2. **Visual Regression**: Generate video artifacts for visual regression testing
3. **Documentation**: Create automated documentation videos
4. **User Interaction Analysis**: Capture user interaction flows

---

## Scrollbar Type Emulation Implementation

### 1. Type Definitions (Emulation.hs)

Added new sum type and command parameter type:

- **`ScrollbarType`**: Enumeration for scrollbar appearance
  - `Classic` - Always-visible scrollbars (typical on Windows/Linux) → encodes as `"classic"`
  - `Overlay` - Auto-hiding scrollbars (typical on macOS) → encodes as `"overlay"`
  - `PlatformDefault` - Restore platform default behavior → encodes as `null`

- **`SetScrollbarTypeOverride`**: Parameters for overriding scrollbar type
  - `scrollbarType`: The scrollbar type to emulate
  - `contexts`: Optional array of specific browsing contexts to affect
  - `userContexts`: Optional array of user contexts to affect

**Custom ToJSON Instance**: Implements special handling for the `scrollbarType` field:
  - `Classic` → `"scrollbarType": "classic"`
  - `Overlay` → `"scrollbarType": "overlay"`
  - `PlatformDefault` → `"scrollbarType": null`

### 2. Command Enum (Command.hs)

Added new command constructor to the `KnownCommand` enum:
- `EmulationSetScrollbarTypeOverride`

Updated:
- `FromJSON` instance to parse `"emulation.setScrollbarTypeOverride"`
- `knownCommandToText` function to convert command constructor to string representation

### 3. API Functions (API.hs)

Added new API function:

```haskell
emulationSetScrollbarTypeOverride :: SetScrollbarTypeOverride -> Command ()
```

Follows the established pattern:
- Returns `Command ()` (EmptyResult)
- Include specification reference to the W3C spec
- Added to spec: 29 June 2026 - Working Draft
- Spec URL: `emulation.setScrollbarTypeOverride`

### 4. Test Actions (test/BiDi/Actions.hs)

Added action function to the test suite:
- `emulationSetScrollbarTypeOverride :: SetScrollbarTypeOverride -> IO ()`

### Specification Reference

- **Spec version**: W3C WebDriver BiDi Working Draft, 29 June 2026
- **Command**: [emulation.setScrollbarTypeOverride](https://www.w3.org/TR/2026/WD-webdriver-bidi-20260629/#command-emulation-setScrollbarTypeOverride)
- **Result**: EmptyResult

### Use Cases

1. **Cross-Platform Testing**: Test layouts consistently across different operating systems without physical hardware
2. **Responsive Design**: Validate layouts that depend on viewport width calculations
3. **Screenshot Consistency**: Ensure consistent screenshot capture across test environments
4. **Scrollbar Width Testing**: Test layouts that must accommodate or exclude scrollbar width

### Implementation Details

The `ScrollbarType` sum type provides a type-safe way to specify scrollbar appearance:
- Platform-specific scrollbar types are explicitly named for clarity
- The `PlatformDefault` constructor allows restoration of default behavior
- JSON encoding matches the spec exactly with `null` for default and strings for specific types

---

## General Implementation Notes

- All types follow the library's established patterns using:
  - Generic deriving for `ToJSON`/`FromJSON` instances
  - `toJSONOmitNothing` for optional fields
  - Newtype wrappers with `deriving newtype` for simple type aliases
  - Record syntax with `MkConstructorName` pattern

- The implementation is complete for the library API but **no tests have been added yet** as requested

- Build Status: ✅ Compiles successfully with only harmless redundant import warnings

## Next Steps for these features:

### For Content Security Policy Bypass:
1. Create demo in `test/BiDi/Demos/BrowsingContextDemos.hs`
2. Test enabling bypass with `Just True`
3. Test disabling bypass with `Nothing` (null)
4. Test with specific contexts
5. Test with user contexts
6. Verify CSP headers are ignored when bypass is enabled
7. Verify CSP enforcement is restored when bypass is disabled

### For Screencast Recording

When implementing tests:
1. Create demo in `test/BiDi/Demos/BrowsingContextDemos.hs`
2. Test starting a screencast with various parameters
3. Test stopping a screencast and verifying the result
4. Test error handling for invalid screencast IDs
5. Verify file paths are correctly returned

### For Scrollbar Type Emulation

When implementing tests:
1. Create demo in `test/BiDi/Demos/EmulationDemos.hs`
2. Test setting classic scrollbars
3. Test setting overlay scrollbars
4. Test restoring platform default with `PlatformDefault`
5. Test with specific contexts
6. Test with user contexts
7. Verify layout changes when switching between scrollbar types
