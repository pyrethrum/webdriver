# Screencast Recording Capability Implementation

## Summary

Implemented the Screencast Recording Capability from the WebDriver BiDi specification (June 29, 2026) as described in [bidi-spec-diff-2026-01-09-26-06-29.md](webdriver-precore/src/WebDriverPreCore/BiDi/bidi-spec-diff-2026-01-09-26-06-29.md).

## Changes Made

### 1. Command Parameter Types (BrowsingContext.hs)

Added new command parameter types:

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

## Implementation Notes

- All types follow the library's established patterns using:
  - Generic deriving for `ToJSON`/`FromJSON` instances
  - `toJSONOmitNothing` for optional fields
  - Newtype wrappers with `deriving newtype` for simple type aliases
  - Record syntax with `MkConstructorName` pattern

- The implementation is complete for the library API but **no tests have been added yet** as requested

- Build Status: ✅ Compiles successfully with only harmless redundant import warnings

## Next Steps

When implementing tests:
1. Create demo in `test/BiDi/Demos/BrowsingContextDemos.hs`
2. Test starting a screencast with various parameters
3. Test stopping a screencast and verifying the result
4. Test error handling for invalid screencast IDs
5. Verify file paths are correctly returned
