# webdriver-precore-0.2.0.0 (2026-01-17)

## Breaking Changes

### Module Restructure

The library has been reorganized to support both HTTP and BiDi protocols:

- `WebDriverPreCore.SpecDefinition` deleted. Use `WebDriverPreCore.HTTP.API` or the deprecated `WebDriverPreCore.HTTP.SpecDefinition`
- `WebDriverPreCore.Capabilities` → `WebDriverPreCore.HTTP.Capabilities` 
- `WebDriverPreCore.HttpResponse` → `WebDriverPreCore.HTTP.HttpResponse`

Main module exports updated to reflect new structure. Import from `WebDriverPreCore.HTTP.*` for HTTP protocol or `WebDriverPreCore.BiDi.*` for BiDi protocol.

### Type Renames

- `W3Spec` → `Command` (in `WebDriverPreCore.HTTP.Protocol`)
- `SessionId` → `Session` (constructor: `MkSession`)
- `WebDriverErrorType` → `ErrorType`
- `ScriptTimeoutError` → `ScriptTimeout`
- Error functions renamed:
  - `errorCodeToErrorType` → `toErrorType`
  - `errorTypeToErrorCode` → `toErrorCode`
  - `parseWebDriverError` → `parseWebDriverException`
  - `parseWebDriverErrorType` → `parseErrorType`

### HTTP API Changes

**Status endpoint**: Return type changed from `DriverStatus` to `Status`. The `Status` type correctly implements the spec:
- `ready` field indicates if server accepts commands
- `message` field provides additional info
- Previous implementation incorrectly returned `ready` as status

**New session**: Now returns `SessionResponse` containing:
- `sessionId :: Session` (the session ID)
- `webSocketUrl :: Maybe Text` (for BiDi connections)
- `capabilities :: Capabilities` (matched capabilities)
- `extensions :: Maybe (Map Text Value)` (extension-specific data)

**Response parsing**: The `Command` type in the new API no longer exposes a `parser` field. Extract the response body value and call `parseJSON` on it. For migration examples, see [HTTP test runners](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test/HTTP).

### Migration Guide

1. Update imports:
   ```haskell
   -- Old
   import WebDriverPreCore.SpecDefinition
   
   -- New
   import WebDriverPreCore.HTTP.API
   ```

2. Update types in code:
   ```haskell
   -- Old
   spec :: W3Spec a
   sessionId :: SessionId
   errType :: WebDriverErrorType
   
   -- New  
   cmd :: Command a
   session :: Session
   errType :: ErrorType
   ```

3. Handle new session response:
   ```haskell
   -- Old: newSession returned SessionId
   sessionId <- runCommand $ newSession caps
   
   -- New: newSession returns SessionResponse
   sessionResp <- runCommand $ newSession caps
   let session = sessionResp.sessionId       -- Session type
       wsUrl = sessionResp.webSocketUrl      -- for BiDi connection
       caps = sessionResp.capabilities       -- matched capabilities
   ```

4. Update error handling:
   ```haskell
   -- Old
   errorCodeToErrorType code
   
   -- New
   toErrorType code
   ```

## Deprecations

`WebDriverPreCore.HTTP.SpecDefinition` module deprecated (removal ~2027-02-01). This module provides backward compatibility with the old `SpecDefinition` API using the deprecated name `HttpSpec` for the spec type. Migrate to `WebDriverPreCore.HTTP.API` which uses the `Command` type.

## New Features

### BiDi Protocol Support

Complete implementation of W3C WebDriver BiDi protocol, including:

- **Session management**: Connection setup, capability negotiation
- **Browsing contexts**: Navigation, context management, tree traversal
- **Script evaluation**: JavaScript execution, realm management, channel messaging
- **Network**: Request interception, authentication, response modification
- **Input**: Actions for keyboard, mouse, wheel
- **Storage**: Cookie and storage partition management
- **Events**: Subscriptions for logs, network events, browsing context lifecycle, script realm events
- **Emulation**: User agent, viewport configuration
- **Browser**: Process management, user context handling
- **WebExtensions**: Install and uninstall browser extensions (experimental)

See `WebDriverPreCore.BiDi.API` for commands and `WebDriverPreCore.BiDi.Protocol` for types.

### Error Handling Improvements

- Added 17 BiDi-specific error types
- `ErrorType` now derives `FromJSON` for direct parsing
- Improved error-to-code conversion with proper camelCase handling

## Bug Fixes

- Status endpoint implementation corrected to match W3C spec
- Fixed session status data structure (was incorrectly returning `ready` as the status value)

## Examples

See [test directory](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test) for HTTP and BiDi usage examples

# webdriver-precore-0.1.0.2 (2025-05-17)

Fix Hackage build failure (ghc-9.8.4)

# webdriver-precore-0.1.0.1 (2025-04-28)

README update - Stackage badge

# webdriver-precore-0.1.0.0 (2025-04-27)

Downgrade `cabal-version:` from  `3.12` => `3.8` for Stackage compatibility

# webdriver-precore-0.0.0.4 (2025-04-21)

Fix another typo (bad repo examples link)

# webdriver-precore-0.0.0.3 (2025-04-21)

A few documentation typos and bump version of Vector dependencies

# webdriver-precore-0.0.0.2 (2025-04-21)

The initial release of this library.

See [README](README.md) for details