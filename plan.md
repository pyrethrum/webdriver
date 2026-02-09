# WebDriver Reorganization Plan

## Overview
Reorganize webdriver-precore into multiple internal libraries for modular development.

## Current State
- webdriver-precore library with HTTP and BiDi API definitions
- utils-internal library
- test-suite with demos and unit tests
- test-server

## Target Structure

### Existing Libraries (keep)
- [x] library (webdriver-precore) - Core types and API definitions
- [x] utils-internal - AesonUtils, Utils
- [ ] test-suite test - Will be slimmed down (unit tests only)
- [x] test-server

### New Internal Libraries (to add to webdriver-precore.cabal)

| Library | Purpose | Dependencies | Status |
|---------|---------|--------------|--------|
| webdriver-precore-extended | Re-exports precore + extended functions | webdriver-precore | ✅ Done |
| webdriver-precore-exception | Exception handling and conversion | base, text, aeson | ✅ Done |
| webdriver-precore-http-runner-base | HTTP runner with JSON interface (no precore types) | base, text, unliftio, aeson, req | ✅ Done |
| webdriver-precore-http-runner | Typed HTTP runner | webdriver-precore, http-runner-base, exception | ✅ Done |
| webdriver-precore-bidi-runner-base | BiDi runner with JSON interface (no precore types) | base, text, unliftio, aeson, websockets | ✅ Done |
| webdriver-precore-bidi-runner | Typed BiDi runner | webdriver-precore, bidi-runner-base, exception | ✅ Done |
| webdriver-precore-driver-control | Driver lifecycle management | typed-process, path, path-io | ✅ Placeholder |
| webdriver-precore-test-resources | Shared test resources | base, text, file-embed | ✅ Done |
| webdriver-effectful | Effectful wrapper | effectful, webdriver-precore-extended | ✅ Placeholder |
| webdriver-rio-poc | RIO wrapper | rio, webdriver-precore-extended | ✅ Placeholder |
| webdriver-bluefin-poc | Bluefin wrapper | bluefin, webdriver-precore-extended | ✅ Placeholder |

### New Test Suites
Each library will have a corresponding test suite with:
- Test.hs module
- Dependencies: tasty, tasty-hunit, falsify, utils-internal

## Execution Order

### Phase 1: Foundation Libraries
1. [x] webdriver-precore-exception
   - Extracted exception types/functions from WebDriverPreCore.Error
   - Independent of precore-specific types

2. [x] webdriver-precore-test-resources
   - Created library to serve static test files using file-embed
   - Ready to share test resources across test suites

### Phase 2: HTTP Runner Stack
3. [x] webdriver-precore-http-runner-base
   - Created HttpResponse type
   - Created JSON-based runner interface
   - No dependency on webdriver-precore types (uses Value instead of Command)

4. [x] webdriver-precore-http-runner
   - Thin wrapper adding webdriver-precore types
   - Ready to migrate HTTP.Runner from test suite

### Phase 3: BiDi Runner Stack
5. [x] webdriver-precore-bidi-runner-base
   - Copied BiDi socket implementation
   - Decoupled from webdriver-precore types
   - Duplicated simple types like JSUInt

6. [x] webdriver-precore-bidi-runner
   - Thin wrapper adding webdriver-precore types
   - Ready to migrate BiDi.Runner from test suite

### Phase 4: Extended and Control Libraries
7. [x] webdriver-precore-extended
   - Re-exports webdriver-precore HTTP.API module
   - Separate modules for HTTP and BiDi due to naming conflicts

8. [x] webdriver-precore-driver-control
   - Placeholder library with dependencies only

### Phase 5: Effect System Wrappers
9. [x] webdriver-effectful - Placeholder library with effectful dependencies
10. [x] webdriver-rio-poc - Placeholder library with RIO dependencies
11. [x] webdriver-bluefin-poc - Placeholder library with bluefin dependencies

### Phase 6: Test Migration
12. [x] Create test suites for all new libraries
    - test-exception (4 tests, passing)
    - test-http-runner-base (2 tests, passing)
    - test-http-runner (1 test, passing)
    - test-bidi-runner-base (2 tests, passing)
    - test-bidi-runner (2 tests, passing)
13. [ ] Migrate HTTP demos to webdriver-precore-http-runner test (FUTURE)
14. [ ] Migrate BiDi demos to webdriver-precore-bidi-runner test (FUTURE)
15. [ ] Slim down original test suite to unit/property tests only (FUTURE)
16. [ ] Remove unused test files from webdriver-precore/test (FUTURE)

Note: Demo migration requires refactoring shared modules (Config, ConfigLoader, TestData, IOUtils, TestServerAPI, CapabilitiesBuilder, Logger) which have dependencies on dhall configuration. This is deferred for a future phase.

### Phase 7: Cleanup
17. [ ] Update tasks.json
18. [ ] Update cabal.project if needed

## Key Design Decisions

### Test Resources Approach
Using an internal library `webdriver-precore-test-resources` that:
- Contains static files as embedded resources (using file-embed) OR
- Provides file paths via cabal data-files
- Shared by all test suites

### Type Decoupling Strategy
- Runner base libraries use Aeson Value for commands
- Exception library handles all error parsing
- Typed runner libraries add the type layer

## Design Decisions (Confirmed)

1. **Test Resources**: Use `file-embed` in a separate library to embed files at compile time
2. **Exception Library Scope**: Include both exception types AND helper functions
3. **JSUInt Duplication**: Duplicate simple types in base libraries, coerce in typed runners
4. **Directory Structure**: All libraries in one cabal file under webdriver-precore with separate src directories

---
Last Updated: Phase 6 Partial - Test Suites Added

## Implementation Summary

### Files Created (Libraries)
- `src-exception/WebDriverPreCore/Exception.hs`
- `src-test-resources/WebDriverPreCore/TestResources.hs`
- `src-http-runner-base/WebDriverPreCore/HttpRunnerBase.hs`
- `src-http-runner-base/WebDriverPreCore/HttpRunnerBase/HttpResponse.hs`
- `src-http-runner/WebDriverPreCore/HttpRunner.hs`
- `src-bidi-runner-base/WebDriverPreCore/BiDiRunnerBase.hs`
- `src-bidi-runner-base/WebDriverPreCore/BiDiRunnerBase/Types.hs`
- `src-bidi-runner-base/WebDriverPreCore/BiDiRunnerBase/Response.hs`
- `src-bidi-runner-base/WebDriverPreCore/BiDiRunnerBase/Socket.hs`
- `src-bidi-runner/WebDriverPreCore/BiDiRunner.hs`
- `src-extended/WebDriverPreCore/Extended.hs`
- `src-extended/WebDriverPreCore/Extended/Base/HTTP/API.hs`
- `src-extended/WebDriverPreCore/Extended/Base/HTTP/Protocol.hs`
- `src-extended/WebDriverPreCore/Extended/Base/BiDi/API.hs`
- `src-extended/WebDriverPreCore/Extended/Base/BiDi/Protocol.hs`
- `src-driver-control/WebDriverPreCore/DriverControl.hs`
- `src-effectful/WebDriver/Effectful.hs`
- `src-rio-poc/WebDriver/RIO.hs`
- `src-bluefin-poc/WebDriver/Bluefin.hs`

### Files Created (Test Suites)
- `test-exception/Test.hs` (4 tests)
- `test-http-runner-base/Test.hs` (2 tests)
- `test-http-runner/Test.hs` (1 test)
- `test-http-runner/HttpDemo.hs` (placeholder)
- `test-bidi-runner-base/Test.hs` (2 tests)
- `test-bidi-runner/Test.hs` (2 tests)
- `test-bidi-runner/BiDiDemo.hs` (placeholder)

### cabal file updated
- Added all 11 new internal library definitions
- Added 5 new test suite definitions
- Fixed version constraints for bluefin (>=0.2 && <0.3) and effectful-core (>=2.3 && <2.7)
