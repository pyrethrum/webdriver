# webdriver-test-0.2.0.0 (2026-01-17)

## New Features

### BiDi Test Suite

Complete test coverage for WebDriver BiDi protocol:

- **Demos**: Comprehensive demos for all BiDi modules in `BiDi/Demos/` including browsing context, script evaluation, network interception, input actions, storage, emulation, and event subscriptions
- **Connection handling**: WebSocket connection utilities in `BiDi/BiDiSocket.hs` and `BiDi/BiDiUrl.hs`
- **Test actions**: Reusable BiDi test actions in `BiDi/Actions.hs`
- **Runner**: BiDi test runner with session management in `BiDi/Runner.hs`
- **Error demos**: BiDi error handling examples in `BiDi/ErrorDemo.hs`

### HTTP Test Reorganization

HTTP tests moved to `HTTP/` directory (previously in root):

- `HttpDemo.hs`: Main HTTP protocol demos
- `Actions.hs`: Reusable HTTP test actions
- `HttpRunner.hs`: HTTP test runner
- `ErrorDemo.hs`: HTTP error handling examples
- `FallbackDemo.hs`: Protocol fallback examples
- `HttpActionsDeprecated.hs`, `HttpRunnerDeprecated.hs`: For testing deprecated SpecDefinition module

### Configuration Management

New configuration:

- Configuration loader in `ConfigLoader.hs` supporting Dhall config files
- Config directory at `.config/` (user config file gitignored)
- Debug configuration via `DebugConfig.hs` module
- Runtime constants in `RuntimeConst.hs` and `Const.hs`
- For details regarding configuration details see [Configuration set up](README.md#4-configuration-set)

### Test Infrastructure

- **Shared utilities**: Common test utilities in `IOUtils.hs`, `TestData.hs`, `Driver.hs`
- **Logging**: Test logging framework in `Logger.hs`
- **Test server**: Local test server with API in `TestServerAPI.hs`
- **Test files**: HTML and JavaScript test files in `TestFiles/`
- Coverage tests updated: `ApiCoverageTest.hs`, `ErrorCoverageTest.hs`, `JSONParsingTest.hs`

## Breaking Changes

### Module Renames

Tests updated to reflect library restructuring:

- Imports changed from `WebDriverPreCore.SpecDefinition` to `WebDriverPreCore.HTTP.API`
- Type updates: `W3Spec` → `Command`, `SessionId` → `Session`
- Error type updates: `WebDriverErrorType` → `ErrorType`

### Config Changes

- Removed profile-based config (old `.profile/` directory)
- Configuration now via Dhall files in `.config/` directory
- `cabal.project.local` used for build-time test flags

## Examples

The test directory serves as reference implementation for library users. See:

- `HTTP/` directory for HTTP protocol usage patterns
- `BiDi/` directory for BiDi protocol usage patterns
- README.md for setup and usage instructions


# webdriver-examples-0.1.1.0 (2025-06-29)

## Examples update only (no hackage update)
- Add dhall config
- Update `ReadMe`
  - better instructions for handling Firefox profile issues
  - instructions on using new dhall config
- Examples are modified in line with unreleased changes to `WebDriverPreCore.Http.SpecDefinition`. These include:
  - `newSession` to return the whole response, not just the `sessionId`
  - refactoring in preparation for including webdriver BiDi

# webdriver-examples-0.1.0.2 (2025-05-17)

Add Firefox headless config option

# webdriver-examples-0.1.0.1 (2025-04-28)

No change - examples that work with `webdriver-precore-0.1.0.1`.

# webdriver-examples-0.1.0.0 (2025-04-21)

No change - examples that work with `webdriver-precore-0.1.0.0`.

# webdriver-examples-0.0.0.4 (2025-04-21)

No change - examples that work with `webdriver-precore-0.0.0.4`.

# webdriver-examples-0.0.0.3 (2025-04-21)

No change - examples that work with `webdriver-precore-0.0.0.3`.

# webdriver-examples-0.0.0.2 (2025-04-21)

Examples that work with `webdriver-precore-0.0.0.2`.

See [README](README.md) for details