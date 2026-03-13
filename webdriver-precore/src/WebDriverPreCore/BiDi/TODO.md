# TODO

## Locators

## fix this file
- [ ] harmonise isolate logging
- [ ] resplit the libraries 
  - [ ] work out a way to work with as one
  - [ ]  

## deferred
 - [ ] edge driver
 - [ ] chrome - CI
 - [ ] test with driver not running
 - [ ] rearrange docker - move hls install to just before dev-container
 - [ ] ci see if can compile test server once

## update according to spec changes 
 - [ ] [webdriver-HTTP-20251028-20260206.md](../../../../docs/webdriver-HTTP-20251028-20260206.md)
 - [ ] 
## address critical review - effectful

What worked well:

withSeqEffToIO bridging — correctly used in App.hs for resource management. The pattern withSeqEffToIO $ \runInIO -> finally (runInIO action) cleanup is the canonical Effectful approach for brackets that span IO/Eff boundaries. ✓

Dynamic effect dispatch — justified. The GADT encoding (WebDriverHttp, WebDriverBiDi) means alternative interpreters (test doubles, mock sessions) can replace the IO interpretations without changing call-site code. ✓

Logger as Static WithSideEffects, LogPause as Static NoSideEffects — semantically correct. LogPause holds a pure Timeout value (no IO), so it doesn't need IOE :> es to install via evalStaticRep. pause carries its own IOE :> es where needed. ✓

withLogger + withHttpSession + withLogPause stacking — reads cleanly in the test harness, clearly reflecting the effect stack in the types. ✓

STM via liftIO — pragmatic. The comment noting the future Effectful.Concurrent.STM enhancement is honest. No semantic difference for tests.

Areas to note:

mkSessionRunner leaks from HTTP/Core — it's an internal detail exposed to allow App.hs to use it. Alternatively it could live in App.hs directly (reducing the API surface of HTTP/Core), but the current approach avoids circular imports.

Logger :> es required by withHttpSession/withBiDiSession — only needed when driverLogging == True. This is a mild over-constraint: users must have Logger in scope even when they don't want logging. The Bluefin equivalent passes the log function directly. However, requiring the effect is more idiomatic for the Effectful style and makes the dependency visible.

Severity (..) constructors not re-exported from the top-level module — due to a name conflict with Debug/Info/Error from Extended.Capabilities. Users needing constructors must import WebDriver.Effectful.Core (Severity (..)) separately. Acceptable for a POC.

effectful >= 2.3 && < 2.7 cabal constraint — the code requires 2.6.x API (Dispatch(..), SideEffects(..) from the redesign). Consider tightening to >= 2.6 && < 2.7 to prevent resolving to an incompatible older version.

BP.ScriptEvaluate for KnownCommand — needed one qualified import (BP) to avoid the GADT/enum naming collision. This is minor but signals the naming tension between the WebDriver protocol domain model and the GADT constructors.

The overall structure is sound. The Effectful POC achieves its purpose: a clean algebraic effect + interpreter architecture that mirrors the Bluefin approach while using effectful's send/interpret_ idiom instead of Bluefin's handle-passing style.

Claude Sonnet 4.6 • 1x


## final restructure

- [ ] [update readme in similar style to](https://github.com/nikita-volkov/hasql)
- [ ] restore legacy test / code for HTTP
- [ ] integration test libraries
- [ ] get off [9.12.3](https://www.reddit.com/r/haskell/comments/1q78zgc/critical_code_generation_bug_with_ghc_9123/)
  - [ ] no wonder it wasn't in ghcup

### interim
- use shared libraries

### immediate post restructure

 [x] all tests working
 [x] read unliftIO
 [x] read RIO
 [ ] reexport base test
 [ ] all patterns

### demo capabilities all

-- challenge 
  -- basic bidi
  -- basic http
  -- bidi events
  -- bidi and http mixed on the same instance
  -- hooks to start runner and close connection
     -- may cascade in bidirunner
  -- concurrency 
    - mock create user rest call
    - 2 browsers (chrome and firefox)

### effectful only

-- non-compliant unparsable response from capabilities 


 ### socket server

[ ] check [this PR](https://github.com/jaspervdj/websockets/pull/259/changes)
[ ] test for server close - goes back to main thread


---

### [ ] Add http documentation of read only properties which are ignored if sent (HTTP)

User: confused about setWindowRect property in http and bidi spec (on the web) clearly setWindowRect is only a return value in bidi from thew spec but is it return only in http?  what should be expected to be returned by newSession in the http endpoint?

GitHub Copilot: Based on the WebDriver specifications, here's the clarification about `setWindowRect`:

## HTTP (W3C WebDriver Spec)

In the HTTP WebDriver spec, `setWindowRect` is a **capability** that is returned in the response to the New Session command. It indicates whether the browser supports the Set Window Rect command.

The New Session HTTP endpoint should return in the response body:
- `capabilities` object containing:
  - `setWindowRect` (boolean) - `true` if the endpoint supports Set Window Rect, `false` otherwise
  - Other standard and extension capabilities

This is a **read-only capability** - the server determines its value based on whether the browser/driver supports window positioning and resizing. For example:
- Desktop browsers typically return `true`
- Mobile browsers typically return `false` (since they don't support arbitrary window sizing)

## WebDriver BiDi Spec

In BiDi, `setWindowRect` is **only a return value** in the `session.new` command response. It serves the same purpose as in HTTP - indicating whether the session supports window rect operations.

## Summary

- **HTTP**: `setWindowRect` is returned in the New Session response as a capability
- **BiDi**: `setWindowRect` is returned in the `session.new` response
- **Both**: It's a boolean indicating support for window rect operations, not a settable capability
- **Both**: It's determined by the server/browser, not requested by the client

The confusion might arise because some capabilities can be requested (like `browserName`), but `setWindowRect` is always server-determined based on the platform's capabilities.

User: So strictly speaking, capabilites response type and  capabilites request type should be different in http? What other differences might there be in return vs request capabilites?

GitHub Copilot: Yes, strictly speaking the capabilities **request** and **response** types should be different in HTTP WebDriver. Here are the key differences:

## Capabilities That Are Response-Only

1. **`setWindowRect`** - Server indicates if window rect operations are supported
2. **`browserVersion`** - Actual browser version (client may request `browserVersion` as a match constraint, but server returns the actual version)
3. **`platformName`** - Actual platform (client may request it, but server returns the actual value)
4. **Extension capabilities** - Server may add vendor-specific capabilities not in the request

## Capabilities With Different Semantics

### Request (Capabilities Matching)
The request uses:
- **`alwaysMatch`** - Capabilities that MUST be satisfied
- **`firstMatch`** - List of capability sets, at least one MUST be satisfied

Example request structure:
```json
{
  "capabilities": {
    "alwaysMatch": { "browserName": "firefox" },
    "firstMatch": [
      { "platformName": "linux" },
      { "platformName": "mac" }
    ]
  }
}
```

### Response (Actual Capabilities)
The response returns:
- A single **`capabilities`** object with the actual/merged capabilities
- Includes server-determined values like `setWindowRect`
- Contains the actual matched values

Example response structure:
```json
{
  "value": {
    "sessionId": "...",
    "capabilities": {
      "browserName": "firefox",
      "browserVersion": "120.0",
      "platformName": "linux",
      "setWindowRect": true
    }
  }
}
```

## In Your Haskell Code

You should have separate types:

```haskell
-- Request type
data NewSessionRequest = NewSessionRequest
  { alwaysMatch :: Maybe Capabilities
  , firstMatch :: Maybe [Capabilities]
  }

-- Response type  
data NewSessionResponse = NewSessionResponse
  { sessionId :: Text
  , capabilities :: Capabilities  -- Actual merged capabilities
  }

-- Capabilities might have fields that are:
data Capabilities = Capabilities
  { browserName :: Maybe Text
  , browserVersion :: Maybe Text  -- Response only (in practice)
  , platformName :: Maybe Text
  , setWindowRect :: Maybe Bool   -- Response only
  , -- ...other fields
  }
```

The `Capabilities` type itself might be shared, but the fields have different meanings depending on context (request = desired, response = actual).

User: I rpresume they are ignored if sent (return only caps)

GitHub Copilot: Yes, exactly! Return-only capabilities like `setWindowRect` should be **ignored if sent in the request**. The server determines these values based on its own capabilities, regardless of what the client sends.

According to the W3C WebDriver spec:
- Unknown or unsupported capabilities in the request should be ignored (not cause an error)
- Return-only capabilities like `setWindowRect` are computed by the server
- The server returns its actual capabilities, not what was requested

So if a client sends:
```json
{
  "capabilities": {
    "alwaysMatch": {
      "browserName": "firefox",
      "setWindowRect": false  // This is ignored
    }
  }
}
```

The server will:
1. Match `browserName`
2. Ignore `setWindowRect` in the request
3. Return its actual `setWindowRect` value (e.g., `true` for desktop Firefox)

This means your Haskell types can handle this gracefully - you can accept these fields in the request type for flexibility, but document that they're ignored, or you can have stricter request types that don't allow them at all.

The practical approach is usually to share the `Capabilities` type but document which fields are meaningful in requests vs responses:

```haskell
data Capabilities = Capabilities
  { browserName :: Maybe Text
  , browserVersion :: Maybe Text    -- Ignored in requests, set by server
  , platformName :: Maybe Text
  , setWindowRect :: Maybe Bool     -- Ignored in requests, set by server
  -- ...
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

This approach is simpler and matches how most WebDriver implementations work.


