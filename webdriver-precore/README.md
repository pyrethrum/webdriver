# webdriver-precore

![build](https://github.com/pyrethrum/webdriver/actions/workflows/cicd.yaml/badge.svg?branch=main)
[![Hackage](https://img.shields.io/hackage/v/webdriver-precore.svg)](https://hackage.haskell.org/package/webdriver-precore)
[![Stackage Nightly](http://stackage.org/package/webdriver-precore/badge/nightly)](http://stackage.org/nightly/package/webdriver-precore)


<!-- 
Adapt and move when ready
![CI](https://github.com/commercialhaskell/path/workflows/CI/badge.svg?branch=master)
[![Stackage LTS](http://stackage.org/package/path/badge/lts)](http://stackage.org/lts/package/path)
[![Stackage Nightly](http://stackage.org/package/path/badge/nightly)](http://stackage.org/nightly/package/path) 
-->

- [webdriver-precore](#webdriver-precore)
  - [What is This Library?](#what-is-this-library)
  - [Why This Library?](#why-this-library)
    - [Core Principles](#core-principles)
    - [Library Non-Goals](#library-non-goals)
    - [Acknowledgements](#acknowledgements)
- [Minimal Example](#minimal-example)
  - [1. Implementing a runner](#1-implementing-a-runner)
      - [Main Types (Used in the Runner)](#main-types-used-in-the-runner)
        - [HttpSpec](#httpspec)
        - [HttpResponse](#httpresponse)
      - [The Runner](#the-runner)
    - [1.1 Convert HttpSpec to params for req](#11-convert-httpspec-to-params-for-req)
    - [1.2 Call the WebDriver](#12-call-the-webdriver)
    - [1.3 Parse HttpResponse Using the Parser Provided in the HttpSpec](#13-parse-httpresponse-using-the-parser-provided-in-the-httpspec)
    - [2. Applying the Runner to the HttpSpec Functions](#2-applying-the-runner-to-the-httpspec-functions)
    - [3. Install a Vendor Provided WebDriver](#3-install-a-vendor-provided-webdriver)
    - [4. Launch WebDriver From the Terminal](#4-launch-webdriver-from-the-terminal)
    - [5. Drive the Browser Via the IO API](#5-drive-the-browser-via-the-io-api)

## What is This Library?

This library provides a minimal abstraction over the [WebDriver W3C Protocol endpoints](https://www.w3.org/TR/webdriver2/) without providing any implementation. It provides a description of the W3C API as a list of functions that return a [HttpSpec type](#HttpSpec). The intention is that other libraries will provide the actual implementation.

You can not use this library directly to drive a browser. If you are looking for a fully featured library to drive a browser, you may be interested in an alternative library such as [haskell-webdriver](https://hackage.haskell.org/package/webdriver), a Selenium 2 client that is actively maintained.

## Why This Library?

Several libraries provide WebDriver bindings for Haskell. However, when development on this library began, the existing options were either unmaintained, dependent on Selenium, or tightly coupled with larger "batteries included" testing frameworks.

We, the authors of this library, are building our own standalone test framework. To support browser based testing within this framework we're first creating a series of independent low-level libraries. This library is the first in that series. Our aim is to make each of our low level libraries broadly useful to others, outside its use within our own framework. 

### Core Principles
- **Direct W3C WebDriver Implementation**  
  - No Selenium dependency  
  - Full control over protocol handling  
  *Note: the [W3C WebDriver standard](https://www.w3.org/TR/webdriver2/) is an initiative driven largely by the core Selenium contributors. It provides a uniform HTTP API to drive browsers, and can be leveraged by any library, including Selenium.*

- **Minimalist Design**  
  - Boring Haskell
  - Few external dependencies  

- **Enable a Layered Architecture**  
  - Provide an unopinionated WebDriver client for use in higher level libraries

### Library Non-Goals
  
  * Any convenience or utility functions, that do not directly correspond to an endpoint on the WC3 spec. Such functions belong in downstream libraries.
  * Any transformers, effects or similar abstractions. These too belong downstream.

### Acknowledgements

This library would not have been possible without the prior work in: 

**Haskell (particularly)**:
* [haskell-webdriver](https://hackage.haskell.org/package/webdriver)
* [webdriver-w3c](https://hackage.haskell.org/package/webdriver-w3c)

**Selenium and WebDriver Standards**:

The decade+ efforts of the [Selenium](https://www.selenium.dev/) maintainers, both in forging the way with Selenium and their ongoing work in the development of the [W3C standards](https://www.w3.org/TR/webdriver2/) 


# Minimal Example

*TLDR ~ bring your own HTTP client and use it to implement the endpoints as defined in this library.*

Driving a browser using this library requires the following:
1. Implement a `runner` that takes a [HttpSpec](#HttpSpec) and makes HTTP calls an active WebDriver instance
2. Create an IO API by applying the `runner` to each of the endpoint functions in this library
3. Install the desired browser and browser driver
4. Run the driver
5. Drive the browser via the IO API

The full source can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/README.md).


## 1. Implementing a runner

The first step in writing a WebDriver implementation is to choose an HTTP library. In this example, the chosen library is [req](https://hackage.haskell.org/package/req).

Then to implement a run function requires the following:

1. Transform a [HttpSpec](#HttpSpec) to RequestParams compatible with the chosen HTTP library.
2. Make an HTTP call to WebDriver as per the RequestParams and return a simplified [HttpResponse](#httpresponse).
3. Use the parser provided by the [HttpSpec](#HttpSpec) to parse the [HttpResponse](#httpresponse) and handle any errors.

#### Main Types (Used in the Runner)

The two most important types in this library are:

##### HttpSpec

The `HttpSpec` returned by each of this library's endpoint functions. This type represents a driver endpoint.

```haskell
data HttpSpec a
  = Get
      { description :: Text,
        path :: UrlPath,
        parser :: HttpResponse -> Result a
      }
  | Post
      { description :: Text,
        path :: UrlPath,
        body :: Value,
        parser :: HttpResponse -> Result a
      }
  | PostEmpty
      { description :: Text,
        path :: UrlPath,
        parser :: HttpResponse -> Result a
      }
  | Delete
      { description :: Text,
        path :: UrlPath,
        parser :: HttpResponse -> Result a
      }
```

##### HttpResponse

`HttpResponse` is consumed by the `parser` provided by this library and needs to be constructed by the `runner`

```haskell
data HttpResponse = MkHttpResponse
  { -- | HTTP status code.
    statusCode :: Int,
    -- | HTTP status message.
    statusMessage :: Text,
    -- | Response body in JSON format.
    body :: Value
  }
```

#### The Runner

[source](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/HttpRunner.hs)


```haskell
run :: HttpSpec a -> IO a
run spec = do
  -- 1. Convert HttpSpec to params for req
  let request = mkRequest spec
  -- 2. Call WebDriver server (via req) and return a simplified HttpResponse 
  response <- callReq request
  -- 3. Apply the HttpSpec parser to the HttpResponse get result type and handle errors  
  parseResponse spec response  
```

### 1.1 Convert HttpSpec to params for req

*HttpSpec -> ReqRequestParams*

```haskell
-- A custom data type specific to req
data ReqRequestParams where
  MkRequestParams ::
    (HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) =>
    { url :: Url 'Http,
      method :: method,
      body :: body,
      port :: Int
    } ->
    ReqRequestParams

-- HttpSpec -> ReqRequestParams
-- the url and port would not normally be hard coded
mkRequest :: forall a. HttpSpec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody 4444 
  Post {body} -> MkRequestParams url POST (ReqBodyJson body) 4444
  PostEmpty {} -> MkRequestParams url POST (ReqBodyJson $ object []) 4444
  Delete {} -> MkRequestParams url DELETE NoReqBody 4444
  where
    url =  foldl' (/:) (http "127.0.0.1") spec.path.segments
```

### 1.2 Call the WebDriver

*ReqRequestParams -> HttpResponse*

```haskell
callReq :: ReqRequestParams -> IO HttpResponse
callReq MkRequestParams {url, method, body, port = prt} =
  runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ do
    r <- req method url body jsonResponse $ port prt
    pure $
      MkHttpResponse
        { statusCode = responseStatusCode r,
          statusMessage = responseStatusText r,
          body = responseBody r :: Value
        }
  where
    responseStatusText = decodeUtf8Lenient . responseStatusMessage
```

### 1.3 Parse HttpResponse Using the Parser Provided in the [HttpSpec](#HttpSpec)

*HttpResponse -> Return Type*

```haskell
-- in this implementation we are just throwing exceptions on failure
parseResponse :: HttpSpec a -> HttpResponse -> IO a
parseResponse spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & \case
          e@NotAnError {} -> unpack spec.description <> "\n" <> 
                            "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a
```

### 2. Applying the Runner to the HttpSpec Functions

*Create an IO API by applying run to each endpoint definition exposed by this library*

The full source for can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IOAPI.hs).
```haskell
module IOAPI where 

import Data.Aeson (Value)
import Data.Text  as T (Text)
import WebDriverPreCore.Http (DriverStatus, ElementId, Selector, SessionId)
import WebDriverPreCore.Http qualified as W
import Prelude hiding (log)
import IOUtils (sleepMs, encodeFileToBase64)
import HttpRunner (run)

status :: IO DriverStatus
status = run W.status

newSession :: W.FullCapabilities -> IO SessionId
newSession = run . W.newSession

getTimeouts :: SessionId -> IO W.Timeouts
getTimeouts = run . W.getTimeouts

setTimeouts :: SessionId -> W.Timeouts -> IO ()
setTimeouts s = run . W.setTimeouts s

getCurrentUrl :: SessionId -> IO Text
getCurrentUrl = run . W.getCurrentUrl

getTitle :: SessionId -> IO Text
getTitle = run . W.getTitle

maximizeWindow :: SessionId -> IO W.WindowRect
maximizeWindow = run . W.maximizeWindow

-- ... and 50+ more API functions
```

### 3. Install a Vendor Provided WebDriver

*Once all the required endpoints are implemented you will be able to interact with browsers via WebDriver*

Examples:
  1. [Firefox](https://github.com/mozilla/geckodriver/releases)
  2. [Chrome](https://googlechromelabs.github.io/chrome-for-testing/)
  3. [Edge](https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver?form=MA13LH)
  4. [Opera](https://github.com/operasoftware/operachromiumdriver?tab=readme-ov-file)
  5. [Safari](https://developer.apple.com/documentation/webkit/testing-with-webdriver-in-safari)

*Ensure the corresponding browser is installed on your system*

### 4. Launch WebDriver From the Terminal

e.g. For Firefox and geckodriver on Linux or WSL you could start geckodriver from the terminal as follows: 

*Note: we are setting the port to 4444, which is the hard coded port in our example.*

```bash
> pkill -f geckodriver || true  && geckodriver --port=4444 &
```
or with logging:

```bash
> pkill -f geckodriver || true  && geckodriver --log trace --port=4444 &
```

or similarly for Chrome and chromedriver:

```bash
> pkill -f chromedriver || true && chromedriver --port=4444 &
```

or with logging:

```bash
> pkill -f chromedriver || true && chromedriver --log-level=ALL --port=4444 &
```

### 5. Drive the Browser Via the IO API

*With the driver running you can now run code that interacts with the browser:*

Full source file can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/HttpE2EDemoTest.hs).

```haskell
demoForwardBackRefresh :: IO ()
demoForwardBackRefresh = do
  ses <- newSession $ minFullCapabilities Firefox
  navigateTo ses "https://the-internet.herokuapp.com/"
  link findElement ses $ CSS "#content ul:nth-child(4) > li:nth-child(6) > a:nth-child(1)"
  elementClick ses link
  back ses
  forward ses
  refresh ses
  deleteSession ses
```

*This is a minimal API. There is plenty of scope to build on this to provide more constrained types, user-friendly functions and capabilities such as retries, and session and driver management.*