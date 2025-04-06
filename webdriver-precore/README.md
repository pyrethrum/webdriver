# webdriver-precore

- [webdriver-precore](#webdriver-precore)
  - [What is This Library?](#what-is-this-library)
  - [Why This Library?](#why-this-library)
    - [Core Principles](#core-principles)
    - [Library Non-Goals](#library-non-goals)
- [Minimal Example](#minimal-example)
  - [1. Implementing a runner](#1-implementing-a-runner)
      - [run function](#run-function)
    - [1.1 Convert W3Spec to params for req](#11-convert-w3spec-to-params-for-req)
    - [1.2 Call the WebDriver](#12-call-the-webdriver)
    - [1.3 Parse HttpResponse using the parser provided in the W3Spec](#13-parse-httpresponse-using-the-parser-provided-in-the-w3spec)
    - [2. Applying the runner to the W3Spec functions](#2-applying-the-runner-to-the-w3spec-functions)
    - [3. Obtain and Launch a Vendor Provided WebDriver](#3-obtain-and-launch-a-vendor-provided-webdriver)
      - [3.1 Drivers Can Be Obtained From the Devlopers' Web Sites](#31-drivers-can-be-obtained-from-the-devlopers-web-sites)
      - [3.2 Launch WebDriver From the Console](#32-launch-webdriver-from-the-console)
      - [4. Drive the browser by calling the IO API](#4-drive-the-browser-by-calling-the-io-api)

## What is This Library?

This library provides a minimal abstraction over the [WebDriver W3C Protocol endpoints]() without providing any implementation. It provides a description of the W3C API as a list of functions that return a [W3Spec type](). The intention is that other libraries will provide the actual implementation.

You can not use this library directly to drive a browser. If you are looking for a library to drive a browser, you may be interested in an alternative library such as [haskell-webdriver](https://hackage.haskell.org/package/webdriver), a Selenium 2 client that is actively maintained.

## Why This Library?

There are a number of libraries that provide WebDriver bindings for Haskell. However, at the time work on this library commenced, the available libraries were not being actively maintained, depended on Selenium, or part of larger opinionated testing frameworks.

The intent of the authors is to write a number of low level libraries to enable web UI testing in our own stand alone test framework. This is the first of those libraries.

### Core Principles
- **Direct W3C WebDriver Implementation**  
  - No Selenium dependency  
  - Full control over protocol handling  
  *Note: the [W3C WebDriver standard]() is an initiative driven largely by the core Selenium contributors. It provides a uniform HTTP API to drive browsers, and can be leveraged by any library, including Selenium.*

- **Minimalist Design**  
  - Boring Haskell
  - Few external dependencies  

- **Enable a Layered Architecture**  
  - Provide an unopinionated WebDriver client for use in higher level libraries

### Library Non-Goals
  Any convenience / utility functions that do not directly correspond to an endpoint on the WC3 spec. Such functions belong in downstream libraries.

# Minimal Example

Driving a browser using this library requires the user to implement the following:
1. Implement a `runner` that takes a [W3Spec]() and calls a running WebDriver
2. Create an IO API by applying the runner to each of the endpoint funtions in this library
3. HERE !!!!!
## 1. Implementing a runner

This is an example of a minimal runner that implements the interaction with WebDriver endpoint definitions as provided by this library.

The first step in writing a WebDriver implementation is to choose an HTTP library. In this example, the chosen library is [req]().

Then to implement a run function requires the following:

1. Transform a [W3Spec]() to RequestParams compatible with the chosen HTTP library.
2. Make an HTTP call to WebDriver as per the RequestParams and return a simplified [HttpResponse]().
3. Use the parser provided by the [W3Spec]() to transform the [HttpResponse]() to the desired result type and handle any errors.

#### run function

The full source for can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IORunner.hs).


```haskell
run :: W3Spec a -> IO a
run spec = do
  -- 1. Convert W3Spec to params for req
  let request = mkRequest spec
  -- 2. Call WebDriver server (via req) and return a simplified HttpResponse 
  response <- callReq request
  -- 3. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors  
  parseResponse spec response  
```

### 1.1 Convert W3Spec to params for req

*W3Spec -> ReqRequestParams*

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

-- W3Spec -> ReqRequestParams
mkRequest :: forall a. W3Spec a -> ReqRequestParams
mkRequest spec = case spec of
  Get {} -> MkRequestParams url GET NoReqBody 4444 -- port 4444 would not normally be hard coded
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

### 1.3 Parse HttpResponse using the parser provided in the [W3Spec]()

*HttpResponse -> Return Type*

```haskell
-- in this implementation we are just throwing exceptions on failure
parseResponse :: W3Spec a -> HttpResponse -> IO a
parseResponse spec r =
  spec.parser r
    & \case
      Error msg -> fail $ parseWebDriverError r & case
          e@NotAnError {} -> unpack spec.description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> show e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> show e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> show e
      Success a -> pure a
```

### 2. Applying the runner to the W3Spec functions

*Create an IO API by applying run to each endpoint definition exposed by this library*


The full source for can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IOAPI.hs).
```haskell
module IOAPI where 

import Data.Aeson (Value)
import Data.Text  as T (Text)
import WebDriverPreCore (DriverStatus, ElementId, Selector, SessionId)
import WebDriverPreCore qualified as W
import Prelude hiding (log)
import IOUtils (sleepMs, encodeFileToBase64)
import IORunner (run)

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

### 3. Obtain and Launch a Vendor Provided WebDriver

*Once all the required endpoints are implemented you will be able to interact with browsers via WebDriver*

#### 3.1 Drivers Can Be Obtained From the Devlopers' Web Sites

Examples:
  1. [firefox](https://github.com/mozilla/geckodriver/releases)
  2. [chrome](https://googlechromelabs.github.io/chrome-for-testing/)
  3. [edge](https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver?form=MA13LH)
  4. [opera](https://github.com/operasoftware/operachromiumdriver?tab=readme-ov-file)
  5. [safari](https://developer.apple.com/documentation/webkit/testing-with-webdriver-in-safari)

#### 3.2 Launch WebDriver From the Console

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

#### 4. Drive the browser by calling the IO API

*With the driver running you can now run code that interacts with the browser:*

Full source file can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/WebDriverE2EDemoTest.hs).

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

*This is a minimal API. There is plenty of scope to build on this to provide a more user-friendly functions and capabilites such as retries, and session and driver management.*