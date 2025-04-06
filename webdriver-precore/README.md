## What is This Library?

This library provides a minimal abstraction over the [WebDriver W3C Protocol endpoints]() without providing any implementation. It provides a description of the W3C API as a list of functions that return a [W3Spec type](). The intention is that other libraries will provide the actual implementation.

You can not use this library directly to drive a browser. If you are looking for a library to drive a browser, you may be interested in an alternative library such [haskell-webdriver](https://hackage.haskell.org/package/webdriver), a Selenium 2 client that is actively maintained.

## Why This Library?

There are a number of libraries that provide WebDriver bindings for Haskell. However, at the time work on this library commenced, the available libraries were either in need of maintenance, required Selenium, or part of larger opinionated testing frameworks.

The intent of the authors is to write a number of low level libraries that we can then depend on to add web UI testing to our own stand alone test framework. This is the first of those libraries.

### Core Principles
- **Direct W3C WebDriver Implementation**  
  - No Selenium dependency  
  - Full control over protocol handling  

- **Minimalist Design**  
  - Few external dependencies  
  - No forced adoption of unrelated concepts  

- **Enable a Layered Architecture**  
  - Provide an unopinionated WebDriver client for use in higher level libraries

### Library Non-Goals
  Any convenience / utility functions that do not directly correspond to an endpoint on the WC3 spec. Such functions belong in downstream libraries.

## 1. Implementing a runner

This is an example of a minimal runner that implements the interaction with WebDriver endpoint definitions as provided by this library.

The first step in writing a WebDriver implementation is to choose an HTTP library. In this example, the chosen library is [req]().

Then to implement a run function requires the following:

1. Transform a [W3Spec]() to RequestParams compatible with the chosen HTTP library.
2. Make an HTTP call to WebDriver as per the RequestParams and return a simplified [HttpResponse]().
3. Use the parser provided by the [W3Spec]() to transform the [HttpResponse]() to the desired result type and handle any errors.

### Example

The full source for can be found in the [example project repo](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/driver-demo-e2e/IORunner.hs).

#### run function

```haskell
run :: W3Spec a -> IO a
run spec = do
  let request = mkRequest spec -- 1. Convert W3Spec to params for req
  response <- callReq request  -- 2. Call WebDriver server (via req) and return a simplified HttpResponse
  parseResponse spec response  -- 3. Use the W3Spec parser to convert the HttpResponse to the desired result type and handle any errors
```

### 1.1 Convert W3Spec to params for req

**W3Spec -> ReqRequestParams**

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

**Call WebDriver endpoints based on ReqRequestParams**

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

*Parse the WebDriver response (in this implementation we are just throwing exceptions on failure)*

```haskell
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

**Create an IO API by applying run to each endpoint definition exposed by this library**


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

### Using the API

*Once all the required endpoints are implemented you will be able to interact with browsers via WebDriver*

#### Prerequisites:

1. An appropriate browser and WebDriver installed
2. WebDriver started

*Drivers can be downloaded from the developers' web sites: e.g. [chrome](https://googlechromelabs.github.io/chrome-for-testing/), [firefox](https://github.com/mozilla/geckodriver/releases), [edge](https://developer.microsoft.com/en-us/microsoft-edge/tools/webdriver?form=MA13LH)*

*Start WebDriver from the console*

e.g. For Firefox and geckodriver on Linux or WSL you could start geckodriver from the terminal as follows: Note: we are setting the port to 4444, which is the hard coded port in our example.

```bash
>>> pkill -f geckodriver || true  && geckodriver --port=4444 &
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

*This is a minimal API. There is plenty of scope to build on this to provide a more user-friendly functions.*