{-# LANGUAGE CPP #-}

{-|
Module      : WebDriverPreCore
Description : W3C WebDriver Protocol Typed Definitions
Copyright   : (c) 2025 John Walker, Adrian Glouftsis
License     : BSD-3-Clause
Maintainer  : theghostjw@gmail.com
Stability   : experimental 

= Overview

This library provides typed definitions for the W3C WebDriver Protocol, supporting both the [HTML](HTMLSpecURL) and the [BiDi](BiDiSpecURL) protocols.

This library is intended as a foundation for building WebDriver client implementations. __It is type constructors only__, and does not include any executable client code.

If you are writing a webdriver client, this library will save you the effort of analysing the specs and implementing the protocol types and JSON instances.

If you are looking for a library to enable you to interact with web pages directly then you need a fully implemented web client library __which this library is not__.

For a fully implemented webdriver client, consider an alternative such as [haskell-webdriver](https://github.com/haskell-webdriver/haskell-webdriver#readme)

= Which Protocol?

- [BiDi](BiDiSpecURL) is a newer websocket protocol. It supports a wider range of features including real-time event subscriptions, improved handling of multiple browsing contexts, and enhanced scripting capabilities. 
- [HTML](HTMLSpecURL) is the original WebDriver protocol, primarily focused on browser automation using HTTP requests. It is widely supported by existing WebDriver implementations.


Note that the Bidi protocol is [still evolving](https://github.com/w3c/webdriver-bidi/blob/main/roadmap.md), and API coverage [varies between drivers](https://wpt.fyi/results/webdriver/tests/bidi?label=experimental&label=master&aligned). Depending on your use case and the WebDriver server you are targeting, you may choose to implement either or both protocols.

For an introduction to WebDriver Bidi and its use cases, see:

- [WebDriver BiDi: Future of browser automation](https://www.youtube.com/watch?v=6oXic6dcn9w)
- [Example of WebDriver BiDi implementation in wdio (TypeScript) framework](https://youtu.be/2TBrFgSkqNE?si=6vl1RlPSjcA18YBy)

= Module Organisation

Both HTTP and BiDi protocols follow a the same module structure. Each has two modules:

__1. An API module__

    Contains type constructors that generate a payload corresponding to each endpoint/command in the relevant WebDriver specification.

__2. A Protocol module__
    Contains:

       1. types used by the API functions

       2. fallback functions that allow the user to generate payloads that diverge from API

== HTTP 

"WebDriverPreCore.HTTP.API"

    Example usage:
    
    @
    -- Calling the API (navigateTo) function to generate the navigation command payload
    let goExample :: Command ()
        goExample = navigateTo (Session "session-id-123") $ MkUrl "https://example.com"
    
  
    -- The result of the API function call contains HTTP request details required for the driver 'WebDriverPreCore.HTTP.Protocol.Command' 
     'Post'
        { description = "Navigate To",
        , path = "/session/session-id-123/url" 
        , body = {"url": "https://example.com"}
        }
    @

"WebDriverPreCore.HTTP.Protocol"

    Protocol types including 'WebDriverPreCore.HTTP.Protocol.Command', request parameters, and response and error types 
      such as URL, and Session.

== BiDi

"WebDriverPreCore.BiDi.API"

    API functions for BiDi commands and event subscriptions. Each command function returns a 'WebDriverPreCore.BiDi.Protocol.Command' value with the WebSocket message specification and response type.
    
    Example usage:
    
    @
    -- Calling the API function to generate the command payload
    let navCommand :: Command NavigateResult
        navCommand = browsingContextNavigate MkNavigate 
          { context = "context-id-123"
          , url = "https://example.com"
          , wait = Just Interactive
          }
    
    -- The result is a Command value with the method and params to send to the WebSocket
       navCommand = MkCommand
         { method = KnownCommand BrowsingContextNavigate
         , params = fromList [("context", String "context-id-123"), 
                              ("url", String "https://example.com"), 
                              ("wait", String "interactive")]
    } 
    @

"WebDriverPreCore.BiDi.Protocol" 

  Protocol types including 'Command', 'Event', request parameters, and response and error types
   such as 'KnownCommand'.

= Using this Library to Implement a WebDriver Client

Implementing a WebDriver client involves:

1. Writing a runner to send HTTP requests or WebSocket messages to WebDriver
2. Creating some kind of abstraction, such as handles, a typeclass, or the use of an effects library to call the runner and hence lift the static type constructors provided by the API into IO actions.
3. Handling errors using the shared error types

Example implementations for both BiDi and HTTP runners, in this case using [the handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)), can be found in the [test repository](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test#readme) for this package.

== Example Implementation (Single Endpoint)
Using navigation as an example with both protocols, the client implementation is implemented as follows:

=== HTTP

__The Runner__ takes commands and sends HTTP requests to the driver and parses responses (implemented in [HTTP.Runner](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/Runner.hs))

__An Actions Type__ mirrors the type API functions with the response wrapped in IO (implemented in [HTTP.Actions](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/Actions.hs))

__An Actions Implementation__ an instance of the Actions type that uses the runner to send commands (implemented in [HTTP.Actions](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/Actions.hs))

@
import WebDriverPreCore.HTTP.API qualified as API
import WebDriverPreCore.HTTP.Protocol (Session, URL)

-- The Actions type
data HttpActions = MkHttpActions
  { navigateTo :: Session -> URL -> IO ()
  , ...
  }

-- The Actions implementation
mkActions :: HttpRunner -> HttpActions
mkActions runner = MkHttpActions
  { navigateTo = \sessionId url -> do
      let command = API.navigateTo sessionId url
      runner.runHttpCommand command
  , ...
  }
@

With the above in place, a /run/ or /withActions/ function can be created to provide the user a means to perform WebDriver operations.

==== User's (of the Client Implementation) Module

A user of the client implementation would import the Actions module for performing operations and the "WebDriverPreCore.HTTP.Protocol" module for the related types and utility functions.

@
import WebDriverPreCore.HTTP.Protocol 
import HTTP.Actions
import HTTP.Runner (mkRunner)
import HTTP.DemoUtils (HttpDemo, runDemo, sessionDemo)

-- >>> runDemo navigate
demoNavigate :: HttpDemo
demoNavigate =
  sessionDemo "navigate" action
  where
    action :: Session -> HttpActions -> IO ()
    action sesId MkHttpActions {..} = do
      navigateTo sesId $ MkUrl "https://example.com/index.html"

@

Full example is available in the [test repository](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/HttpDemo.hs)  

=== BiDi

A BiDi client implementation follows the same pattern as HTTP, with a runner, actions type, and actions implementation. It is somewhat more complicated than HTTP due to the asynchronous nature of WebSocket communication and event handling, the need to create correlation IDs for commands and the need to manage subscriptions for events.

Example implementations can be found in the [test repository](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test#readme)

= Deprecated Modules (Removal planned ~ 2027-02-01)

⚠️  The following modules are deprecated. Please migrate to their replacements.

"WebDriverPreCore.Http"
    __Deprecated:__ Use "WebDriverPreCore.HTTP.Protocol" instead
    
    This module re-exported various HTTP protocol components. The functionality 
    has been reorganized into the Protocol module.

"WebDriverPreCore.HTTP.SpecDefinition" (formerly @WebDriverPreCore.Http.SpecDefinition@)
    __Deprecated:__ Use "WebDriverPreCore.HTTP.API" instead
    
    This module contained the HTTP API endpoint functions. It has been renamed 
    to better reflect the distinction between API (functions) and Protocol (types).

"WebDriverPreCore.HTTP.HttpResponse" (formerly "WebDriverPreCore.Http.HttpResponse")
    __Deprecated:__ This type is implementation specific and has been removed. The parser supplied in this type is now implicit, as the return types of all Commands are instances of 'FromJSON'.


== Migration for Deprecated Modules

See the [ChangeLog](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/ChangeLog.md) for details
-}


module WebDriverPreCore () where
  -- This module is intentionally left empty. It serves as a placeholder for the package documentation.
