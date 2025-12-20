{-|
Module      : WebDriverPreCore
Description : W3C WebDriver Protocol Typed Definitions
Copyright   : (c) 2025 John Walker, Adrian Glouftsis
License     : BSD-3-Clause
Maintainer  : theghostjw@gmail.com
Stability   : experimental 

= Overview

This library provides typed definitions for the W3C WebDriver Protocol, supporting both the $HTMLSpecURL and the $BiDiSpecURL protocols.

This library is intended as a foundation for building WebDriver client implementations. 

If you are looking for a fully implemented web client library, you should check out an alternative such as [haskell-webdriver](https://github.com/haskell-webdriver/haskell-webdriver#readme)

= Module Organisation

Both HTTP and BiDi protocols follow a the same module structure. 

__An API module__

    Contains functions that generate the payload for each WebDriver endpoint/command

__A Protocol module__

    Contains:

       1. types used by the API functions

       2. fallback functions that allow the user to generate payloads that diverge from API

== HTTP 

"WebDriverPreCore.HTTP.API"

    API functions corresponding to W3C WebDriver endpoints. Each function returns 
    a 'Command' value representing the HTTP request specification and response type.
    
    Example usage:
    
    @
    -- Calling the API (navigateTo) function to generate the command payload
    let goExample :: Command ()
        goExample = navigateTo "session-id-123" $ MkUrl "https://example.com"
    
  
    -- The result of the API function call contains HTTP request details required for the driver ('WebDriverPreCore.HTTP.Protocol.Command') 
     'Post'
        { description = "Navigate To",
        , path = "/session/session-id-123/url" 
        , body = {"url": "https://example.com"}
        }
    @

"WebDriverPreCore.HTTP.Protocol"

    Protocol types including 'Command', request parameters, and response types 
    used by the API functions.

== BiDi

"WebDriverPreCore.BiDi.API"

    API functions for BiDi commands and event subscriptions. Each command function returns a 'Command' value with the WebSocket message specification and response type.
    
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

  Protocol types including 'Command', 'Event', request parameters, and response types.

== Shared

"WebDriverPreCore.Error" Error types used by both HTTP and BiDi protocols

= Implementing a WebDriver Client

Implementing a WebDriver client involves:

1. Writing a runner to send HTTP requests or WebSocket messages to WebDriver
2. Creating some kind of abstraction, such as handles, a typeclass, or the use of an effects library to send commands genrated by the API functions and parse the responses
3. Handling errors using the shared error types

Example implementations for both BiDi and HTTP runners, in this caase, using [the handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)), can be found in the [test repository](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test#readme) for this package.

== Example Implementation

Using navigation as an example with both protocols, the client implmentation is implemented as follows:

=== HTTP

__The Runner__ takes commands and sends HTTP requests to the driver and parses responses (implemented in [HTTP.HttpRunner](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/HttpRunner.hs))

__An Actions Type__ mirrors the type API functions with the response wrapped in IO (implemented in [HTTP.HttpActions](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/HttpActions.hs))

__An Actions Implementation__ an instance of the Actions type that uses the runner to send commands (implemented in [HTTP.HttpActions](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/HttpActions.hs))

@
import WebDriverPreCore.HTTP.API qualified as API
import WebDriverPreCore.HTTP.Protocol (SessionId, URL)

-- The Actions type
data HttpActions = MkHttpActions
  { navigateTo :: SessionId -> URL -> IO ()
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

With the above in place, a /run/ or /withActions/ function can be created to provide the user a means to perfom WebDriver operations.

==== User's Module

@
import Client.HTTP.Actions
import WebDriverPreCore.HTTP.Protocol 
import HTTP.HttpRunner (mkRunner)
import HTTP.DemoUtils (HttpDemo, demo, runDemo, sessionDemo)
import HTTP.HttpActions (HttpActions (..))

-- >>> runDemo navigate
demoForwardBackRefresh :: HttpDemo
demoForwardBackRefresh =
  sessionDemo "navigate" action
  where
    action :: SessionId -> HttpActions -> IO ()
    action sesId MkHttpActions {..} = do
      navigateTo sesId $ MkUrl "https://example.com/index.html "

@

Full example is available in the [test repository]((https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/HTTP/HttpDemo.hs)   


= Deprecated Modules (Removal planned ~ 2027-02-01)

⚠️  The following modules are deprecated. Please migrate to their replacements.
See @ChangeLog.md@ for detailed migration instructions.

[@WebDriverPreCore.Http@] 
    __Deprecated:__ Use "WebDriverPreCore.HTTP.Protocol" instead
    
    This module re-exported various HTTP protocol components. The functionality 
    has been reorganized into the Protocol module.

[@WebDriverPreCore.HTTP.SpecDefinition@] (formerly @WebDriverPreCore.Http.SpecDefinition@)
    __Deprecated:__ Use "WebDriverPreCore.HTTP.API" instead
    
    This module contained the HTTP API endpoint functions. It has been renamed 
    to better reflect the distinction between API (functions) and Protocol (types).

[@WebDriverPreCore.Http.HttpResponse@] 
    __Deprecated:__ This type is implementation specific and has been removed. The parser supplied in this type is now implicit, as the return types of all Commands are instances of 'FromJSON'.


= Migration For Depricated Modules

See the [ChangeLog](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/ChangeLog.md) for details
-}

module WebDriverPreCore () where

-- This is a documentation-only module.
-- Import the specific modules you need:
--
-- @
-- import WebDriverPreCore.BiDi.API
-- import WebDriverPreCore.BiDi.Protocol
--
-- import WebDriverPreCore.HTTP.API
-- import WebDriverPreCore.HTTP.Protocol
--
-- import WebDriverPreCore.Error
-- @

-- $HTMLSpecURL
-- https://www.w3.org/TR/2025/WD-webdriver2-20251028/

-- $BiDiSpecURL
-- https://www.w3.org/TR/2025/WD-webdriver-bidi-20251212/
