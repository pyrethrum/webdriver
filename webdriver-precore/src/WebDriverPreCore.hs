{-|
Module      : WebDriverPreCore
Description : W3C WebDriver Protocol Typed Definitions
Copyright   : (c) 2025 John Walker, Adrian Glouftsis
License     : BSD-3-Clause
Maintainer  : theghostjw@gmail.com
Stability   : experimental

= Overview

This library provides typed definitions for the W3C WebDriver Protocol,
supporting both the @<https://www.w3.org/TR/2025/WD-webdriver2-20251028/ HTML>@ and the @<https://www.w3.org/TR/2025/WD-webdriver-bidi-20251212/ BiDi>@ protocols,

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
    API functions for BiDi commands and event subscriptions. Each command function 
    returns a 'Command' value with the WebSocket message specification and response type.
    
    Example usage:
    
    @
    -- Calling the API function to generate the command payload
    let navCommand = browsingContextNavigate MkNavigate 
          { context = "context-id-123"
          , url = "https://example.com"
          , wait = Just Interactive
          }
    
    -- API function definition
    browsingContextNavigate :: Navigate -> Command NavigateResult
    browsingContextNavigate = mkCommand BrowsingContextNavigate
    
    -- The result of the API function call contains method and params to send to the WebSocket
    data Command r = MkCommand
      { method :: CommandMethod    -- e.g., "browsingContext.navigate"
      , params :: Object           -- e.g., {"context": "...", "url": "..."}
      }
    @

"WebDriverPreCore.BiDi.Protocol" 
    Protocol types including 'Command', 'Event', request parameters, and response types.
    
    The 'Navigate' parameter type and 'NavigateResult' response type:
    
    @
    data Navigate = MkNavigate
      { context :: BrowsingContext
      , url :: URL
      , wait :: Maybe ReadinessState
      }
    
    data NavigateResult = MkNavigateResult
      { navigation :: Maybe Text
      , url :: URL
      }
    @

== Shared

[@WebDriverPreCore.Error@] Error types used by both HTTP and BiDi protocols

= Quick Start

To build a WebDriver client:

1. Use the API functions to get 'HttpSpec' or 'BiDiSpec' values
2. Pattern match on these specs to extract HTTP\/WebSocket details
3. Send the corresponding requests to your WebDriver server
4. Parse responses using the provided parser functions

For complete examples, see the 
[examples directory](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/README.md).


== BiDi Protocol (Recommended for New Projects)

The BiDi protocol provides modern, bidirectional communication with support for
events and real-time updates:

[@WebDriverPreCore.BiDi.API@] BiDi command functions for all WebDriver operations

[@WebDriverPreCore.BiDi.Protocol@] BiDi protocol types, commands, events, and responses

== HTTP Protocol (Stable)

The HTTP protocol provides the traditional request-response pattern:

[@WebDriverPreCore.HTTP.API@] HTTP endpoint functions for all WebDriver operations

[@WebDriverPreCore.HTTP.Protocol@] HTTP protocol types, specifications, and responses

== Shared Modules

[@WebDriverPreCore.Error@] Error types used by both protocols

== Deprecated Modules (Removal planned ~ 2027-02-01)

⚠️  The following modules are deprecated. Please migrate to their replacements.
See @ChangeLog.md@ for detailed migration instructions.

[@WebDriverPreCore.Http@] 
    __Deprecated:__ Use "WebDriverPreCore.HTTP.Protocol" instead
    
    This module re-exported various HTTP protocol components. The functionality 
    has been reorganized into the Protocol module.

[@WebDriverPreCore.Http.SpecDefinition@] 
    __Deprecated:__ Use "WebDriverPreCore.HTTP.API" instead
    
    This module contained the HTTP API endpoint functions. It has been renamed 
    to better reflect the distinction between API (functions) and Protocol (types).

[@WebDriverPreCore.Http.HttpResponse@] 
    __Deprecated:__ Functionality integrated into "WebDriverPreCore.HTTP.Protocol"
    
    The 'HttpResponse' type is now part of the Protocol module where it belongs
    with other HTTP protocol types.

= Migration Guide

== From Deprecated HTTP Modules

If you're using the deprecated modules:

> import WebDriverPreCore.Http
> import WebDriverPreCore.Http.SpecDefinition
> import WebDriverPreCore.Http.HttpResponse

__Replace with:__

> import WebDriverPreCore.HTTP.API      -- For endpoint functions
> import WebDriverPreCore.HTTP.Protocol  -- For types and specs

The API remains largely unchanged; only import paths need updating.

= Building a WebDriver Client

A minimal WebDriver client needs to:

1. __Send requests:__ Extract HTTP method, URL, and body from 'HttpSpec' or command from 'BiDiSpec'
2. __Parse responses:__ Use the parser provided in the spec
3. __Handle errors:__ Process 'WebDriverPreCore.Error.WebDriverError' values

Example structure:

@
import WebDriverPreCore.HTTP.API qualified as API
import WebDriverPreCore.HTTP.Protocol

runHttpSpec :: HttpSpec a -> IO a
runHttpSpec spec = do
  response <- sendHttpRequest spec.method spec.endpoint spec.body
  case spec.parser response of
    Left err -> throwIO err
    Right val -> pure val
@

= Further Reading

* [Project repository](https://github.com/pyrethrum/webdriver)
* [Examples](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/README.md)
* [W3C WebDriver Specification](https://www.w3.org/TR/webdriver2/)

= Alternatives

If you need a complete, ready-to-use WebDriver client library, consider
[haskell-webdriver](https://github.com/haskell-webdriver/haskell-webdriver#readme).

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
