{-|
Module      : WebDriverPreCore
Description : W3C WebDriver Protocol Typed Definitions
Copyright   : (c) 2025 John Walker, Adrian Glouftsis
License     : BSD-3-Clause
Maintainer  : theghostjw@gmail.com
Stability   : experimental

= Overview

This library provides typed definitions for the 
@<https://www.w3.org/TR/2025/WD-webdriver2-20251028/ W3C WebDriver>@ protocol,
supporting both the HTTP and BiDi (Bidirectional) protocols.

It is designed as a foundation for building WebDriver client implementations, 
providing type-safe endpoint definitions and response parsers without imposing 
a specific HTTP or WebSocket client library.

= Quick Start

To build a WebDriver client:

1. Use the API functions to get 'HttpSpec' or 'BiDiSpec' values
2. Pattern match on these specs to extract HTTP\/WebSocket details
3. Send the corresponding requests to your WebDriver server
4. Parse responses using the provided parser functions

For complete examples, see the 
[examples directory](https://github.com/pyrethrum/webdriver/blob/main/webdriver-examples/README.md).

= Module Organization

== BiDi Protocol (Recommended for New Projects)

The BiDi protocol provides modern, bidirectional communication with support for
events and real-time updates:

[@WebDriverPreCore.BiDi.API@] BiDi command functions for all WebDriver operations

[@WebDriverPreCore.BiDi.Protocol@] BiDi protocol types, commands, events, and responses

== HTTP Protocol (Stable)

The HTTP protocol provides the traditional request-response pattern:

[@WebDriverPreCore.Http.API@] HTTP endpoint functions for all WebDriver operations

[@WebDriverPreCore.Http.Protocol@] HTTP protocol types, specifications, and responses

== Shared Modules

[@WebDriverPreCore.Error@] Error types used by both protocols

== Deprecated Modules (Removal planned ~ 2027-02-01)

⚠️  The following modules are deprecated. Please migrate to their replacements.
See @ChangeLog.md@ for detailed migration instructions.

[@WebDriverPreCore.Http@] 
    __Deprecated:__ Use "WebDriverPreCore.Http.Protocol" instead
    
    This module re-exported various HTTP protocol components. The functionality 
    has been reorganized into the Protocol module.

[@WebDriverPreCore.Http.SpecDefinition@] 
    __Deprecated:__ Use "WebDriverPreCore.Http.API" instead
    
    This module contained the HTTP API endpoint functions. It has been renamed 
    to better reflect the distinction between API (functions) and Protocol (types).

[@WebDriverPreCore.Http.HttpResponse@] 
    __Deprecated:__ Functionality integrated into "WebDriverPreCore.Http.Protocol"
    
    The 'HttpResponse' type is now part of the Protocol module where it belongs
    with other HTTP protocol types.

= Migration Guide

== From Deprecated HTTP Modules

If you're using the deprecated modules:

> import WebDriverPreCore.Http
> import WebDriverPreCore.Http.SpecDefinition
> import WebDriverPreCore.Http.HttpResponse

__Replace with:__

> import WebDriverPreCore.Http.API      -- For endpoint functions
> import WebDriverPreCore.Http.Protocol  -- For types and specs

The API remains largely unchanged; only import paths need updating.

= Building a WebDriver Client

A minimal WebDriver client needs to:

1. __Send requests:__ Extract HTTP method, URL, and body from 'HttpSpec' or command from 'BiDiSpec'
2. __Parse responses:__ Use the parser provided in the spec
3. __Handle errors:__ Process 'WebDriverPreCore.Error.WebDriverError' values

Example structure:

@
import WebDriverPreCore.Http.API qualified as API
import WebDriverPreCore.Http.Protocol

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
-- import WebDriverPreCore.Http.API
-- import WebDriverPreCore.Http.Protocol
-- import WebDriverPreCore.Error
-- @
