
{-| 
This module defines the 'Command' type and related types/functions for BiDi commands referenced by the API functions in "WebDriverPreCore.BiDi.API" 

"WebDriverPreCore.BiDi.API" contains functions that generate the payload for each BiDi command and is the main interface for users of this package to interact with BiDi.

An example of using these modules to implement a basic BiDi client can be found in the [test repository](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test#readme) for this package.
-}

module WebDriverPreCore.BiDi.Protocol
  ( 
    -- * Command
    Command (..),
    CommandMethod (..),
    KnownCommand (..),
    OffSpecCommand (..),
     -- ** Constructors
     --
     -- | The following constructors are provided for creating 'Command' values, and used in the API functions in "WebDriverPreCore.BiDi.API".
     --   
    mkCommand,
    emptyCommand,
    -- ** Fallback Constructors
    mkOffSpecCommand,
    extendCommand,
    extendLoosenCommand,
    extendCoerceCommand,
    loosenCommand,
    coerceCommand,
    -- ** Command Method Utilities
    knownCommandToText,
    toCommandText,
    -- * Browser
    module WebDriverPreCore.BiDi.Browser,
    -- * Browsing Context
    module WebDriverPreCore.BiDi.BrowsingContext,
    -- * Capabilities
    module WebDriverPreCore.BiDi.Capabilities,
    -- * Event
    module WebDriverPreCore.BiDi.Event,
    -- * Emulation
    module WebDriverPreCore.BiDi.Emulation,
    -- * Input
    module WebDriverPreCore.BiDi.Input,
    -- * Log
    module WebDriverPreCore.BiDi.Log,
    -- * Script
    module WebDriverPreCore.BiDi.Script,
    -- * Session
    module WebDriverPreCore.BiDi.Session,
    -- * Storage
    module WebDriverPreCore.BiDi.Storage,
    -- * Web Extensions
    module WebDriverPreCore.BiDi.WebExtensions,
    -- * Network
    module WebDriverPreCore.BiDi.Network,
    -- * Core Types
    module WebDriverPreCore.BiDi.CoreTypes,
    -- * Error
    module WebDriverPreCore.Error,
  )
where

import WebDriverPreCore.BiDi.Browser
import WebDriverPreCore.BiDi.BrowsingContext
import WebDriverPreCore.BiDi.Capabilities
import WebDriverPreCore.BiDi.Command
import WebDriverPreCore.BiDi.CoreTypes
import WebDriverPreCore.BiDi.Emulation
import WebDriverPreCore.Error
import WebDriverPreCore.BiDi.Event
import WebDriverPreCore.BiDi.Input
import WebDriverPreCore.BiDi.Log
import WebDriverPreCore.BiDi.Network
import WebDriverPreCore.BiDi.Script
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.BiDi.Storage
import WebDriverPreCore.BiDi.WebExtensions
