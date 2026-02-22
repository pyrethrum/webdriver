{-|
Module: HttpActions
Description: Minimal HTTP actions for BiDi demo session management

This module provides just the essential HTTP actions needed for BiDi demos:
creating and deleting sessions to obtain the webSocketUrl for BiDi connection.
-}
module HttpActions
  ( HttpActions (..),
    mkActions,
  )
where

import Data.Aeson (FromJSON)
import WebDriverPreCore.HTTP.API qualified as API
import WebDriverPreCore.HTTP.Protocol
  ( Command,
    FullCapabilities,
    Session,
    SessionResponse,
  )

data HttpActions = MkHttpActions
  { -- Session management for BiDi connection
    newSession :: FullCapabilities -> IO SessionResponse,
    deleteSession :: Session -> IO ()
  }

mkActions :: (forall r. (FromJSON r) => Command r -> IO r) -> HttpActions
mkActions run =
  MkHttpActions
    { newSession = run . API.newSession,
      deleteSession = run . API.deleteSession
    }
