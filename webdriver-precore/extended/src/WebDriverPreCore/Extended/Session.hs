-- |
-- Module: WebDriverPreCore.Extended.Session
-- Description: Session management for the Extended WebDriver API
module WebDriverPreCore.Extended.Session
  ( -- * Session Management
    newHttpSession
  )
where

import WebDriverPreCore.Extended.Capabilities
  ( FullCapabilities,
    HttpCapability,
    HttpSessionResponse,
    fromHttpSessionResponse,
    toHttpCapabilities,
  )
import WebDriverPreCore.Extended.HTTP.Base.Actions (Runner, newSession)
import WebDriverPreCore.HTTP.Protocol qualified as HTTP

-- | Create a new HTTP WebDriver session with the given capabilities.
--
-- Specification Entry: [HTMLSpecURL#new-session](https://www.w3.org/TR/webdriver/#new-session)
--
-- @POST \/session New Session@
newHttpSession ::
  forall m.
  (Functor m) =>
  Runner m HTTP.SessionResponse ->
  FullCapabilities HttpCapability ->
  m HttpSessionResponse
newHttpSession runner =
  fmap fromHttpSessionResponse . newSession runner . toHttpCapabilities
