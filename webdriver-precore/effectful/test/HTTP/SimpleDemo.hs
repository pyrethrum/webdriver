module HTTP.SimpleDemo where

import Effectful (Eff, IOE, (:>), Effect)
import HTTP.Runner (runHttpTest, testUrl)
import WebDriver.Effectful
  ( Logger,
    Pause,
    WebDriverHttp,
    log,
    pause,
  )
import WebDriver.Effectful.HTTP.Base.Actions
  ( elementSendKeys,
    findElement,
    getTitle,
    maximizeWindow,
    navigateTo,
  )
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as P
import WebDriverPreCore.Test.TestData (contentPageUrl, loginUrl)
import Prelude hiding (log)

-- get rid of warning
_runHttpTest :: (forall (es :: [Effect]).  (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) =>  Eff es ()) -> IO ()
_runHttpTest = runHttpTest

-- | HTTP-only demo:
--
--   * Navigates to the login page
--   * Fills in username and password via 'HTTP.elementSendKeys'
--   * Navigates to the colourful content page
--   * Gets and logs the page title
--
-- >>> runHttpTest http_login_navigation_demo
http_login_navigation_demo :: (Logger :> es, WebDriverHttp :> es, IOE :> es, Pause :> es) => Eff es ()
http_login_navigation_demo = do
  log "=== Navigate to login form ==="
  loginPage <- testUrl loginUrl
  navigateTo loginPage
  maximizeWindow
  pause

  log "=== Fill in username ==="
  usernameField <- findElement $ P.CSS "#username"
  elementSendKeys usernameField "demoUser"
  pause

  log "=== Fill in password ==="
  passwordField <- findElement $ P.CSS "#password"
  elementSendKeys passwordField "s3cr3tP4ssw0rd"
  pause

  log "=== Navigate to colourful content page ==="
  contentPage <- testUrl contentPageUrl
  navigateTo contentPage
  pause

  title <- getTitle
  log $ "Landed on: " <> title
