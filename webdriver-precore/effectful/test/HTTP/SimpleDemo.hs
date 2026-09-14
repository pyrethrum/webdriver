module HTTP.SimpleDemo where

import Effectful (Eff, IOE, (:>), Effect)
import HTTP.Runner (withHttp, testUrl)
import WebDriver.Effectful
  ( WaitPrimative,
    WebDriverHttp
  )
import WebDriver.Effectful.Logger (log, Logger)
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
_runHttpTest :: (forall (es :: [Effect]).  (IOE :> es, Logger :> es, WaitPrimative :> es, WebDriverHttp :> es) =>  Eff es ()) -> IO ()
_runHttpTest = withHttp

-- >>> runHttpTest http_login_navigation_demo
http_login_navigation_demo :: (Logger :> es, WebDriverHttp :> es, IOE :> es) => Eff es ()
http_login_navigation_demo = do
  log "=== Navigate to login form ==="
  loginPage <- testUrl loginUrl
  navigateTo loginPage
  maximizeWindow

  log "=== Fill in username ==="
  usernameField <- findElement $ P.CSS "#username"
  elementSendKeys usernameField "demoUser"

  log "=== Fill in password ==="
  passwordField <- findElement $ P.CSS "#password"
  elementSendKeys passwordField "s3cr3tP4ssw0rd"

  log "=== Navigate to colourful content page ==="
  contentPage <- testUrl contentPageUrl
  navigateTo contentPage

  title <- getTitle
  log $ "Landed on: " <> title
