module HTTP.SimpleDemo where

import Effectful (Eff, IOE, liftIO, (:>))
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions qualified as HTTP
import WebDriverPreCore.Extended.HTTP.Base.Protocol qualified as P
import WebDriverPreCore.Test.CapabilitiesBuilder (httpCapabilities)
import WebDriverPreCore.Test.ConfigLoader (Config (..), loadConfig)
import WebDriverPreCore.Test.TestData (contentPageUrl, loginUrl)
import WebDriverPreCore.Utils.Timeout (milliseconds)
import Prelude hiding (log)

runSetup
  :: (forall es. (IOE :> es) => HttpDriverInfo -> InteractBehaviour -> Config -> Eff es a)
  -> IO a
runSetup action = runHttp $ do
  config <- liftIO loadConfig
  let behaviour = mkInteractBehaviour config
      driverInfo =
        MkHttpDriverInfo
          { httpEndpoint = MkHttpEndpoint {host = config.httpUrl, port = config.httpPort},
            driverLogFn  = Nothing
          }
  action driverInfo behaviour config

mkInteractBehaviour :: Config -> InteractBehaviour
mkInteractBehaviour config =
  MkInteractBehaviour
    { pauseDuration = fromIntegral config.pauseMS * milliseconds,
      driverLogging = config.logging
    }

mkHttpCaps :: Config -> HttpCapabilities
mkHttpCaps config =
  MkFullCapabilities
    { alwaysMatch = Just . fromHttpCapability $ httpCapabilities config,
      firstMatch  = []
    }

runHttpTest
  :: ( forall es
      . ( IOE :> es
        , Logger :> es
        , Pause :> es
        , WebDriverHttp :> es
        )
     => Eff es ()
     )
  -> IO ()
runHttpTest action =
  runSetup $ \driverInfo behaviour config ->
    withLogger "eval.log" $
      withHttpSession driverInfo behaviour (mkHttpCaps config) $
        withPause behaviour.pauseDuration action

-- | HTTP-only demo:
--
--   * Navigates to the login page
--   * Fills in username and password via 'HTTP.elementSendKeys'
--   * Navigates to the colourful content page
--   * Gets and logs the page title
--
-- >>> http_login_navigation_demo
http_login_navigation_demo :: IO ()
http_login_navigation_demo = runHttpTest $ do
  log "=== Navigate to login form ==="
  loginPage <- liftIO loginUrl
  HTTP.navigateTo loginPage
  HTTP.maximizeWindow
  pause

  log "=== Fill in username ==="
  usernameField <- HTTP.findElement $ P.CSS "#username"
  HTTP.elementSendKeys usernameField "demoUser"
  pause

  log "=== Fill in password ==="
  passwordField <- HTTP.findElement $ P.CSS "#password"
  HTTP.elementSendKeys passwordField "s3cr3tP4ssw0rd"
  pause

  log "=== Navigate to colourful content page ==="
  contentPage <- liftIO contentPageUrl
  HTTP.navigateTo contentPage
  pause

  title <- HTTP.getTitle
  log $ "Landed on: " <> title
