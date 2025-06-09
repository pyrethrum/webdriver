module BiDi.BiDiE2EDemoTest where

import WebDriverPreCore.BiDi.Session
import Prelude hiding (putStrLn)
import Http.HttpAPI (deleteSession, SessionResponse(..), newSessionFull')
import E2EConst (httpFullCapabilities)
import IOUtils (logShow)



-- >>> unit_demoNewSession
unit_demoNewSessionViaHttp :: IO ()
unit_demoNewSessionViaHttp = do
  ses <- newSessionFull' httpFullCapabilities
  logShow "new session response:\n" ses
  deleteSession ses.sessionId


-- >>> unit_sessionNew
-- *** Exception: user error (New Session
-- Failed to parse response:
--  parsing WebDriverPreCore.BiDi.Session.ProxyConfiguration failed, expected Object with key "tag" containing one of ["AutodetectProxyConfiguration","DirectProxyConfiguration","ManualProxyConfiguration","PacProxyConfiguration","SystemProxyConfiguration"], key "tag" not found
-- in response:NotAnError {httpResponse = MkHttpResponse {statusCode = 200, statusMessage = "OK", body = Object (fromList [("value",Object (fromList [("capabilities",Object (fromList [("acceptInsecureCerts",Bool False),("browserName",String "firefox"),("browserVersion",String "137.0.2"),("moz:accessibilityChecks",Bool False),("moz:buildID",String "20250414091429"),("moz:geckodriverVersion",String "0.36.0"),("moz:headless",Bool False),("moz:platformVersion",String "6.11.0-25-generic"),("moz:processID",Number 15645.0),("moz:profile",String "/tmp/rust_mozprofile0Qk6Xy"),("moz:shutdownTimeout",Number 60000.0),("moz:webdriverClick",Bool True),("moz:windowless",Bool False),("pageLoadStrategy",String "normal"),("platformName",String "linux"),("proxy",Object (fromList [])),("setWindowRect",Bool True),("strictFileInteractability",Bool False),("timeouts",Object (fromList [("implicit",Number 0.0),("pageLoad",Number 300000.0),("script",Number 30000.0)])),("unhandledPromptBehavior",String "dismiss and notify"),("userAgent",String "Mozilla/5.0 (X11; Linux x86_64; rv:137.0) Gecko/20100101 Firefox/137.0"),("webSocketUrl",String "ws://127.0.0.1:9222/session/306034e3-57c8-46d9-bbc8-a0709ab1e2c5")])),("sessionId",String "306034e3-57c8-46d9-bbc8-a0709ab1e2c5")]))])}})
-- sessionNew :: IO ()
-- sessionNew = do
--   ses <- newSession firefoxCapabilities
--   putStrLn $ txt ses

firefoxCapabilities :: Capabilities
firefoxCapabilities =
  MkCapabilities
    { alwaysMatch = Just firefoxCapability,
      firstMatch = []
    }

firefoxCapability :: Capability
firefoxCapability =
  MkCapability
    { browserName = Just "firefox",
      browserVersion = Nothing,
      -- Always true for BiDi
      webSocketUrl = True,
      acceptInsecureCerts = Nothing,
      platformName = Nothing,
      proxy = Nothing,
      unhandledPromptBehavior = Nothing
    }
