module BiDi.BiDiE2EDemoTest where

import BiDi.BiDiRunner
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, assertBool, (@=?))
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.Internal.Utils
import Prelude hiding (putStrLn)
import Data.Text.IO (putStrLn)


-- >>> unit_sessionNew
-- *** Exception: user error (New Session
-- Failed to parse response:
--  parsing WebDriverPreCore.BiDi.Session.ProxyConfiguration failed, expected Object with key "tag" containing one of ["AutodetectProxyConfiguration","DirectProxyConfiguration","ManualProxyConfiguration","PacProxyConfiguration","SystemProxyConfiguration"], key "tag" not found
-- in response:NotAnError {httpResponse = MkHttpResponse {statusCode = 200, statusMessage = "OK", body = Object (fromList [("value",Object (fromList [("capabilities",Object (fromList [("acceptInsecureCerts",Bool False),("browserName",String "firefox"),("browserVersion",String "137.0.2"),("moz:accessibilityChecks",Bool False),("moz:buildID",String "20250414091429"),("moz:geckodriverVersion",String "0.36.0"),("moz:headless",Bool False),("moz:platformVersion",String "6.11.0-25-generic"),("moz:processID",Number 15883.0),("moz:profile",String "/tmp/rust_mozprofileW5nh1t"),("moz:shutdownTimeout",Number 60000.0),("moz:webdriverClick",Bool True),("moz:windowless",Bool False),("pageLoadStrategy",String "normal"),("platformName",String "linux"),("proxy",Object (fromList [])),("setWindowRect",Bool True),("strictFileInteractability",Bool False),("timeouts",Object (fromList [("implicit",Number 0.0),("pageLoad",Number 300000.0),("script",Number 30000.0)])),("unhandledPromptBehavior",String "dismiss and notify"),("userAgent",String "Mozilla/5.0 (X11; Linux x86_64; rv:137.0) Gecko/20100101 Firefox/137.0"),("webSocketUrl",String "ws://127.0.0.1:9222/session/62ce1eb4-efd6-4194-9494-b60fccc614ab")])),("sessionId",String "62ce1eb4-efd6-4194-9494-b60fccc614ab")]))])}})
unit_sessionNew :: IO ()
unit_sessionNew = do
  ses <- newSession firefoxCapabilities
  putStrLn $ txt ses

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
