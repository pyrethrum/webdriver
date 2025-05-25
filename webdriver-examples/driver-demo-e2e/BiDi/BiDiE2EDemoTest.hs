module BiDi.BiDiE2EDemoTest where

import BiDi.BiDiRunner
import Test.Tasty.HUnit as HUnit (Assertion, HasCallStack, assertBool, (@=?))
import WebDriverPreCore.BiDi.Session
import WebDriverPreCore.Internal.Utils
import Prelude hiding (putStrLn)
import Data.Text.IO (putStrLn)

firefoxCapabilities :: Capabilities
firefoxCapabilities =
  MkCapabilities
    { alwaysMatch = Just firefoxCapability,
      firstMatch = Nothing
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

-- >>> unit_sessionNew
-- *** Exception: user error (WebDriver error thrown:
--  WebDriverError {error = InvalidArgument, description = "The arguments passed to a command are either invalid or malformed", httpResponse = MkHttpResponse {statusCode = 400, statusMessage = "Bad Request", body = Object (fromList [("value",Object (fromList [("error",String "invalid argument"),("message",String "missing field `capabilities`"),("stacktrace",String "")]))])}})
unit_sessionNew :: IO ()
unit_sessionNew = do
  ses <- newSession firefoxCapabilities
  putStrLn $ txt ses
