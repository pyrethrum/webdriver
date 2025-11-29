module BiDi.Demos.NetworkSupportTest where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import Control.Exception (SomeException, throwIO, try)
import IOUtils (DemoActions (..), exceptionTextIncludes)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import Prelude hiding (log)

-- Test which network commands are actually supported by the current driver

-- >>> runDemo networkSupportTest
networkSupportTest :: BiDiDemo
networkSupportTest =
  demo "Network Support Test - Check which commands are implemented" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Testing network command support in current WebDriver implementation..."
      logTxt "Commands that fail with 'unknown command' are not yet implemented."
      pause

      -- Test 1: addDataCollector
      logTxt "Test 1: network.addDataCollector"
      result1 <-
        try $
          networkAddDataCollector $
            MkAddDataCollector
              { dataTypes = [Response],
                maxEncodedDataSize = MkJSUInt 1024,
                collectorType = Nothing,
                contexts = Nothing,
                userContexts = Nothing
              }
      handleCommandResult result1
      pause

      -- Test 2: addIntercept
      logTxt "Test 2: network.addIntercept"
      result2 <-
        try $
          networkAddIntercept $
            MkAddIntercept
              { phases = [BeforeRequestSent],
                contexts = Just [bc],
                urlPatterns = Nothing
              }
      handleCommandResult result2
      pause

      -- Test 3: setCacheBehavior
      logTxt "Test 3: network.setCacheBehavior"
      result3 <-
        try $
          networkSetCacheBehavior $
            MkSetCacheBehavior
              { cacheBehavior = DefaultCacheBehavior,
                contexts = Nothing
              }
      handleCommandResult result3
      pause

      -- Test 4: continueRequest (requires active intercept, so will fail with "no such request")
      logTxt "Test 4: network.continueRequest"
      result4 <-
        try $
          networkContinueRequest $
            MkContinueRequest
              { request = MkRequest "test-request-id",
                body = Nothing,
                cookies = Nothing,
                headers = Nothing,
                method = Nothing,
                url = Nothing
              }
      handleCommandResult result4
      pause

      logTxt "Support test completed."
      logTxt "❌ = Command not implemented in driver"
      logTxt "✅ = Command supported and works (or exists but needs active request)"
      logTxt "⚠️ = Command supported but failed for unexpected reason"
      where
        handleCommandResult :: Either SomeException a -> IO ()
        handleCommandResult = either handleError (const $ logTxt "✅ SUPPORTED")
          where
            handleError (e :: SomeException) =
              let errIncludes = any (flip exceptionTextIncludes e)
               in if
                    | errIncludes ["unknown command", "not supported"] ->
                        logShow "❌ NOT SUPPORTED (unknown command)" e
                    | errIncludes ["no such request", "not found"] ->
                        logTxt "✅ SUPPORTED (command exists but no active request - expected)"
                    | otherwise ->
                        do
                          logShow "⚠️ SUPPORTED but failed" e
                          throwIO e
