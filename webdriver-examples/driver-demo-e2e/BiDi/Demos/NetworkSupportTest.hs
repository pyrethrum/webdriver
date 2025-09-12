module BiDi.Demos.NetworkSupportTest where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import Control.Exception (SomeException, try, Exception (displayException))
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Network qualified as Network
import Prelude hiding (log)
import Data.Text (pack, isInfixOf)

-- Test which network commands are actually supported by the current driver

-- >>> runDemo networkSupportTest
networkSupportTest :: BiDiDemo
networkSupportTest =
  demo "Network Support Test - Check which commands are implemented" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {..} cmds@MkCommands {..} = do
      bc <- rootContext utils cmds

      logTxt "Testing network command support in current WebDriver implementation..."
      logTxt "Commands that fail with 'unknown command' are not yet implemented."
      pause

      -- Test 1: addDataCollector
      logTxt "Test 1: network.addDataCollector"
      result1 <-
        try $
          networkAddDataCollector $
            Network.MkAddDataCollector
              { dataTypes = [Network.MkDataType "response"],
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
            Network.MkAddIntercept
              { phases = [Network.BeforeRequestSent],
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
            Network.MkSetCacheBehavior
              { cacheBehavior = Network.DefaultCacheBehavior,
                contexts = Nothing
              }
      handleCommandResult result3
      pause

      -- Test 4: continueRequest (requires active intercept, so will likely fail differently)
      logTxt "Test 4: network.continueRequest"
      result4 <-
        try $
          networkContinueRequest $
            Network.MkContinueRequest
              { request = Network.MkRequestId "test-request-id",
                body = Nothing,
                cookies = Nothing,
                headers = Nothing,
                method = Nothing,
                url = Nothing
              }
      handleCommandResult result4
      pause

      logTxt "Support test completed. Commands marked with ❌ are not implemented in the current driver."
      logTxt "Commands marked with ⚠️ are implemented but failed for other reasons (expected)."
      where
        handleCommandResult :: Either SomeException a -> IO ()
        handleCommandResult = either handleError (const $ logTxt "✅ SUPPORTED")
          where
            handleError (e :: SomeException) =
              let errTxt = pack $ displayException e
              in
              if "unknown command" `isInfixOf` errTxt
                then logShow "❌ NOT SUPPORTED (unknown command)" e
                else logShow "⚠️ SUPPORTED but failed" e
