module BiDi.DemoUtils where

import BiDi.BiDiActions (BiDiActions (..), mkActions)
import BiDi.Runner (withBiDi, withBidiFailTest)
import BiDi.BiDiSocket (SocketActions)
import Config (Config (..))
import ConfigLoader (loadConfig)
import Const (Timeout (..), milliseconds, seconds)
import Control.Exception (Exception, catch, throwIO, SomeException, try)
import Data.Function ((&))
import Data.Text (Text, isInfixOf, unpack)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word64)
import Http.DemoUtils (withSession)
import Http.HttpActions qualified as HTTPA
import Http.HttpRunner (mkRunner)
import IOUtils (DemoActions (..), Logger, logNothingLogger, mkDemoActions)
import Logger (withChannelFileLogger)
import Network.HTTP.Req (http)
import RuntimeConst (httpCapabilities, httpFullCapabilities)
import BiDi.BiDiUrl (BiDiUrl, getBiDiUrl)
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext (..),
    Close (..),
    ContextTarget (..),
    Create (..),
    CreateType (..),
    Evaluate (..),
    EvaluateResult (..),
    GetTree (..),
    GetTreeResult (..),
    Info (..),
    PrimitiveProtocolValue (..),
    RemoteValue (..),
    StringValue (..),
    Target (..)
  )
import WebDriverPreCore.HTTP.Protocol (FullCapabilities (..))
import WebDriverPreCore.HTTP.Protocol qualified as Caps (Capabilities (..))
import Utils (txt)
import Prelude hiding (log, putStrLn)
import qualified Data.Text as T

data BiDiDemo = MkBiDiDemo
  { name :: Text,
    action :: DemoActions -> BiDiActions -> IO ()
  }

demo :: Text -> (DemoActions -> BiDiActions -> IO ()) -> BiDiDemo
demo name action = MkBiDiDemo {name, action}

-- -- Bidi capabilities request is the same as regular HTTP capabilities,
-- -- but with the `webSocketUrl` field set to `True`
httpBidiCapabilities :: Config -> FullCapabilities
httpBidiCapabilities cfg =
  (httpFullCapabilities cfg)
    { alwaysMatch =
        Just $ (httpCapabilities cfg) {Caps.webSocketUrl = Just True}
    }

type Runner = DemoActions -> BiDiUrl -> (DemoActions -> SocketActions -> IO ()) -> IO ()

runDemo' :: Config -> BiDiDemo -> IO ()
runDemo' = runDemo'' withBiDi

runDemo'' :: Runner -> Config -> BiDiDemo -> IO ()
runDemo''
  bidiRunner
  cfg@MkConfig
    { httpUrl,
      httpPort,
      logging,
      pauseMS
    }
  demo' = do
    if logging
      then
        withChannelFileLogger runWithLogger
      else
        runWithLogger logNothingLogger
    where
      httpCaps = httpBidiCapabilities cfg
      runWithLogger :: Logger -> IO ()
      runWithLogger logger =
        let demoActions = mkDemoActions logger $ fromIntegral pauseMS * milliseconds
            httpRunner = mkRunner (http httpUrl) (fromIntegral httpPort) demoActions
            httpActions = HTTPA.mkActions httpRunner
         in withSession httpCaps httpActions $ \ses -> do
              bidiUrl <- getBiDiUrl ses & either (fail . unpack) pure
              bidiRunner demoActions bidiUrl $ \_ socketActions -> do
                let bidiActions = mkActions socketActions
                demoActions.logTxt $ "Executing: " <> demo'.name
                demo'.action demoActions bidiActions

runDemo :: BiDiDemo -> IO ()
runDemo dmo = loadConfig >>= flip runDemo' dmo

runDemoFail :: Word64 -> Word64 -> Word64 -> BiDiDemo -> IO ()
runDemoFail failSendCount failGetCount failEventCount dmo = loadConfig >>= \c -> runDemoFail' c failSendCount failGetCount failEventCount dmo

runDemoFail' :: Config -> Word64 -> Word64 -> Word64 -> BiDiDemo -> IO ()
runDemoFail' config failSendCount failGetCount failEventCount dmo = runDemo'' (withBidiFailTest failSendCount failGetCount failEventCount) config dmo


newWindowContext :: DemoActions -> BiDiActions -> IO BrowsingContext
newWindowContext MkDemoActions {..} MkBiDiActions {..} = do
  logTxt "New browsing context - Window"
  bcWin <- browsingContextCreate bcParams {createType = Window}
  logShow "Browsing context - Window" bcWin
  pause
  pure bcWin
  where
    bcParams =
      MkCreate
        { createType = Tab,
          background = False,
          referenceContext = Nothing,
          userContext = Nothing
        }

closeContext :: DemoActions -> BiDiActions -> BrowsingContext -> IO ()
closeContext MkDemoActions {pause, logTxt, logShow} MkBiDiActions {..} bc = do
  logTxt "Close browsing context"
  co <- browsingContextClose $ MkClose {context = bc, promptUnload = Nothing}
  logShow "Close result" co
  pause

rootContext :: DemoActions -> BiDiActions -> IO BrowsingContext
rootContext MkDemoActions {..} MkBiDiActions {..} = do
  logTxt "Get root browsing context"
  tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
  logShow "Browsing context tree" tree
  case tree of
    MkGetTreeResult (info : _) -> pure $ info.context
    _ -> error "No browsing contexts found"

-- | Custom exception for text validation failures
data TextValidationError = MkTextValidationError
  { message :: Text,
    expectedText :: Text,
    actualText :: Text
  }
  deriving (Show)

instance Exception TextValidationError

-- | Check if expected text is present in DOM with timeout and retry, throw error if not found
chkDomContains' :: Timeout -> Timeout -> DemoActions -> BiDiActions -> BrowsingContext -> Text -> IO ()
chkDomContains' timeout pause' MkDemoActions {..} MkBiDiActions {..} bc expectedText = do
  startTime <- getPOSIXTime
  logTxt $ "Checking DOM contains: " <> expectedText <> " (timeout: " <> txt timeout <> "ms, pause: " <> txt pause' <> "ms)"
  checkLoop $ startTime + (fromIntegral timeout.microseconds / 1000000)
  where
    checkLoop :: POSIXTime -> IO ()
    checkLoop endTime = do
      currentTime <- getPOSIXTime
      if currentTime > endTime
        then do
          throwIO $
            MkTextValidationError
              { message = "✗ Timeout reached! Expected text not found after " <> txt timeout <> "ms",
                expectedText,
                actualText = ""
              }
        else do
          result <-
            (validateDomText >> pure ()) `catch` \(_ :: TextValidationError) -> do
              pauseAtLeast pause'
              checkLoop endTime
          pure result

    validateDomText :: IO ()
    validateDomText = do
      -- Get the full DOM text content
      domResult <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domResult of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue actualText))} -> do
          if expectedText `isInfixOf` actualText
            then logTxt $ "✓ Found expected text: " <> expectedText
            else do
              throwIO $
                MkTextValidationError
                  { message = "✗ Expected text not in DOM",
                    expectedText,
                    actualText
                  }
        EvaluateResultSuccess {result = otherResult} -> do
          throwIO $
            MkTextValidationError
              { message = "Unexpected result type: " <> txt otherResult,
                expectedText,
                actualText = "Non-string result"
              }
        EvaluateResultException {exceptionDetails} -> do
          throwIO $
            MkTextValidationError
              { message = "✗ Script evaluation failed",
                expectedText,
                actualText = txt exceptionDetails
              }

-- | Check if expected text is present in DOM with default timeout and retry settings
chkDomContains :: DemoActions -> BiDiActions -> BrowsingContext -> Text -> IO ()
chkDomContains = chkDomContains' (10 * seconds) (MkTimeout 100)




-- | General function to test that an IO action throws an exception containing expected text
expectError :: Text -> Text -> IO () -> IO ()
expectError testName errorFragment action = do
  result <- try action
  case result of
    Left (e :: SomeException) -> do
      let errText = txt $ show e
      if errorFragment `T.isInfixOf` errText
        then pure ()
        else
          fail . unpack $
            testName
              <> ": Error did not contain expected fragment."
              <> "\n"
              <> " Expected Fragment was: "
              <> "\n"
              <>  errorFragment
              <> "\n"
              <> "Actual Error was:"
              <> "\n"
              <> errText
    Right _ ->
      fail $ unpack $ testName <> ": Expected error, but action completed successfully."
