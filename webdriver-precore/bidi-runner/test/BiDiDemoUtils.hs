{-|
BiDi Demo utilities module for webdriver-precore-bidi-runner tests

This module provides utilities for running BiDi demos.
-}
module BiDiDemoUtils
  ( BiDiDemo (..),
    demo,
    runDemo,
    runDemoWithConfig,
    runDemoFail,
    runDemoFail',
    httpBidiCapabilities,
    -- Helper functions
    rootContext,
    newWindowContext,
    closeContext,
    chkDomContains,
    chkDomContains',
    TextValidationError (..),
    expectError,
    expectErrorText,
    FailTest (..),
    toText,
    toLambda,
  )
where

import Actions (Actions (..))
import Actions qualified
import Control.Exception (Exception, SomeException, bracket, catch, throwIO, try)
import Data.Text (Text, isInfixOf, unpack)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Word (Word64)
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext,
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
    Target (..),
  )
import WebDriverPreCore.BiDiRunner (BiDiUrl, parseBiDiUrl, withBiDi, withBiDiFailTest)
import WebDriverPreCore.HTTP.Protocol (FullCapabilities (..), SessionResponse (..))
import WebDriverPreCore.HTTP.Protocol qualified as Caps (Capabilities (..))
import WebDriverPreCore.HttpRunner (mkHttpRunner)
import WebDriverPreCore.HttpRunner.Actions (HttpActions (..), mkActions)
import WebDriverPreCore.Test.Config (Config (..))
import WebDriverPreCore.Test.ConfigLoader (loadConfig)
import WebDriverPreCore.Test.Const (Timeout (..), milliseconds, seconds)
import WebDriverPreCore.Test.IOUtils (DemoActions (..), Logger (..), logNothingLogger, mkDemoActions)
import WebDriverPreCore.Test.Logger (withChannelFileLogger)
import WebDriverPreCore.Test.RuntimeConst (httpCapabilities, httpFullCapabilities)
import Utils (txt)
import Prelude hiding (log)

-- | A BiDi demo is a named action that runs with DemoActions and a Actions
data BiDiDemo = MkBiDiDemo
  { name :: Text,
    action :: DemoActions -> Actions -> IO ()
  }

-- | Create a BiDi demo
demo :: Text -> (DemoActions -> Actions -> IO ()) -> BiDiDemo
demo name action = MkBiDiDemo {name, action}

-- | BiDi capabilities are HTTP capabilities with webSocketUrl enabled
httpBidiCapabilities :: Config -> FullCapabilities
httpBidiCapabilities cfg =
  (httpFullCapabilities cfg)
    { alwaysMatch =
        Just $ (httpCapabilities cfg) {Caps.webSocketUrl = Just True}
    }

-- | Extract BiDi URL from session response
getBiDiUrl :: SessionResponse -> Either Text BiDiUrl
getBiDiUrl r =
  case r.webSocketUrl of
    Nothing -> Left $ "WebSocket URL not provided in session response:\n" <> txt r
    Just wsUrl -> 
      case parseBiDiUrl wsUrl of
        Nothing -> Left $ "Could not parse WebSocket URL: " <> wsUrl
        Just bidiUrl -> Right bidiUrl

-- | Run a BiDi demo with the default config
runDemo :: BiDiDemo -> IO ()
runDemo dmo = loadConfig >>= flip runDemoWithConfig dmo

-- | Run a BiDi demo with a specific config
runDemoWithConfig :: Config -> BiDiDemo -> IO ()
runDemoWithConfig cfg demo' = do
  if cfg.logging
    then
      withChannelFileLogger runWithLogger
    else
      runWithLogger logNothingLogger
  where
    runWithLogger :: Logger -> IO ()
    runWithLogger logger = do
      let demoActions = mkDemoActions logger $ fromIntegral cfg.pauseMS * milliseconds
          mLogger = if cfg.logging then Just logger.log else Nothing
          httpRunner = mkHttpRunner cfg.httpUrl (fromIntegral cfg.httpPort) mLogger
          httpActions = mkActions httpRunner
          httpCaps = httpBidiCapabilities cfg
      
      -- Create HTTP session first (BiDi needs webSocketUrl from session response)
      bracket
        (httpActions.newSession httpCaps)
        (httpActions.deleteSession . (.sessionId))
        $ \ses -> do
          -- Parse the BiDi URL from the session response
          bidiUrl <- case getBiDiUrl ses of
            Left err -> fail $ show err
            Right url -> pure url
          
          -- Run with BiDi connection
          withBiDi mLogger bidiUrl $ \biDiRunner -> do
            let bidiActions = Actions.mkActions biDiRunner
            demoActions.logTxt $ "Executing: " <> demo'.name
            demo'.action demoActions bidiActions

-- | Get root browsing context
rootContext :: DemoActions -> Actions -> IO BrowsingContext
rootContext MkDemoActions {..} MkActions {..} = do
  logTxt "Get root browsing context"
  tree <- browsingContextGetTree $ MkGetTree Nothing Nothing
  logShow "Browsing context tree" tree
  case tree of
    MkGetTreeResult (info : _) -> pure $ info.context
    _ -> error "No browsing contexts found"

-- | Create a new window context
newWindowContext :: DemoActions -> Actions -> IO BrowsingContext
newWindowContext MkDemoActions {..} MkActions {..} = do
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

-- | Close a browsing context
closeContext :: DemoActions -> Actions -> BrowsingContext -> IO ()
closeContext MkDemoActions {pause, logTxt, logShow} MkActions {..} bc = do
  logTxt "Close browsing context"
  co <- browsingContextClose $ MkClose {context = bc, promptUnload = Nothing}
  logShow "Close result" co
  pause

-- | Custom exception for text validation failures
data TextValidationError = MkTextValidationError
  { message :: Text,
    expectedText :: Text,
    actualText :: Text
  }
  deriving (Show)

instance Exception TextValidationError

-- | Check if expected text is present in DOM with timeout and retry, throw error if not found
chkDomContains' :: Timeout -> Timeout -> DemoActions -> Actions -> BrowsingContext -> Text -> IO ()
chkDomContains' timeout pause' MkDemoActions {..} MkActions {..} bc expectedText = do
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
chkDomContains :: DemoActions -> Actions -> BrowsingContext -> Text -> IO ()
chkDomContains = chkDomContains' (10 * seconds) (MkTimeout 100)

-- | Test specification for expected error
data FailTest
  = Predicate (Text -> Bool)
  | Fragment Text

toLambda :: FailTest -> (Text -> Bool)
toLambda = \case
  Predicate f -> f
  Fragment t -> \errText -> t `T.isInfixOf` errText

toText :: FailTest -> Text
toText (Fragment t) = t
toText (Predicate _) = "<custom lambda>"

-- | Test that an IO action throws an exception containing expected text fragment
expectErrorText :: Text -> Text -> IO () -> IO ()
expectErrorText testName expectedFragment =
  expectError testName (Fragment expectedFragment)

-- | General function to test that an IO action throws an exception matching the FailTest
expectError :: Text -> FailTest -> IO () -> IO ()
expectError testName failTest action = do
  result <- try action
  case result of
    Left (e :: SomeException) -> do
      let errText = txt $ show e
      if toLambda failTest errText
        then pure ()
        else
          fail . unpack $
            testName
              <> ": Error did not contain expected fragment."
              <> "\n"
              <> " Expected Fragment was: "
              <> "\n"
              <> toText failTest 
              <> "\n"
              <> "Actual Error was:"
              <> "\n"
              <> errText
    Right _ ->
      fail . unpack $
        testName <> ": Expected error but action succeeded"

-- | Run a BiDi demo with failure injection for testing
runDemoFail :: Word64 -> Word64 -> Word64 -> BiDiDemo -> IO ()
runDemoFail failSendCount failGetCount failEventCount dmo = 
  loadConfig >>= \c -> runDemoFail' c failSendCount failGetCount failEventCount dmo

-- | Run a BiDi demo with failure injection for testing (with config)
runDemoFail' :: Config -> Word64 -> Word64 -> Word64 -> BiDiDemo -> IO ()
runDemoFail' cfg failSendCount failGetCount failEventCount demo' = do
  if cfg.logging
    then withChannelFileLogger runWithLogger
    else runWithLogger logNothingLogger
  where
    runWithLogger :: Logger -> IO ()
    runWithLogger logger = do
      let demoActions = mkDemoActions logger $ fromIntegral cfg.pauseMS * milliseconds
          mLogger = if cfg.logging then Just logger.log else Nothing
          httpRunner = mkHttpRunner cfg.httpUrl (fromIntegral cfg.httpPort) mLogger
          httpActions = mkActions httpRunner
          httpCaps = httpBidiCapabilities cfg
      
      bracket
        (httpActions.newSession httpCaps)
        (httpActions.deleteSession . (.sessionId))
        $ \ses -> do
          bidiUrl <- case getBiDiUrl ses of
            Left err -> fail $ show err
            Right url -> pure url
          
          -- Run with BiDi connection with failure injection
          withBiDiFailTest failSendCount failGetCount failEventCount mLogger bidiUrl $ \biDiRunner -> do
            let bidiActions = Actions.mkActions biDiRunner
            demoActions.logTxt $ "Executing (with failures): " <> demo'.name
            demo'.action demoActions bidiActions
