module BiDi.DemoUtils where

import BiDi.BiDiRunner (BiDiActions (..), mkDemoBiDiClientParams, withCommands)
import Const (Timeout (..), seconds)
import Control.Exception (Exception, catch, throwIO)
import Data.Text (Text, isInfixOf)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import IOUtils (DemoUtils (..), QueLog)
import Logger (withChannelFileLogger)
import WebDriverPreCore.BiDi.CoreTypes (StringValue (..))
import WebDriverPreCore.BiDi.Protocol
  ( BrowsingContext,
    Close (MkClose, context, promptUnload),
    ContextTarget (MkContextTarget, context, sandbox),
    Create
      ( MkCreate,
        background,
        createType,
        referenceContext,
        userContext
      ),
    CreateType (Tab, Window),
    Evaluate
      ( MkEvaluate,
        awaitPromise,
        expression,
        resultOwnership,
        serializationOptions,
        target
      ),
    GetTree (MkGetTree),
    GetTreeResult (MkGetTreeResult),
    Info (context),
    Target (ContextTarget),
  )
import WebDriverPreCore.BiDi.Script (EvaluateResult (..), PrimitiveProtocolValue (..), RemoteValue (..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

-- TODO: deprecate - move to config - rename to demoPause
demoPause :: Timeout
demoPause = 2 * seconds

data BiDiDemo = MkBiDiDemo
  { name :: Text,
    action :: DemoUtils -> BiDiActions -> IO ()
  }

demo :: Text -> (DemoUtils -> BiDiActions -> IO ()) -> BiDiDemo
demo name action = MkBiDiDemo {name, action}

runDemo' :: Maybe QueLog -> Timeout -> BiDiDemo -> IO ()
runDemo' mQueueLog pauseMs' d =
  mkDemoBiDiClientParams mQueueLog pauseMs' >>= flip withCommands d.action

runDemo :: BiDiDemo -> IO ()
runDemo demo' =
  withChannelFileLogger $ \queueLog ->
    runDemo' (Just queueLog) demoPause demo'

newWindowContext :: DemoUtils -> BiDiActions -> IO BrowsingContext
newWindowContext MkDemoUtils {..} MkBiDiActions {..} = do
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

closeContext :: DemoUtils -> BiDiActions -> BrowsingContext -> IO ()
closeContext MkDemoUtils {..} MkBiDiActions {..} bc = do
  logTxt "Close browsing context"
  co <- browsingContextClose $ MkClose {context = bc, promptUnload = Nothing}
  logShow "Close result" co
  pause

rootContext :: DemoUtils -> BiDiActions -> IO BrowsingContext
rootContext MkDemoUtils {..} MkBiDiActions {..} = do
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
chkDomContains' :: Timeout -> Timeout -> DemoUtils -> BiDiActions -> BrowsingContext -> Text -> IO ()
chkDomContains' timeout pause' MkDemoUtils {..} MkBiDiActions {..} bc expectedText = do
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
chkDomContains :: DemoUtils -> BiDiActions -> BrowsingContext -> Text -> IO ()
chkDomContains = chkDomContains' (10 * seconds) (MkTimeout 100)
