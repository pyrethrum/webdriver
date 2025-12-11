module Http.ErrorDemo where

import GHC.Utils.Misc (HasCallStack)
import Http.DemoUtils (HttpDemo, runDemo, sessionDemo)
import Http.HttpActions (HttpActions (..))
import IOUtils (DemoActions (..), (===))
import TestData (inputsUrl)
import WebDriverPreCore.Error (ErrorType (..), WebDriverException (..))
import WebDriverPreCore.Http.Protocol (Selector (..), SessionId, Timeouts (..))
import Prelude hiding (log)
import UnliftIO (try)
import Test.Tasty.HUnit (assertFailure)

-- stop warning for unused demo (its used in eval)
_rundemo :: HttpDemo -> IO ()
_rundemo = runDemo

--  >>> runDemo errorDemo
errorDemo :: HttpDemo
errorDemo =
  sessionDemo "Http Error Demo" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      -- Set short timeouts
      setTimeouts sesId $
        MkTimeouts
          { pageLoad = Just 0,
            script = Just 0,
            implicit = Just 0
          }

      url <- inputsUrl
      navigateTo sesId url

      -- Try to find non-existent element and expect NoSuchElement error
      exc <-
        expectProtocolException NoSuchElement .
          findElement sesId $
            CSS "#id-that-does-not-exist"

      logShow "Caught expected exception" exc


expectProtocolException ::
  (HasCallStack) =>
  ErrorType ->
  IO a ->
  IO WebDriverException
expectProtocolException expectedError action =
  try action
    >>= \case
      Left exc@(ProtocolException {error = err}) ->
        expectedError === err
          >> pure exc
      Left exc -> do
        assertFailure $ "Expected ProtocolException but got: " <> show exc
      Right _ -> do
        assertFailure $ "Expected ProtocolException with error " <> show expectedError <> " but action succeeded"
