module Http.ErrorDemo where

import Http.DemoUtils (HttpDemo, sessionDemo)
import Http.HttpActions (HttpActions (..))
import IOUtils (DemoActions (..), expectProtocolException)
import TestData (inputsUrl)
import WebDriverPreCore.Error (ErrorType (..))
import WebDriverPreCore.Http.Protocol (Selector (..), SessionId, Timeouts (..))

-- | Test error handling when clicking a non-existent element
errorDemo :: HttpDemo
errorDemo =
  sessionDemo "Http Error Demo" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      -- Set short timeouts
      setTimeouts sesId $
        MkTimeouts
          { pageLoad = Just 30_000,
            script = Just 11_000,
            implicit = Just 1_000
          }
      
      url <- inputsUrl
      navigateTo sesId url

      -- Try to find non-existent element and expect NoSuchElement error
      exc <- expectProtocolException NoSuchElement $
        findElement sesId $ CSS "#id-that-does-not-exist"
      
      logShow "Caught expected exception" exc
