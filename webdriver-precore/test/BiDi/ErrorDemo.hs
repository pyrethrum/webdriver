module BiDi.ErrorDemo where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils (BiDiDemo, demo)
import IOUtils (DemoActions (..), expectProtocolException)
import TestData (inputsUrl)
import WebDriverPreCore.BiDi.Protocol (Close (..), Create (..), CreateType (..), Locator (..), LocateNodes (..), Navigate (..), ReadinessState (..))
import WebDriverPreCore.Error (ErrorType (..))

-- | Test error handling when locating a non-existent element
errorDemo :: BiDiDemo
errorDemo =
  demo "BiDi Error Demo" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action MkDemoActions {..} MkBiDiActions {..} = do
      url <- inputsUrl
      
      -- Create browsing context and navigate
      ctx <- browsingContextCreate MkCreate {createType = Tab, background = False, userContext = Nothing, referenceContext = Nothing}
      browsingContextNavigate $ MkNavigate {context = ctx, url, wait = Just Interactive}

      -- Try to locate non-existent element and expect NoSuchElement error
      exc <- expectProtocolException NoSuchElement $
        browsingContextLocateNodes $
          MkLocateNodes
            { context = ctx,
              locator = CSS {value = "#id-that-does-not-exist"},
              maxNodeCount = Nothing,
              serializationOptions = Nothing,
              startNodes = Nothing
            }
      
      logShow "Caught expected exception" exc
      
      -- Clean up
      browsingContextClose $ MkClose {context = ctx, promptUnload = Nothing}
