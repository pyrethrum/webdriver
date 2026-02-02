module ErrorDemo where

import Actions (BiDiActions (..))
import BiDiDemoUtils (BiDiDemo, demo, runDemo)
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit qualified as HUnit
import UnliftIO.Exception (try)
import WebDriverPreCore.Test.IOUtils (DemoActions (..), (===))
import WebDriverPreCore.Test.TestData (inputsUrl)

import WebDriverPreCore.BiDi.Protocol
  ( Create (..),
    CreateType (..),
    Navigate (..),
    Origin (..),
    PerformActions (..),
    Pointer (..),
    PointerCommonProperties (..),
    PointerSourceAction (..),
    PointerSourceActions (..),
    PointerType (..),
    ReadinessState (..),
    SharedId (..),
    SharedReference (..),
    SourceActions (..),
    ErrorType (..), 
    WebDriverException (..)
  )
import Prelude hiding (log)

-- stop warning for unused demo (its used in eval)
_rundemo :: BiDiDemo -> IO ()
_rundemo = runDemo

-- >>> runDemo errorDemo
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
      pause

      -- Try to click using an invalid SharedId and expect NoSuchNode error
      let invalidSharedId = MkSharedId "invalid-shared-id-that-does-not-exist"
          
      exc <- expectProtocolException NoSuchNode $ do
        r <- inputPerformActions $
          MkPerformActions
            { context = ctx,
              actions =
                [ PointerSourceActions $
                    MkPointerSourceActions
                      { pointerId = "mouse1",
                        pointer = Just $ MkPointer {pointerType = Just MousePointer},
                        pointerActions =
                          [ PointerMove
                              { x = 0,
                                y = 0,
                                duration = Just 100,
                                origin =
                                  Just $
                                    ElementOrigin $
                                      MkSharedReference
                                        { sharedId = invalidSharedId,
                                          handle = Nothing,
                                          extensions = Nothing
                                        },
                                pointerCommonProperties =
                                  MkPointerCommonProperties
                                    { width = Nothing,
                                      height = Nothing,
                                      pressure = Nothing,
                                      tangentialPressure = Nothing,
                                      twist = Nothing,
                                      altitudeAngle = Nothing,
                                      azimuthAngle = Nothing
                                    }
                              },
                            PointerDown
                              { button = 0,
                                pointerCommonProperties =
                                  MkPointerCommonProperties
                                    { width = Nothing,
                                      height = Nothing,
                                      pressure = Nothing,
                                      tangentialPressure = Nothing,
                                      twist = Nothing,
                                      altitudeAngle = Nothing,
                                      azimuthAngle = Nothing
                                    }
                              },
                            PointerUp
                              { button = 0
                              }
                          ]
                      }
                ]
            }
        pure r
      
      logShow "Caught expected exception" exc
      pause


expectProtocolException ::
  (HasCallStack) =>
  ErrorType ->
  IO a ->
  IO WebDriverException
expectProtocolException expectedError action =
  try action
    >>= \case
      Left exc@(ProtocolException {error = err}) -> do
        expectedError === err
        pure exc
      Left exc -> do
        HUnit.assertFailure $ "Expected ProtocolException but got: " <> show exc
      Right _ -> do
        HUnit.assertFailure $ "Expected ProtocolException with error " <> show expectedError <> " but action succeeded"
