module ErrorDemo where

import BiDiActions (BiDiActions (..))
import BiDiDemoUtils (BiDiDemo, demo, runDemo)
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit qualified as HUnit
import UnliftIO.Exception (try)
import WebDriverPreCore.BiDiRunnerBase.Response (ResponseException (..))
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
    WebDriverException (..), 
    SharedReference(..),
    parseWebDriverException
  )
import Prelude hiding (log)

-- stop warning for unused demo (its used in eval)
_rundemo :: BiDiDemo -> IO ()
_rundemo = runDemo

-- >>> runDemo errorDemo
-- *** Exception: ProtocolException {error = NoSuchNode, description = "Tried to deserialize an unknown SharedReference", message = "The node with the reference invalid-shared-id-that-does-not-exist is not known", stacktrace = Just "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nNoSuchNodeError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:717:5\ndeserializeSharedReference@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:224:11\ndeserialize@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:275:12\ndeserialize@chrome://remote/content/webdriver-bidi/modules/WindowGlobalBiDiModule.sys.mjs:42:17\n#deserializeElementSharedReference@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:119:26\n_getClientRects@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:289:66\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:282:33\nreceiveMessage@chrome://remote/content/shared/messagehandler/transports/js-window-actors/MessageHandlerFrameChild.sys.mjs:78:37\n", errorData = Nothing, response = Object (fromList [("error",String "no such node"),("id",Number 3.0),("message",String "The node with the reference invalid-shared-id-that-does-not-exist is not known"),("stacktrace",String "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nNoSuchNodeError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:717:5\ndeserializeSharedReference@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:224:11\ndeserialize@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:275:12\ndeserialize@chrome://remote/content/webdriver-bidi/modules/WindowGlobalBiDiModule.sys.mjs:42:17\n#deserializeElementSharedReference@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:119:26\n_getClientRects@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:289:66\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:282:33\nreceiveMessage@chrome://remote/content/shared/messagehandler/transports/js-window-actors/MessageHandlerFrameChild.sys.mjs:78:37\n"),("type",String "error")])}
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
  IO ResponseException
expectProtocolException expectedError action =
  try action
    >>= \case
      Left exc@(BiDIError errorValue) -> do
        -- Parse the Value to get WebDriverException
        let parsedExc = parseWebDriverException "expectProtocolException" errorValue
        case parsedExc of
          ProtocolException {error = err} ->
            expectedError === err
              >> pure exc
          _ -> 
            HUnit.assertFailure $ "Expected ProtocolException but got: " <> show parsedExc
      Left exc -> do
        HUnit.assertFailure $ "Expected BiDIError but got: " <> show exc
      Right _ -> do
        HUnit.assertFailure $ "Expected ProtocolException with error " <> show expectedError <> " but action succeeded"
