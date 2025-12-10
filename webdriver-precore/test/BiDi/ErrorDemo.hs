module BiDi.ErrorDemo where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils (BiDiDemo, demo, runDemo)
import IOUtils (DemoActions (..), expectProtocolException)
import TestData (inputsUrl)
import WebDriverPreCore.BiDi.Protocol
  ( Close (..),
    Create (..),
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
    SourceActions (..)
  )
import WebDriverPreCore.Error (ErrorType (..))

-- stop warning for unused demo (its used in eval)
_rundemo :: BiDiDemo -> IO ()
_rundemo = runDemo

-- >>> runDemo errorDemo
-- *** Exception: Error executing BiDi command: With JSON: 
-- {
--     "id": 3,
--     "method": "input.performActions",
--     "params": {
--         "actions": [
--             {
--                 "actions": [
--                     {
--                         "duration": 100,
--                         "origin": {
--                             "element": {
--                                 "extensions": null,
--                                 "handle": null,
--                                 "sharedId": "invalid-shared-id-that-does-not-exist"
--                             },
--                             "type": "element"
--                         },
--                         "type": "pointerMove",
--                         "x": 0,
--                         "y": 0
--                     },
--                     {
--                         "button": 0,
--                         "type": "pointerDown"
--                     },
--                     {
--                         "button": 0,
--                         "type": "pointerUp"
--                     }
--                 ],
--                 "id": "mouse1",
--                 "parameters": {
--                     "pointerType": "mouse"
--                 },
--                 "type": "pointer"
--             }
--         ],
--         "context": "19451747-c641-4345-857f-2d65359eb611"
--     }
-- }
-- BiDi driver error: 
-- ProtocolException
--   { error = NoSuchNode
--   , description = "Tried to deserialize an unknown SharedReference"
--   , message =
--       "The node with the reference invalid-shared-id-that-does-not-exist is not known"
--   , stacktrace =
--       Just
--         "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nNoSuchNodeError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:717:5\ndeserializeSharedReference@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:224:11\ndeserialize@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:275:12\ndeserialize@chrome://remote/content/webdriver-bidi/modules/WindowGlobalBiDiModule.sys.mjs:42:17\n#deserializeElementSharedReference@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:36:26\n_getClientRects@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:127:66\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:282:33\nreceiveMessage@chrome://remote/content/shared/messagehandler/transports/js-window-actors/MessageHandlerFrameChild.sys.mjs:78:37\n"
--   , errorData = Nothing
--   , response =
--       Object
--         (fromList
--            [ ( "error" , String "no such node" )
--            , ( "id" , Number 3.0 )
--            , ( "message"
--              , String
--                  "The node with the reference invalid-shared-id-that-does-not-exist is not known"
--              )
--            , ( "stacktrace"
--              , String
--                  "RemoteError@chrome://remote/content/shared/RemoteError.sys.mjs:8:8\nWebDriverError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:202:5\nNoSuchNodeError@chrome://remote/content/shared/webdriver/Errors.sys.mjs:717:5\ndeserializeSharedReference@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:224:11\ndeserialize@chrome://remote/content/webdriver-bidi/RemoteValue.sys.mjs:275:12\ndeserialize@chrome://remote/content/webdriver-bidi/modules/WindowGlobalBiDiModule.sys.mjs:42:17\n#deserializeElementSharedReference@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:36:26\n_getClientRects@chrome://remote/content/webdriver-bidi/modules/windowglobal/input.sys.mjs:127:66\nhandleCommand@chrome://remote/content/shared/messagehandler/MessageHandler.sys.mjs:282:33\nreceiveMessage@chrome://remote/content/shared/messagehandler/transports/js-window-actors/MessageHandlerFrameChild.sys.mjs:78:37\n"
--              )
--            , ( "type" , String "error" )
--            ])
--   }
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
          
      !exc <- expectProtocolException NoSuchNode $ do
        !r <- inputPerformActions $
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
