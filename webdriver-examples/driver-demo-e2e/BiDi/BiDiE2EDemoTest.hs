module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), mkDemoBiDiClientParams, withCommands, mkFailBidiClientParams)
import Data.Text (Text)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.Protocol (Create (..), CreateType (..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)
import Data.Word (Word64)

-- >>> demo_parseUrl
-- "Right\n  MkBiDiUrl\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
demo_parseUrl :: Text
demo_parseUrl = txt $ parseUrl "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"

runDemo :: BiDiDemo -> IO ()
runDemo d =
  mkDemoBiDiClientParams pauseMs >>= \p -> withCommands p d.action

runFailDemo :: BiDiDemo -> Word64 -> Word64 -> Word64 -> IO ()
runFailDemo d failSendCount failGetCount failPrintCount = do
  mkFailBidiClientParams pauseMs failSendCount failGetCount failPrintCount >>= \p -> withCommands p d.action

-- example fail demos :: to be turned into tests later
sendFailDemo :: BiDiDemo -> IO ()
sendFailDemo d = runFailDemo d 2 0 0

getFailDemo :: BiDiDemo -> IO ()
getFailDemo d = runFailDemo d 0 2 0

printFailDemo :: BiDiDemo -> IO ()
printFailDemo d = runFailDemo d 0 0 3


pauseMs :: Int
pauseMs = 0

data BiDiDemo = MkBiDiDemo
  { name :: Text,
    action :: DemoUtils -> Commands -> IO ()
  }

demo :: Text -> (DemoUtils -> Commands -> IO ()) -> BiDiDemo
demo name action = MkBiDiDemo {name, action}

-- TODO: Session find out about newSession Firefox threads

{-
##### BrowsingContext #####

4. browsingContextCreate
1. browsingContextActivate
2. browsingContextCaptureScreenshot
3. browsingContextClose
5. browsingContextGetTree
6. browsingContextHandleUserPrompt
7. browsingContextLocateNodes
8. browsingContextNavigate
9. browsingContextPrint
10. browsingContextReload
11. browsingContextSetViewport
12. browsingContextTraverseHistory

-}

{-
TODO:
  - mock failer for testin
  - make serialisation strict
-}

-- Failure Demos :: TODO turn into tests ---

-- >>> printFailDemo browsingContext1
-- *** Exception: Forced failure for testing: print (call #3)

-- >>> getFailDemo browsingContext1
-- *** Exception: Forced failure for testing: get (call #2)

-- >>> sendFailDemo browsingContext1
-- *** Exception: Forced failure for testing: send (call #2)

-- >>> runDemo browsingContext1
-- *** Exception: Failed setting command parameters for method: browsingContext.activate
-- JSON Value must be of JSON type: Object
-- The actual JSON type was: String
-- The actual JSON value was: "b5362963-c1ed-41e4-af74-4603395ed51e"

-- *** Exception: Failed setting command parameters for method: browsingContext.activate

-- JSON Value must be of JSON type: Object
-- The actual JSON type was: String
-- The actual JSON value was: "7a68d552-033a-4018-a4dd-3214d9ca7a20"
browsingContext1 :: BiDiDemo
browsingContext1 =
  demo "Browsing Context 1" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action MkDemoUtils {..} MkCommands {..} = do
      logTxt "New browsing context - Tab"
      let bcParams =
            MkCreate
              { createType = Tab,
                background = False,
                referenceContext = Nothing,
                userContext = Nothing
              }
      bc <- browsingContextCreate bcParams
      logShow "Browsing context - Tab" bc
      pause

      {-

      logTxt "New browsing context - Window"
      bcWin <- browsingContextCreate bcParams {createType = Window}
      logShow "Browsing context - Window" bcWin
      pause

      logTxt "New browsing context - Tab with reference context"
      bcWithContext <- browsingContextCreate bcParams {referenceContext = Just bc}
      logShow "Browsing context - Tab with reference context" bcWithContext
      pause

      logTxt "New browsing context - Background"
      bg <-
        browsingContextCreate
          bcParams
            { background = True,
              referenceContext = Just bcWin
            }
      logShow "Background browsing context created on front window" bg
      pause

      logTxt "New user context"
      uc <-
        browserCreateUserContext
          MkCreateUserContext
            { acceptInsecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User context created" uc
      pause

      logTxt "New browsing context - Window with user context"
      bcWinWithUC <-
        browsingContextCreate
          bcParams
            { createType = Window,
              userContext = Just uc
            }
      logShow "Browsing context - Window with user context" bcWinWithUC
      pause

      -}
      logTxt "Activate initial browsing context"
      o <- browsingContextActivate bc
      logShow "Activate result" o
      pause

      pure ()
