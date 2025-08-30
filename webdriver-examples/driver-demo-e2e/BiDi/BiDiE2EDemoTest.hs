module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), withCommands)
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.Text (Text)
import IOUtils (DemoUtils (..), Logger (..), sleepMs)
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.Protocol (Create (..), CreateType (..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

-- >>> demo_parseUrl
-- "Right\n  MkBiDiUrl\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
demo_parseUrl :: Text
demo_parseUrl = txt $ parseUrl "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"

-- -- >>> unit_newBidiSessionViaHttp
-- unit_newBidiSessionViaHttp :: IO ()
-- unit_newBidiSessionViaHttp = do
--   cfg <- loadConfig
--   ses <- Http.newSessionFull $ httpBidiCapabilities cfg
--   finally
--     (logShow "new session response:\n" ses)
--     (Http.deleteSession ses.sessionId)


bidiDemoUtils :: Logger -> IO DemoUtils
bidiDemoUtils MkLogger {log = log'} =
  do
    let logTxt = log'
        log l t = logTxt $ l <> ": " <> t
        logShow :: forall a. (Show a) => Text -> a -> IO ()
        logShow l = log l . txt
    pure $
      MkDemoUtils
        { sleep = sleepMs,
          log,
          logShow,
          logM = \l mt -> mt >>= log l,
          logShowM = \l t -> t >>= logShow l,
          logTxt,
          pause = sleepMs pauseMs
        }


runExample :: (DemoUtils -> Commands -> IO ()) -> IO ()
runExample action =
  withCommands $ action 

-- runExample :: DemoUtils -> (DemoUtils -> Commands -> IO ()) -> IO ()
-- runExample utils action =
--   withCommands $ action utils

-- withDemoUtils :: (DemoUtils -> IO ()) -> IO ()
-- withDemoUtils action = withAsyncLogger $ (=<<) action . bidiDemoUtils

-- runDemo :: (DemoUtils -> Commands -> IO ()) -> IO ()
-- runDemo = flip runExample

runDemo :: (DemoUtils -> Commands -> IO ()) -> IO ()
runDemo = runExample


pauseMs :: Int
-- pauseMs = 3_000
pauseMs = 0

-- TODO: Session find out

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
  - wait empty logic is wrong ~hanging for 10 secs
  - mock failer for testin
  - Parameterisable logger
  - make serialisation strict
-}

-- >>> runDemo browsingContext1
-- *** Exception: Failed setting command parameters for method: browsingContext.activate
-- JSON Value must be of JSON type: Object
-- The actual JSON type was: String
-- The actual JSON value was: "d5d3cc21-099c-417b-97e0-371ec5a0a13a"
browsingContext1 :: DemoUtils -> Commands -> IO ()
browsingContext1 MkDemoUtils {..} MkCommands {..} = do
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
