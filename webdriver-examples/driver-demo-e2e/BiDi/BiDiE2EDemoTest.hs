module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), withCommands)
import Control.Exception (finally)
import Data.Text (Text)
import IOUtils (DemoUtils (..), Logger (..), getLogger, sleepMs)
import WebDriverPreCore.BiDi.BiDiPath (parseUrl)
import WebDriverPreCore.BiDi.Protocol (Create(..), CreateUserContext(..), CreateType(..))
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)


-- >>> demo_parseUrl
-- "Right\n  MkBiDiPath\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
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

bidiDemoUtils :: IO DemoUtils
bidiDemoUtils =
  do
    MkLogger {log = log', stop} <- getLogger
    let logTxt = log'
        log l t = logTxt $ l <> ": " <> t
        logShow :: forall a. (Show a) => Text -> a -> IO ()
        logShow l = log l . txt
        logM l mt = mt >>= log l
        logShowM :: forall a. (Show a) => Text -> IO a -> IO ()
        logShowM l t = t >>= logShow l
    pure $
      MkDemoUtils
        { sleep = sleepMs,
          logTxt,
          log,
          logShow,
          logM,
          logShowM,
          stopLogger = stop
        }

runExample :: DemoUtils -> (DemoUtils -> Commands -> IO ()) -> IO ()
runExample utils action =
  withCommands (Just utils.logTxt) $ action utils

runDemo :: (DemoUtils -> Commands -> IO ()) -> IO ()
runDemo action =
  do
    demoUtils <- bidiDemoUtils
    finally
      (runExample demoUtils action)
      (demoUtils.stopLogger)

-- >>> runDemo browsingContext
-- *** Exception: Error executing BiDi command: MkCommand
--   { method = "browser.createUserContext"
--   , params =
--       MkCreateUserContext
--         { acceptInsecureCerts = Nothing
--         , proxy = Nothing
--         , unhandledPromptBehavior = Nothing
--         }
--   , extended = Nothing
--   }
-- Failed to decode JSON returned by driver to response type: 
-- {
--     "id": 5,
--     "result": {
--         "userContext": "362bab4d-cca2-4d46-96e7-6029ddd8383d"
--     },
--     "type": "success"
-- }
browsingContext :: DemoUtils -> Commands -> IO ()
browsingContext MkDemoUtils {..} MkCommands {..} = do
  logTxt "New browsing context - Tab"
  let bcParams =
        MkCreate
          { createType = Tab,
            background = False,
            referenceContext = Nothing,
            userContext = Nothing
          }
      pause = sleep 3_000
  bc <- browsingContextCreate bcParams
  logShow "Browsing context - Tab" bc
  pause

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

  logTxt "New browsing context - Window with user context"
  uc <- browserCreateUserContext MkCreateUserContext { acceptInsecureCerts = Nothing,
    proxy = Nothing,
    unhandledPromptBehavior = Nothing
  }
  bcWinWithUC <-
    browsingContextCreate
      bcParams
        { createType = Window,
          userContext = Just uc
        }
  logShow "Browsing context - Window with user context" bcWinWithUC

  pure ()
