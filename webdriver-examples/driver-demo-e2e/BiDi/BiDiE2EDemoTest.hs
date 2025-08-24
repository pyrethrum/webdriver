module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), withCommands)
import Control.Exception (finally)
import Data.Text (Text)
import IOUtils (DemoUtils (..), Logger (..), getLogger, sleepMs)
import WebDriverPreCore.BiDi.BiDiPath (parseUrl)
import WebDriverPreCore.BiDi.BrowsingContext
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

-- >>> runDemo newTab
newTab :: DemoUtils -> Commands -> IO ()
newTab MkDemoUtils {..} MkCommands {..} = do
  logTxt "About to open tab"
  let bcParams =
        MkCreate
          { createType = Tab,
            referenceContext = Nothing,
            background = Nothing,
            userContext = Nothing
          }
  bc <- browsingContextCreate bcParams
  logShow "Browsing Context (Tab)" bc
  sleep 1_000

  bcWin <- browsingContextCreate bcParams { createType = Window }
  logShow "Browsing Context (Window)" bcWin

  sleep 1_000
  pure ()
