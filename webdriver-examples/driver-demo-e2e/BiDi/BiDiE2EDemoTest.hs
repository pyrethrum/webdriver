module BiDi.BiDiE2EDemoTest where


import Config (loadConfig)
import Control.Exception (finally)
-- custom import needed to disambiguate capabilities
import Http.HttpAPI qualified as Http
import IOUtils (DemoUtils (..), demoUtils)
import Prelude hiding (putStrLn, log)
import BiDi.BiDiRunner (httpBidiCapabilities, Commands(..), withCommands)
import Data.Text (Text)
import WebDriverPreCore.Internal.Utils (txt)
import WebDriverPreCore.BiDi.BiDiPath (parseUrl)
import WebDriverPreCore.BiDi.BrowsingContext


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


runExample :: DemoUtils -> (DemoUtils -> Commands -> IO ()) -> IO ()
runExample utils action = withCommands $ action utils

runDemo :: (DemoUtils -> Commands -> IO ()) -> IO ()
runDemo = runExample demoUtils



-- >>> demoNewTab
-- *** Exception: JSONParseError
--   (fromList
--      [ ( "id" , Number 1.0 )
--      , ( "result"
--        , Object
--            (fromList
--               [ ( "context" , String "bbe1d55b-88d7-4e00-b4f2-d20478af3bb8" ) ])
--        )
--      , ( "type" , String "success" )
--      ])
demoNewTab :: IO ()
demoNewTab = runDemo newTab

newTab :: DemoUtils -> Commands -> IO ()
newTab MkDemoUtils{..} MkCommands {..} = do
  logTxt ""
  bc <- browsingContextCreate $ MkCreate Tab Nothing Nothing Nothing
  logShow "Browsing Context" bc
  sleep 3_000
  pure ()
  
