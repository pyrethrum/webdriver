module BiDi.Demos.OtherDemos where

import BiDi.BiDiRunner (mkDemoBiDiClientParams, mkFailBidiClientParams, withCommands)
import BiDi.DemoUtils
import Data.Text (Text)
import Data.Word (Word64)
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

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

-- ###########################################################################

-- Failure Demos :: TODO turn into tests ---

-- >>> printFailDemo browsingContextCreateActivateClose

-- *** Exception: Forced failure for testing: print (call #3)

-- >>> getFailDemo browsingContextCreateActivateClose

-- *** Exception: Forced failure for testing: get (call #2)

-- >>> sendFailDemo browsingContextCreateActivateClose

-- *** Exception: Forced failure for testing: send (call #2)