module BiDi.Demos.OtherDemos where

import BiDi.BiDiRunner 
import BiDi.DemoUtils
import IOUtils
import Data.Text (Text)
import Data.Word (Word64)
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

-- low level demos -  TODO turn into tests ---

-- >>> parseUrlDemo
-- "Right\n  MkBiDiUrl\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
parseUrlDemo :: Text
parseUrlDemo = txt $ parseUrl "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"


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



-- >>> printFailDemo dummyDemo
-- *** Exception: Forced failure for testing: print (call #3)


-- >>> getFailDemo dummyDemo
-- *** Exception: Forced failure for testing: get (call #2)


-- >>> sendFailDemo dummyDemo
-- *** Exception: Forced failure for testing: send (call #2)


-- >>> runDemo dummyDemo
dummyDemo :: BiDiDemo
dummyDemo =
  demo "Browsing Context - Capture Screenshot, Close" action
  where
    action :: DemoUtils -> Commands -> IO ()
    action utils@MkDemoUtils {pause} cmds = do
      rootContext utils cmds
      pause

