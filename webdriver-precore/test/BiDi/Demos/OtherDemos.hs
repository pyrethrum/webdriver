module BiDi.Demos.OtherDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import Config (Config, loadConfig)
import Data.Text (Text)
import IOUtils (DemoActions (..))
import TestData (navigation1Url, navigation2Url)
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.Protocol
  ( KnownSubscriptionType (BrowsingContextNavigationStarted),
    Navigate (MkNavigate, context, url, wait),
    ReadinessState (Complete),
  )
import WebDriverPreCore.Internal.Utils (txt)
import Prelude hiding (log, putStrLn)

-- low level demos -  TODO turn into tests ---

-- >>> parseUrlDemo
-- "Right\n  MkBiDiUrl\n    { host = \"127.0.0.1\"\n    , port = 9222\n    , path = \"/session/e43698d9-b02a-4284-a936-12041deb3552\"\n    }"
parseUrlDemo :: Text
parseUrlDemo = txt $ parseUrl "ws://127.0.0.1:9222/session/e43698d9-b02a-4284-a936-12041deb3552"


-- Check expected errors when rigged to fail

-- >>> evalFail getFailDemo
getFailDemo :: Config -> IO ()
getFailDemo c = expectError "getfail" "Forced failure for testing: get" $ runDemoFail' c 0 2 0 failDemo

-- >>> evalFail sendFailDemo
sendFailDemo :: Config -> IO ()
sendFailDemo c = expectError "sendfail" "Forced failure for testing: send" $ runDemoFail' c 2 0 0 failDemo

-- >>> evalFail eventFailDemo
eventFailDemo :: Config -> IO ()
eventFailDemo c = expectError "eventfail" "Forced failure for testing: eventhandler (call #2)" $ runDemoFail' c 0 0 2 failDemo

evalFail :: (Config -> IO ()) -> IO ()
evalFail failAction = do
  c <- loadConfig
  failAction c

-- >>> runDemo dummyDemo
failDemo :: BiDiDemo
failDemo =
  demo "Fail demo" action
  where
    action :: DemoActions -> BiDiActions -> IO ()
    action utils@MkDemoActions {pause, logTxt, logShow, timeLimitLog} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Subscribe to navigation started events"
      (startedEventFired, waitStartedEventFired) <- timeLimitLog BrowsingContextNavigationStarted
      subscribeBrowsingContextNavigationStarted startedEventFired

      logTxt "Navigate to navigation1.html"
      nav1 <- navigation1Url
      navResult1 <- browsingContextNavigate $ MkNavigate {context = bc, url = nav1, wait = Just Complete}
      logShow "Navigation result 1" navResult1
      pause

      logTxt "Navigate to navigation2.html"
      nav2 <- navigation2Url
      navResult2 <- browsingContextNavigate $ MkNavigate {context = bc, url = nav2, wait = Just Complete}
      logShow "Navigation result 2" navResult2
      pause

      waitStartedEventFired
