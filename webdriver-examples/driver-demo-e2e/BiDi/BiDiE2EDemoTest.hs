module BiDi.BiDiE2EDemoTest where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiRunner (Commands (..), withCommands, mkDemoBiDiClientParams)
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


runDemo :: (DemoUtils -> Commands -> IO ()) -> IO ()
runDemo action =
  mkDemoBiDiClientParams pauseMs >>= \p -> withCommands p action


pauseMs :: Int
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
  - mock failer for testin
  - make serialisation strict
-}

-- >>> runDemo browsingContext1
-- *** Exception: Failed setting command parameters for method: browsingContext.activate
-- JSON Value must be of JSON type: Object
-- The actual JSON type was: String
-- The actual JSON value was: "f8635116-427f-4841-8f9a-b031ac2821c2"
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
