module Http.FallbackDemo where

import Data.Aeson as A (Value (..))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Function ((&))
import Data.Text (Text)
import Http.DemoUtils (HttpDemo, runDemo, sessionDemo)
import Http.HttpActions (HttpActions (..))
import IOUtils (DemoActions (..))
import TestData (checkboxesUrl, textAreaUrl)
import WebDriverPreCore.HTTP.API qualified as A
import WebDriverPreCore.HTTP.Protocol
  ( Command (..),
    ElementId (..),
    SessionId (..),
    Timeouts (..),
    coerceCommand,
    extendPost,
    extendPostLoosen,
    loosenCommand,
    voidCommand,
  )
import Utils (UrlPath (..))
import Prelude hiding (log)

-- >>> runDemo demoFallbackActions
demoFallbackActions :: HttpDemo
demoFallbackActions =
  sessionDemo "fallback actions demo - manually construct commands" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {runCommand} = do
      -- Navigate to checkboxes page using runCommand and log the response
      url <- checkboxesUrl
      let navigateCmd =
            Post
              { description = "Navigate To (using fallback)",
                path = sessionUri1 sesId "url",
                body = A.fromList ["url" A..= url]
              }

      logTxt "Navigate using runCommand"
      -- as we haven't put a type signature on navigateCmd, we need to use @() to indicate we expect no return value
      navigateResult <- runCommand @() navigateCmd
      logShow "Navigate response" navigateResult
      pause

      -- Find the checkbox using Value command with runCommand
      let findElementCmd =
            Post
              { description = "Find Element (using fallback Value)",
                path = sessionUri1 sesId "element",
                body = A.fromList ["using" A..= ("css selector" :: String), "value" A..= ("#checkbox1" :: String)]
              }

      logTxt "Find checkbox element using Value command and runCommand"
      -- as we haven't put a type signature on findElementCmd, we need to use @Value to indicate we expect a Value return type
      val <- runCommand @Value findElementCmd
      logShow "Element search Result" val
      logShow "Full response" val
      pause

      -- Extract element ID from the Value result
      let checkboxId = case val of
            A.Object obj ->
              A.lookup "element-6066-11e4-a52e-4f735466cecf" obj
                & maybe
                  (error "Failed to extract element ID from response")
                  ( \case
                      A.String eid -> MkElement eid
                      _ -> error "Expected string for element ID"
                  )
            _ -> error "Expected object in response"

      logShow "Extracted element ID" checkboxId
      pause

newtype MyURL = MkMyURL {getMyURL :: Text}
  deriving (Show, Eq)
  deriving newtype (A.FromJSON)

-- >>> runDemo demoFallbackCoercions
demoFallbackCoercions :: HttpDemo
demoFallbackCoercions =
  sessionDemo "fallback coerce commands" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {runCommand} = do
      -- Navigate to another page using typed Navigate command and getResponse
      logTxt "Navigate to another page using typed command and getResponse (navigateTo returns Command ())"
      url2 <- textAreaUrl
      runCommand $ A.navigateTo sesId url2
      pause

      logTxt "Return a different type with compatible JSON (runCommand will return MyURL )"
      myUrl <- runCommand . coerceCommand @_ @MyURL $ A.getCurrentUrl sesId
      logShow "Current url - coerced" myUrl
      pause

      logTxt "Return Value rather than url"
      url' <- runCommand . loosenCommand $ A.getCurrentUrl sesId
      logShow "Current url - as Value" url'
      pause

      logTxt "Return void rather than url"
      urlVoid <- runCommand . voidCommand $ A.getCurrentUrl sesId
      logShow "Current url - voided" urlVoid
      pause

-- >>> runDemo demoExtendPost
demoExtendPost :: HttpDemo
demoExtendPost =
  sessionDemo "fallback extend Post commands demo" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {runCommand'} = do
      cbxsUrl <- checkboxesUrl
      runCommand' $ A.navigateTo sesId cbxsUrl
      
      -- Step 1: extendPost with a Post command
      let timeoutsCmd = A.setTimeouts sesId (MkTimeouts (Just 1000) Nothing Nothing)
          extended1 = A.fromList ["script" A..= (2000 :: Int)]
          extendedCmd1 = extendPost timeoutsCmd extended1
      logTxt "Extended Post Command" 
      logShowM "Extended Post Result" $ runCommand' extendedCmd1
      
      -- Step 2: extendPostLoosen with a Post command  
      let extended2 = A.fromList ["pageLoad" A..= (3000 :: Int)]
          extendedCmd2 = extendPostLoosen timeoutsCmd extended2
      logTxt "Extended Post (loosened) Command" 
      logShowM "Extended Post (loosened) Result" $ runCommand' extendedCmd2
      
      -- Step 3: extendPost with a PostEmpty command
      let backCmd = A.back sesId
          extended3 = A.fromList ["ignored" A..= String "test"]
          extendedCmd3 = extendPost backCmd extended3
      logTxt "Extended PostEmpty Command" 
      logShowM "Extended PostEmpty Result" $ runCommand' extendedCmd3
      
      -- Step 4: extendPostLoosen with a PostEmpty command
      let refreshCmd = A.refresh sesId
          extended4 = A.fromList ["alsoIgnored" A..= String "demo"]
          extendedCmd4 = extendPostLoosen refreshCmd extended4
      logTxt "Extended PostEmpty (loosened) Command" 
      logShowM "Extended PostEmpty (loosened) Result" $ runCommand' extendedCmd4
      
      pause

-- Helper functions (copied from API.hs since they're not exported)

sessionUri1 :: SessionId -> Text -> UrlPath
sessionUri1 s sp = MkUrlPath ["session", s.id, sp]

elementUri1 :: SessionId -> ElementId -> Text -> UrlPath
elementUri1 s er ep = MkUrlPath ["session", s.id, "element", er.id, ep]

_stopDemoUnusedWarning :: HttpDemo -> IO ()
_stopDemoUnusedWarning = runDemo
