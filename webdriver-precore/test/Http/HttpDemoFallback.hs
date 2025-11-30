module Http.HttpDemoFallback where

import Data.Aeson as A (Value (..))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Text (Text)
import Http.DemoUtils (HttpDemo, sessionDemo, runDemo)
import Http.HttpActions (HttpActions (..))
import Http.HttpRunner (Extended (..))
import IOUtils (DemoActions (..))
import TestData (checkboxesUrl, textAreaUrl)
import WebDriverPreCore.Http.API qualified as A
import WebDriverPreCore.Http.Command ()
import WebDriverPreCore.Http.Protocol
  ( Command (..),
    ElementId (..),
    SessionId (..),
  )
import WebDriverPreCore.Internal.Utils (UrlPath (..))
import Prelude hiding (log)
import WebDriverPreCore.Http.Command (voidCommand)


-- >>> runDemo demoFallbackActions
-- *** Exception: runCommand not implemented in legacy actions
demoFallbackActions :: HttpDemo
demoFallbackActions =
  sessionDemo "fallback actions demo" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {..} = do
      -- Step 1: Navigate to checkboxes page using runCommand and log the response
      url <- checkboxesUrl
      let navigateCmd =
            Post
              { description = "Navigate To (using fallback)",
                path = sessionUri1 sesId "url",
                body = A.object ["url" A..= url]
              }

      logTxt "Step 1: Navigate using runCommand"
      navigateResult <- runCommand (navigateCmd :: Command ())
      logShow "Navigate response" navigateResult.fullResponse
      pause

      -- Step 2: Find the checkbox using Value command with runCommand
      let findElementCmd =
            Post
              { description = "Find Element (using fallback Value)",
                path = sessionUri1 sesId "element",
                body = A.object ["using" A..= ("css selector" :: String), "value" A..= ("#checkbox1" :: String)]
              }

      logTxt "Step 2: Find checkbox element using Value command and runCommand"
      elementResult <- runCommand @Value findElementCmd
      logShow "Element search result" elementResult.parsed
      logShow "Full response" elementResult.fullResponse
      pause

      -- Extract element ID from the Value result
      let checkboxId = case elementResult.parsed of
            A.Object obj -> case A.lookup "element-6066-11e4-a52e-4f735466cecf" obj of
              Just (A.String eid) -> MkElement eid
              _ -> error "Failed to extract element ID from response"
            _ -> error "Expected object in response"

      logShow "Extracted element ID" checkboxId
      pause

      -- Step 3: Click the checkbox using Value command and getResponse

      let clickCmd :: Command ()
          clickCmd =
            Post
              { description = "Element Click (using fallback)",
                path = elementUri1 sesId checkboxId "click",
                body = A.object []
              }

      logTxt "\nStep 3: Click checkbox using Value command and getResponse"
      clickResponse <- runCommand' clickCmd
      logShow "Click response" clickResponse
      pause

      let 
        -- type system gets a bit tricky when using record of functions this work around
        -- allows us to get an HttpResponse from a (Command a) for any a
        runAnyCommand :: forall a. Command a -> IO Value
        runAnyCommand = runCommand' . voidCommand

      -- Step 4: Navigate to another page using typed Navigate command and getResponse
      logTxt "Step 4: Navigate to another page using typed command and getResponse (navigateTo returns Command ())"
      url2 <- textAreaUrl
      navigateResponse <- runAnyCommand $ A.navigateTo sesId  url2
      logShow "Typed navigate response" navigateResponse
      pause

      url' <- runAnyCommand $ A.getCurrentUrl sesId
      logShow "Current url" url'
      pause



-- Helper functions (copied from API.hs since they're not exported)

sessionUri1 :: SessionId -> Text -> UrlPath
sessionUri1 s sp = MkUrlPath ["session", s.id, sp]

elementUri1 :: SessionId -> ElementId -> Text -> UrlPath
elementUri1 s er ep = MkUrlPath ["session", s.id, "element", er.id, ep]



_stopDemoUnusedWarning :: HttpDemo -> IO ()
_stopDemoUnusedWarning = runDemo



