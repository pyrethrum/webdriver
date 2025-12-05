module Http.FallbackDemo where

import Data.Aeson as A (Value (..))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Text (Text)
import Http.DemoUtils (HttpDemo, runDemo, sessionDemo)
import Http.HttpActions (HttpActions (..))
import Http.HttpRunner (Extended (..))
import IOUtils (DemoActions (..))
import TestData (checkboxesUrl, textAreaUrl)
import WebDriverPreCore.Http.API qualified as A
import WebDriverPreCore.Http.Protocol
  ( Command (..),
    ElementId (..),
    SessionId (..),
    coerceCommand,
    extendPost,
    extendPostLoosen,
    loosenCommand,
    voidCommand,
  )
import WebDriverPreCore.Internal.Utils (UrlPath (..))
import Prelude hiding (log)

-- >>> runDemo demoFallbackActions
demoFallbackActions :: HttpDemo
demoFallbackActions =
  sessionDemo "fallback actions demo" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {runCommand, runCommand'} = do
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
      logShow "Navigate response" navigateResult.fullResponse
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

-- >>> runDemo demoFallbackCoercions
demoFallbackCoercions :: HttpDemo
demoFallbackCoercions =
  sessionDemo "fallback actions demo" action
  where
    action :: SessionId -> DemoActions -> HttpActions -> IO ()
    action sesId MkDemoActions {..} MkHttpActions {runCommand, runCommand'} = do
      let runAnyCommand :: forall a. Command a -> IO Value
          runAnyCommand = runCommand' . coerceCommand

      -- Navigate to another page using typed Navigate command and getResponse
      logTxt "Navigate to another page using typed command and getResponse (navigateTo returns Command ())"
      url2 <- textAreaUrl
      navigateResponse <- runCommand' . voidCommand $ A.navigateTo sesId url2
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
