module BiDi.Demos.OtherDemos where

import BiDi.BiDiRunner (Commands (..), mkDemoBiDiClientParams, mkFailBidiClientParams, withCommands)
import BiDi.DemoUtils
import Data.Aeson (ToJSON (..), Value (Null), object, (.=))
import Data.Text (Text)
import Data.Word (Word64)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.BiDiUrl (parseUrl)
import WebDriverPreCore.BiDi.BrowsingContext (Locator (..), PrintMargin (..), PrintPage (..), Viewport (..))
import WebDriverPreCore.BiDi.CoreTypes (JSInt (..), JSUInt (..), NodeRemoteValue (..), SharedId (..))
import WebDriverPreCore.BiDi.Protocol
  ( Activate (MkActivate),
    BrowsingContext,
    CaptureScreenshot
      ( MkCaptureScreenshot,
        clip,
        context,
        format,
        origin
      ),
    ClipRectangle (BoxClipRectangle, height, width, x, y),
    Close (MkClose, context, promptUnload),
    ContextTarget (MkContextTarget, context, sandbox),
    Create
      ( MkCreate,
        background,
        createType,
        referenceContext,
        userContext
      ),
    CreateType (Tab, Window),
    CreateUserContext
      ( MkCreateUserContext,
        acceptInsecureCerts,
        proxy,
        unhandledPromptBehavior
      ),
    Evaluate
      ( MkEvaluate,
        awaitPromise,
        expression,
        resultOwnership,
        serializationOptions,
        target
      ),
    GetTree (MkGetTree, maxDepth, root),
    GetTreeResult (MkGetTreeResult),
    HandleUserPrompt (MkHandleUserPrompt, accept, context, userText),
    ImageFormat (MkImageFormat, imageType, quality),
    Info (context),
    LocateNodes
      ( MkLocateNodes,
        context,
        locator,
        maxNodeCount,
        serializationOptions,
        startNodes
      ),
    LocateNodesResult (MkLocateNodesResult),
    Navigate (MkNavigate, context, url, wait),
    Orientation (Landscape, Portrait),
    PageRange (Page, Range, fromPage, toPage),
    Print
      ( MkPrint,
        background,
        context,
        margin,
        orientation,
        page,
        pageRanges,
        scale,
        shrinkToFit
      ),
    PrintResult (MkPrintResult, base64Text),
    ReadinessState (Complete, Interactive, None),
    Reload (MkReload, context, ignoreCache, wait),
    RemoveUserContext (MkRemoveUserContext),
    ScreenShotOrigin (Document, Viewport),
    SetViewport
      ( MkSetViewport,
        context,
        devicePixelRatio,
        userContexts,
        viewport
      ),
    SharedId (MkShareId, id),
    SharedReference (MkSharedReference, extensions, handle, sharedId),
    TraverseHistory (MkTraverseHistory, context, delta),
  )
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