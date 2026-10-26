module HTTP.BaseLocateTest where

import Common.BaseLocateTest qualified as Common
import Common.SessionInit (WDSession (..), closeWDSession, getWDSession)
import Common.TastyEval qualified as TE
import Common.Utils (defHttpOpts)
import Common.WebDriver.Effect (WebDriver)
import Common.WebDriver.InterpreterHttp qualified as Interp
import Data.Text (Text, unpack)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Test.Tasty (TestTree, inOrderTestGroup, withResource)
import UnliftIO (throwIO)
import WebDriver.Effectful
  ( WaitPrimative,
    WebDriverHttp,
    runWaitPrimative,
    runWebDriverHttp,
  )
import WebDriver.Effectful.HTTP.Base.Interpreter (HttpParams (..))
import WebDriver.Effectful.Logger (Logger, runLogger)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)
import WebDriverPreCore.Extended.Locate qualified as L
import Prelude

-- | Base effect stack for HTTP tests, with the dynamic 'Error' effect first.
type HttpBaseEffs = '[Error Text, WebDriverHttp, Logger, WaitPrimative, IOE]

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  inOrderTestGroup
    "Base Locate Test Suite"
    [ withResource (getWDSession False) closeWDSession viewportTests,
      withResource (getWDSession False) closeWDSession runSessionTests
    ]
  where
    runWith :: IO WDSession -> forall a. Eff HttpBaseEffs a -> IO a
    runWith ses act = do
      MkWDSession {loggerEnv, session, endpoint} <- ses
      res <-
        runEff
          $ runWaitPrimative
          $ runLogger loggerEnv
          $ runWebDriverHttp (MkHttpParams {session = session, endpoint = endpoint})
          $ runErrorNoCallStack @Text act
      case res of
        Left err -> throwIO (userError (unpack err))
        Right a -> pure a

    runSessionTests :: IO WDSession -> TestTree
    runSessionTests ses =
      inOrderTestGroup
        "Base Locate Tests"
        [ Common.testLandmarkAndRole run (interp defHttpOpts),
          Common.testExtendedRoleMatchingNever run (interp defHttpOpts),
          Common.testExtendedRoleMatchingAlways
            run
            (interp (defHttpOpts {L.extendedRoleLocation = L.ExtLocateAlways})),
          Common.testExtendedRoleMatchingMiss
            run
            (interp (defHttpOpts {L.extendedRoleLocation = L.ExtLocateSingletonMiss})),
          Common.testExtendedRoleMatchingRoleType
            run
            (interp defHttpOpts)
            (interp (defHttpOpts {L.extendedRoleLocation = L.ExtLocateAlways})),
          Common.testVisibilityAlways run (interp defHttpOpts),
          Common.testVisibilityNever
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever})),
          Common.testVisibilityDisambiguate
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckDisambiguateUnique})),
          Common.testVisibilityDisambiguateVsNever
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckDisambiguateUnique}))
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever})),
          Common.testBasicLocatorTypes run (interp defHttpOpts),
          Common.testClassLocatorVariants run (interp defHttpOpts),
          Common.testAttributeLocatorVariants run (interp defHttpOpts),
          Common.testRoleNameAndRole run (interp defHttpOpts),
          Common.testLocateFromElement run (interp defHttpOpts),
          Common.testCombinedLocators run (interp defHttpOpts),
          Common.testMiscAriaRoleTypes run (interp defHttpOpts),
          Common.testMiscAriaRoleTypesNever
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever}))
        ]
      where
        run :: forall a. Eff HttpBaseEffs a -> IO a
        run = runWith ses

        interp :: L.HttpLocateOpts -> forall a. Eff (WebDriver ElementId : HttpBaseEffs) a -> Eff HttpBaseEffs a
        interp = interpHttp

    interpHttp :: L.HttpLocateOpts -> forall a. Eff (WebDriver ElementId : HttpBaseEffs) a -> Eff HttpBaseEffs a
    interpHttp opts = Interp.runWebDriverHttp opts

    viewportTests :: IO WDSession -> TestTree
    viewportTests ses =
      Common.testViewportDisplayCheck (runWith ses) (interpHttp defHttpOpts)

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOutOnFailure

_pattern :: Maybe Text
_pattern = Just "ExtLocateAlways finds textbox with aria-label"

-- Specific test
--- >>> _eval _pattern tests

-- *** Exception: ExitSuccess

-- All tests
--- >>> _eval Nothing tests

-- *** Exception: ExitSuccess
