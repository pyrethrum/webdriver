module HTTP.BaseLocateTest where

import Common.BaseLocateTest 
import Common.SessionInit (WDSession (..), closeWDSession, getWDSession)
import Common.TastyEval qualified as TE
import Common.Utils (defHttpOpts)
import Common.WebDriver.Effect (WebDriver)
import Common.WebDriver.InterpreterHttp qualified as Interp
import Data.Text (Text, unpack)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Error.Dynamic (Error, runErrorNoCallStack)
import Test.Tasty (TestTree, inOrderTestGroup, withResource)
import UnliftIO (throwIO)
import WebDriver.Effectful.HTTP.Core
    ( WebDriverHttp, runWebDriverHttp )
import WebDriver.Effectful.WaitPrimative
    ( WaitPrimative, runWaitPrimative )
import WebDriver.Effectful.HTTP.Base.Interpreter (HttpParams (..))
import WebDriver.Effectful.Logger (Logger, runLogger)
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)
import WebDriverPreCore.Extended.Locate qualified as L
import Prelude

import Common.Utils (beforeAll_)
import Common.WebDriver.Effect
  ( locate,
    navigateTo,
  )
import Common.WebDriver.TestUtils (locateChkAutoId)
import Common.WebDriver.TestUtils qualified as TU
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase)
import WebDriverPreCore.Extended.Locators as LS
import WebDriverPreCore.Test.TestData
import WebDriverPreCore.Utils.Utils (txt)

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
        [ testLandmarkAndRole run (interp defHttpOpts),
          testExtendedRoleMatchingNever run (interp defHttpOpts),
          testExtendedRoleMatchingAlways
            run
            (interp (defHttpOpts {L.extendedRoleLocation = L.ExtLocateAlways})),
          testExtendedRoleMatchingMiss True
            run
            (interp (defHttpOpts {L.extendedRoleLocation = L.ExtLocateSingletonMiss})),
          testExtendedRoleMatchingRoleType
            run
            (interp defHttpOpts)
            (interp (defHttpOpts {L.extendedRoleLocation = L.ExtLocateAlways})),
          testVisibilityAlways run (interp defHttpOpts),
          testVisibilityNever
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever})),
          testVisibilityDisambiguate
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckDisambiguateUnique})),
          testVisibilityDisambiguateVsNever
            run
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckDisambiguateUnique}))
            (interp (defHttpOpts {L.jsRecheckDisplayed = L.DisplayedCheckNever})),
          testBasicLocatorTypes run (interp defHttpOpts),
          testClassLocatorVariants run (interp defHttpOpts),
          testAttributeLocatorVariants run (interp defHttpOpts),
          testRoleNameAndRole run (interp defHttpOpts),
          testLocateFromElement run (interp defHttpOpts),
          testCombinedLocators run (interp defHttpOpts),
          testMiscAriaRoleTypes run (interp defHttpOpts),
          testMiscAriaRoleTypesNever
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
      testViewportDisplayCheck (runWith ses) (interpHttp defHttpOpts)

-- ---------------------------------------------------------------------------
-- Extended Role Matching Tests - HTTP Specific
-- ---------------------------------------------------------------------------

testExtendedRoleMatchingNever ::
  forall elm es.
  (Show elm, Error Text :> es) =>
  (forall a. Eff es a -> IO a) ->
  (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
  TestTree
testExtendedRoleMatchingNever run interpNever =
  beforeAll_ (extendedRolesUrl >>= \url -> run (interpNever (navigateTo @elm url))) $
    testGroup
      "Extended Role Matching Tests - ExtLocateNever"
      [ testGroup
          "aria-labelledby resolution"
          [ testNever "ExtLocateNever - locate does NOT find region via aria-labelledby" $ do
              locRslt <- locate @elm $ region "Personal Information"
              TU.chkLocException (txt (region "Personal Information")) isNotFound locRslt,
            chkElmCountNever "ExtLocateNever - locateAll does NOT find region via aria-labelledby" (region "Personal Information") 0
          ],
        testGroup
          "for id label association"
          [ testNever "ExtLocateNever - locate does NOT find radio via for id label" $ do
              locRslt <- locate @elm $ radio "Email"
              TU.chkLocException (txt (radio "Email")) isNotFound locRslt,
            testNever "ExtLocateNever - locate does NOT find textbox via for id label" $ do
              locRslt <- locate @elm $ textbox "Given Name"
              TU.chkLocException (txt (textbox "Given Name")) isNotFound locRslt
          ],
        testGroup
          "aria-label - always resolved regardless of setting"
          [ chkAutoId "ExtLocateNever finds textbox with aria-label" (textbox "Nickname") "edt-nickname"
          ]
      ]
  where
    testNever :: Text -> Eff (WebDriver elm : es) () -> TestTree
    testNever name act = testCase (unpack name) (run (interpNever act))

    chkAutoId :: Text -> Locator -> Text -> TestTree
    chkAutoId name loc expctd = testNever name (locateChkAutoId @elm loc expctd)

    chkElmCountNever :: Text -> Locator -> Int -> TestTree
    chkElmCountNever = chkElmCount' run interpNever

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
