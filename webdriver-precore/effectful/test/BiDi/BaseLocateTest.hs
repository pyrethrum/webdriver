module BiDi.BaseLocateTest where

import BiDi.Runner
  ( BiDiLocateEffs,
    BiDiResources,
    interpBiDiLocate,
    runBiDiLocate,
    withBiDiResources,
  )
import Common.BaseLocateTest
  ( testAttributeLocatorVariants,
    testBasicLocatorTypes,
    testClassLocatorVariants,
    testCombinedLocators,
    testExtendedRoleMatchingAlways,
    testExtendedRoleMatchingMiss,
    testExtendedRoleMatchingNever,
    testExtendedRoleMatchingRoleType,
    testLandmarkAndRole,
    testLocateFromElement,
    testMiscAriaRoleTypes,
    testMiscAriaRoleTypesNever,
    testRoleNameAndRole,
    testViewportDisplayCheck,
    testVisibilityAlways,
    testVisibilityDisambiguate,
    testVisibilityDisambiguateVsNever,
    testVisibilityNever,
  )
import Common.TastyEval qualified as TE
import Common.WebDriver.Effect (WebDriver)
import Data.Text (Text)
import Effectful (Eff, (:>))
import Effectful.Error.Dynamic (Error)
import Test.Tasty (TestTree, inOrderTestGroup)
import WebDriverPreCore.BiDi.Protocol (NodeRemoteValue)
import Prelude

-- >>> _eval tests

-- *** Exception: ExitSuccess

tests :: TestTree
tests =
  inOrderTestGroup
    "Base Locate Test Suite"
    [ withBiDiResources viewportTests,
      withBiDiResources runSessionTests
    ]
  where
    viewportTests :: IO BiDiResources -> TestTree
    viewportTests res =
      testViewportDisplayCheck (runBiDiLocate res) interpBiDiLocate

    runSessionTests :: IO BiDiResources -> TestTree
    runSessionTests res =
      inOrderTestGroup
        "Base Locate Tests"
        [ runInprt testLandmarkAndRole,
          runInprt testExtendedRoleMatchingNever,
          runInprt testExtendedRoleMatchingAlways,
          runInprt testExtendedRoleMatchingMiss,
          runInprt testVisibilityAlways,
          runInprt testVisibilityNever,
          runInprt testVisibilityDisambiguate,
          runInprt testBasicLocatorTypes,
          runInprt testClassLocatorVariants,
          runInprt testAttributeLocatorVariants,
          runInprt testRoleNameAndRole,
          runInprt testLocateFromElement,
          runInprt testCombinedLocators,
          runInprt testMiscAriaRoleTypes,
          runInprt testMiscAriaRoleTypesNever,
          testExtendedRoleMatchingRoleType run interp interp,
          testVisibilityDisambiguateVsNever run interp interp
        ]
      where
        runInprt ::
          ( forall elm es.
            (Eq elm, Show elm, Error Text :> es) =>
            (forall a. Eff es a -> IO a) ->
            (forall a. Eff (WebDriver elm : es) a -> Eff es a) ->
            TestTree
          ) ->
          TestTree
        runInprt test = test run interp

        run :: forall a. Eff BiDiLocateEffs a -> IO a
        run = runBiDiLocate res

        interp :: forall a. Eff (WebDriver NodeRemoteValue : BiDiLocateEffs) a -> Eff BiDiLocateEffs a
        interp = interpBiDiLocate



arggh :: IO TE.TastyResult
arggh = TE.tastyEvalStdOut (Just "/Landmark and Role Tests/&&/Img - div with explicit role override/") tests

_eval :: Maybe Text -> TestTree -> IO TE.TastyResult
_eval = TE.tastyEvalStdOut

_pattern :: Maybe Text
_pattern = Just "/Landmark and Role Tests/"


--- >>> arggh
-- Using debug local config
-- Base Locate Test Suite
--   Base Locate Tests
--     Landmark and Role Tests
--       Role Locator Tests
--         Role with name - aria-label
--           Img - div with explicit role override: FAIL
--             Exception: ghc-internal:GHC.Internal.IO.Exception.IOException:
--             
--             eval.log: openFile: resource busy (file is locked)
--             
--             While handling eval.log: openFile: resource busy (file is locked)
--             
--             IPE backtrace:
--             HasCallStack backtrace:
--               throwIO, called at ./Test/Tasty/Run.hs:169:19 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
-- 
-- 1 out of 1 tests failed (0.00s)
--             
--             eval.log: openFile: resource busy (file is locked)
--             
--             While handling eval.log: openFile: resource busy (file is locked)
--             
--             IPE backtrace:
--             HasCallStack backtrace:
--               throwIO, called at ./Test/Tasty/Run.hs:169:19 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
-- 
-- 1 out of 1 tests failed (0.00s)
--             
--             VanillaHttpException (HttpExceptionRequest Request {
--               host                 = "127.0.0.1"
--               port                 = 4444
--               secure               = False
--               requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--               path                 = "/session"
--               queryString          = ""
--               method               = "POST"
--               proxy                = Nothing
--               rawBody              = False
--               redirectCount        = 10
--               responseTimeout      = ResponseTimeoutDefault
--               requestVersion       = HTTP/1.1
--               proxySecureMode      = ProxySecureWithConnect
--             }
--              (ConnectionFailure Network.Socket.connect: <socket: 125>: does not exist (Connection refused)))
--             
--             While handling VanillaHttpException (HttpExceptionRequest Request {
--               |   host                 = "127.0.0.1"
--               |   port                 = 4444
--               |   secure               = False
--               |   requestHeaders       = [("Accept","application/json"),("Content-Type","application/json; charset=utf-8")]
--               |   path                 = "/session"
--               |   queryString          = ""
--               |   method               = "POST"
--               |   proxy                = Nothing
--               |   rawBody              = False
--               |   redirectCount        = 10
--               |   responseTimeout      = ResponseTimeoutDefault
--               |   requestVersion       = HTTP/1.1
--               |   proxySecureMode      = ProxySecureWithConnect
--               | }
--               |  (ConnectionFailure Network.Socket.connect: <socket: 125>: does not exist (Connection refused)))
--             
--             IPE backtrace:
--             HasCallStack backtrace:
--               throwIO, called at ./Test/Tasty/Run.hs:169:19 in tasty-1.5.4-c219ffdaeeb21cab11e36afde43b2b97fa361763300e20b8e8c29de0e1f9bdfc:Test.Tasty.Run
-- 
-- 1 out of 1 tests failed (0.05s)
--             
--             user error (ElementNotFound
--               { description = "No element found matching locator."
--               , locator =
--                   Role
--                     { role = RoleFull { role = Image , name = "Abstract coloured shape" }
--                     }
--               })
--             
--             IPE backtrace:
--             HasCallStack backtrace:
--               throwIO, called at src/UnliftIO/Exception.hs:441:20 in unliftio-0.2.25.1-0a52462179249ece0354b7954600fa487aa91b339b74016a8258d1ea2101a1a7:UnliftIO.Exception
-- 
-- 1 out of 1 tests failed (1.46s)
