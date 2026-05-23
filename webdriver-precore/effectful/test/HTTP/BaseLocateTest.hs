module HTTP.BaseLocateTest where

import HTTP.Runner (getWDSession, closeWDSession, runHttpTest, WDSession, testUrl, runHttp, BaseHTTPEffs)
import Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import System.Environment (withArgs)
import Test.Tasty.HUnit (assertBool, assertFailure)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriver.Effectful.HTTP.Base.Actions 
import WebDriverPreCore.Test.TestData
import Effectful
import Effectful.Exception (catch)
import UnliftIO (throwIO)
import WebDriver.Effectful
import WebDriver.Effectful.Logger
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)
import Data.Text (Text, unpack)
import Utils (txt)
import Data.Function ((&))
import Data.Functor ((<&>))

-- >>> _eval baseLocateTests
-- *** Exception: ExitFailure 1
baseLocateTests :: TestTree
baseLocateTests =
    --  these tests run against megaforma.html
  withResource navToMegaForm closeWDSession $ \ses ->
    do 
     let
      test :: Text -> BaseHTTPEffs () -> TestTree
      test = runHttpTest ses

      atrrChk :: Text -> Locator -> Text -> Text -> TestTree
      atrrChk testName loc attrName expctd = 
        test testName $ do
          locRslt <- locate loc
          logTrace locRslt
          chkAttributeEq (txt loc) attrName expctd locRslt

     testGroup "Base Locate Tests"
      [
        atrrChk "Locate by ID" (elmId "section-personal") "auto-id" "sec-personal",
        atrrChk "Locate by Class" (elmClass "input") "auto-id" "hello"
      ]
     where

      defOpts :: L.HttpLocateOpts
      defOpts = L.MkHttpLocateOpts { extendedRoleLocation = L.ExtLocateNever
                                 , jsRecheckDisplayed = L.DisplayedCheckAlways
                                 , singletonCardinality = L.Unique
                                 , mkDefaultLoc = attribute "auto-id"
                                 , locateTracing = L.NoLocateTracing
                                 }
      wantConsoleTrace = False

      logTrace :: L.LocateResult -> IO ()
      logTrace lr = 
        when wantConsoleTrace $ do
          putStrLn "Locate trace:"
          case lr.result of
            Left err -> putStrLn $ " - Locate failed with error: " <> txt err <> "\n - Trace:\n" <> txt lr.trace
            Right elms -> putStrLn $ " - Located elements: " <> txt (length elms) <> "\n - Trace:\n" <> txt lr.trace
     
      locate :: forall es. (IOE :> es, WebDriverHttp :> es)  => Locator -> Eff es L.LocateResult
      locate = locateHttp defOpts
      
      locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es L.LocateResult
      locateAll = locateAllHttp defOpts

      locateFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es L.LocateResult
      locateFromElement = locateFromElementHttp defOpts

      locateAllFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es L.LocateResult
      locateAllFromElement = locateAllFromElementHttp defOpts

      chkAttrEq :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Text -> Text -> Text -> Eff es ()
      chkAttrEq loc msg attr expected = 
        locate loc >>= chkAttributeEq msg attr expected


_eval :: TestTree -> IO ()
_eval = withArgs [] . defaultMain

navToMegaForm :: IO WDSession
navToMegaForm = do
  ses <- getWDSession
  runHttp ses $ testUrl megaformaUrl >>= navigateTo
  pure ses

-- actions :: forall es. (IOE :> es, Logger :> es, Pause :> es, WebDriverHttp :> es) => Eff es (L.LocateActions (Eff es))
actions :: forall es. (IOE :> es, WebDriverHttp :> es) => Eff es (L.LocateActions (Eff es))
actions = pure $ L.MkLocateActions { 
                                   throw = throwIO,
                                   catch,
                                   findElement,
                                   findElementFromElement,
                                   findElements,
                                   findElementsFromElement,
                                   executeScript,
                                   getElementAttribute,  
                                   getElementText
                                }
         
-- ################ Base Eff Actions ################

locateHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es L.LocateResult
locateHttp opts loc =  (actions >>= \a -> L.locateHttp a opts loc)

locateAllHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator ->  Eff es L.LocateResult
locateAllHttp opts loc =  (actions >>= \a -> L.locateAllHttp a opts loc)

locateFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator ->  Eff es L.LocateResult
locateFromElementHttp ops loc elmId' =  (actions >>= \a -> L.locateFromElementHttp a ops loc elmId')

locateAllFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator ->  Eff es L.LocateResult
locateAllFromElementHttp ops loc elmId' =  (actions >>= \a -> L.locateAllFromElementHttp a ops loc elmId')

-- ################ Checks ################

chkLocException :: (IOE :> es) => Text -> (L.LocateException -> Maybe Text) -> L.LocateResult -> Eff es ()
chkLocException errMsg p locRslt =
  either
    (\ex -> liftChk (errMsg <> ": LocateException check failed: " <> txt ex) $ p ex)
    (const . liftFail $ errMsg <> ": expected Left LocateException but got Right")
    (locRslt.result)

chkElms :: (IOE :> es) => Text -> ([ElementId] -> Maybe Text) -> L.LocateResult -> Eff es ()
chkElms errMsg p locRslt =
  either
    (liftFail . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (liftChk (errMsg <> ": element list check failed") . p)
    (locRslt.result)


chkElmsM :: (IOE :> es) => Text -> L.LocateResult -> ([ElementId] -> Eff es (Maybe Text)) -> Eff es ()
chkElmsM testTitle locRslt chkM =
  locRslt.result & either
    (\err -> liftFail $ " - locate failed:\n" <> testTitle <> "\n" <> txt err)
    (\elms -> chkM elms >>= liftChk (testTitle <> " - element list check failed"))

chkAttribute :: forall es. (IOE :> es, WebDriverHttp :> es)=> Text -> L.LocateResult -> Text -> (Text -> Maybe Text) -> Eff es ()
chkAttribute testTitle locRslt attrName attrValChkM = 
    chkElmsM testTitle locRslt elmChk 
    where 
      elmChk :: [ElementId] -> Eff es (Maybe Text)
      elmChk = \case 
        [el] ->  do
          attr <- getElementAttribute el attrName 
          pure $ maybe (Just $ testTitle <> " - attribute not found: " <> txt attrName) attrValChkM attr
        elms -> pure $ Just $ testTitle <> " - expected singlet locate resultlist but got " <> txt (length elms) <> " elms"
   
--   -- do 
--   --  attrs <- getElementAttribute el attrName
--   --  _
--   -- chkElmsM errMsg $ 
--   --   case attrs of
--   --   [el] ->  getElementAttribute el attrName >>= _
--   --       -- maybe
--   --       --   (liftFail $ errMsg <> ": attribute not found: " <> txt attrName)
--   --       --    chkM
--   --   elms -> Just $ errMsg <> ": expected singleton element list but got " <> txt (length elms) <> " elms"

chkAttributeEq :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Text -> L.LocateResult -> Eff es ()
chkAttributeEq testTitle attrName expctd locrslt = 
  chkAttribute testTitle locrslt attrName (\actual -> if actual == expctd 
                                                      then Nothing 
                                                      else Just $ 
                                                       testTitle <> " - expected attribute value: " <> txt expctd <> " but got: " <> txt actual)

liftFail :: (IOE :> es) => Text -> Eff es a
liftFail = liftIO . assertFailure . unpack

liftChk :: (IOE :> es) => Text -> Maybe Text -> Eff es ()
liftChk testTitle mErr = mErr & maybe (pure ()) (\erMsg -> liftFail $ testTitle <> " - " <> erMsg)

