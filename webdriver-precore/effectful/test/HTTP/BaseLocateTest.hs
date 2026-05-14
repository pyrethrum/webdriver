module HTTP.BaseLocateTest where

import HTTP.Runner (getWDSession, closeWDSession, runHttpTest, WDSession, testUrl, runHttp, BaseHTTPEffs)
import Test.Tasty (TestTree, testGroup, withResource)
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

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

baseLocateTests :: TestTree
baseLocateTests =
  withResource navToMegaForm closeWDSession $ \ses ->
    do 
     let 
      test :: Text -> BaseHTTPEffs () -> TestTree
      test = runHttpTest ses

      defOpts = L.MkHttpLocateOpts { extendedRoleLocation = L.ExtLocateNever
                                 , jsRecheckDisplayed = L.DisplayedCheckAlways
                                 , singletonCardinality = L.Unique
                                 , mkDefaultLoc = attribute "auto-id"
                                 }
      locate :: forall es. (IOE :> es, WebDriverHttp :> es)  => Locator -> Eff es (Either L.LocateException [ElementId])
      locate = locateHttp defOpts
      
      locateAll :: forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es (Either L.LocateException [ElementId])
      locateAll = locateAllHttp defOpts

      locateFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
      locateFromElement = locateFromElementHttp defOpts

      locateAllFromElement :: forall es. (IOE :> es, WebDriverHttp :> es) => ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
      locateAllFromElement = locateAllFromElementHttp defOpts
    --  these tests run against megaforma.html
     testGroup "Base Locate Tests"
      [
        test "Locate by ID" do 
          l <- locate $ elmId "section-personal"
          undefined
      , test "Locate by Name" do
          undefined
      ]


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
         

locateHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException [ElementId])
locateHttp opts loc = actions >>= \a -> L.locateHttp a opts loc 

locateAllHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException [ElementId])
locateAllHttp opts loc = actions >>= \a -> L.locateAllHttp a opts loc

locateFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
locateFromElementHttp ops loc elmId' = actions >>= \a -> L.locateFromElementHttp a ops loc elmId'

locateAllFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
locateAllFromElementHttp ops loc elmId' = actions >>= \a -> L.locateAllFromElementHttp a ops loc elmId'

-- ---------------------------------------------------------------------------
-- Check helpers
-- ---------------------------------------------------------------------------

chkLocException :: (IOE :> es) => Text -> (L.LocateException -> Bool) -> Either L.LocateException [ElementId] -> Eff es ()
chkLocException errMsg p =
  either
    (\ex -> liftChk (errMsg <> ": LocateException check failed: " <> txt ex) $ p ex)
    (const . liftFail $ errMsg <> ": expected Left LocateException but got Right")

chkElms :: (IOE :> es) => Text -> ([ElementId] -> Bool) -> Either L.LocateException [ElementId] -> Eff es ()
chkElms errMsg p =
  either
    (liftFail . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (liftChk (errMsg <> ": element list check failed") . p)


chkElmsM :: (IOE :> es) => Text -> ([ElementId] -> Eff es Bool) -> Either L.LocateException [ElementId] -> Eff es ()
chkElmsM errMsg chk =
  either
    (liftFail . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (\elms -> chk elms >>= liftChk (errMsg <> ": element list monadic check failed"))

chkAttribute :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> (Text -> Bool) -> Either L.LocateException [ElementId] -> Eff es ()
chkAttribute errMsg attrName chk = chkElmsM errMsg $ \case
  [el] ->
    getElementAttribute el attrName >>=
      maybe
        (liftFail $ errMsg <> ": attribute not found: " <> txt attrName)
        (pure . chk)
  elms -> liftFail $ errMsg <> ": expected singleton element list but got " <> txt (length elms)

chkAttributeEq :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Text -> Either L.LocateException [ElementId] -> Eff es ()
chkAttributeEq errMsg attrName  = chkAttribute errMsg attrName . (==) 


liftFail :: (IOE :> es) => Text -> Eff es a
liftFail = liftIO . assertFailure . unpack

liftChk :: (IOE :> es) => Text -> Bool -> Eff es ()
liftChk msg ok = liftIO $ assertBool (unpack msg) ok 

