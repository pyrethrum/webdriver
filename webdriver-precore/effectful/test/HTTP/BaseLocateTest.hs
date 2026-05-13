module HTTP.BaseLocateTest where

import HTTP.Runner (getWDSession, closeWDSession, runHttpTest, WDSession, testUrl, runHttp, BaseHTTPEffs)
import Test.Tasty (TestTree, testGroup, withResource)
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

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

baseLocateTests :: TestTree
baseLocateTests =
  withResource navToMegaForm closeWDSession $ \ses ->
    do 
     let test = runHttpTest ses
         opts = L.MkHttpLocateOpts { extendedRoleLocation = L.ExtLocateNever
                                 , jsRecheckDisplayed = L.DisplayedCheckAlways
                                 , singletonCardinality = L.Unique
                                 , mkDefaultLoc = attribute "auto-id"
                                 }

    --  these tests run against megaforma.html
     testGroup "Base Locate Tests"
      [
        test "Locate by ID" do 
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
         
-- viaActions :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException [ElementId])
viaActions :: (IOE :> es, WebDriverHttp :> es) => (L.LocateActions (Eff es) -> t1 -> t2 -> Eff es b) -> t1 -> t2 -> Eff es b
viaActions f opts loc = actions >>= \a -> f a opts loc

locateHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException [ElementId])
locateHttp = viaActions L.locateHttp 

locateAllHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException [ElementId])
locateAllHttp = viaActions L.locateAllHttp 

applyElmId :: (la -> opts -> eid -> loc -> r) -> la -> opts -> loc -> eid -> r
applyElmId elmf la opts loc eid = elmf la opts eid loc

locateFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
locateFromElementHttp ops loc elmId' = actions >>= \a -> L.locateFromElementHttp a ops loc elmId'

locateAllFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
locateAllFromElementHttp ops loc elmId' = actions >>= \a -> L.locateAllFromElementHttp a ops loc elmId'
