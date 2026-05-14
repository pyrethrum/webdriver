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
import Data.Text (Text)

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
          l <- locate $ elmId "input1"
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
