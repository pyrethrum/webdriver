module HTTP.BaseLocateTest where

import HTTP.Runner (getWDSession, closeWDSession, runHttpTest, withHttp, WDSession, testUrl, runHttp)
import Test.Tasty (TestTree, testGroup, withResource)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locate
import WebDriver.Effectful.HTTP.Base.Actions (navigateTo)
import Effectful (runEff)
import WebDriverPreCore.Test.TestData

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

baseLocateTests :: TestTree
baseLocateTests =
  withResource navToMegaForm closeWDSession $ \ses ->
    do 
     let test = runHttpTest ses
         opts = MkHttpLocateOpts { extendedRoleLocation = ExtLocateNever
                                 , jsRecheckDisplayed = DisplayedCheckAlways
                                 , singletonCardinality = Unique
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