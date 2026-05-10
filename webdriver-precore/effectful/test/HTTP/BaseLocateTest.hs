module HTTP.BaseLocateTest where

import HTTP.Runner (acquireResources, releaseResources, runMegaformaTest)
import Test.Tasty (TestTree, testGroup, withResource)
import WebDriverPreCore.Extended.Locators
import WebDriverPreCore.Extended.Locate

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

baseLocateTests :: TestTree
baseLocateTests =
  withResource acquireResources releaseResources $ \res ->
    do 
     let test = runMegaformaTest res
         opts = MkHttpLocateOpts { extendedRoleLocation = ExtLocateNever
                                 , jsRecheckDisplayed = DisplayedCheckAlways
                                 , singletonCardinality = Unique
                                 ,  mkDefaultLoc = elmId
                                 }
    --  these tests run against megaforma.html
     testGroup "Base Locate Tests"
      [
        test "Locate by ID" do 
          undefined
      , test "Locate by Name" do
          undefined
      ]


