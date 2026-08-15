module Common.Utils
  ( -- * LocateActions
    actions,
    locateHttp,
    locateAllHttp,
    locateFromElementHttp,
    locateAllFromElementHttp,

    beforeAll,
    beforeAll_,

    -- * Checkers (list result)
    chkElms,
    chkElmsM,
    chkElmsWithAutoId,
    chkAttribute,
    chkAttributeEq,
    chkLocException,
    chkEq,
    liftFail,
    liftChk,

    -- * Checkers (singleton result)
    chkElm,
    chkElmM,
    chkElmWithAutoId,
    chkAttributeElm,
    chkAttributeEqElm,

    -- * Predicates
    chkCount,
    chkSingleton,
    chkEmpty,

    -- * Test Helpers (list result)
    atrrChk,
    chkAutoId,
    chkAll,
    chkAllNever,

    -- * Test Helpers (singleton result)
    atrrChkElm,
    chkAutoIdElm,

    -- * Element Inspection
    liftFailWithElements,
    liftChkWithElements,
    liftFailWithElement,
    liftChkWithElement,

    -- * Config
    autoId,
    defAllOpts,
    defOpts
  )
where

import Data.Aeson (Value (String))
import Data.Function ((&))
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful
import Effectful.Exception (catch)
import HTTP.Runner (BaseHTTPEffs)
import Test.Tasty (TestTree, withResource)
import Test.Tasty.HUnit (assertFailure, assertEqual)
import UnliftIO (throwIO)
import Utils (txt)
import WebDriver.Effectful
import WebDriver.Effectful.HTTP.Base.Actions
  ( executeScript,
    findElement,
    findElementFromElement,
    findElements,
    findElementsFromElement,
    getElementAttribute,
    getElementProperty,
    getElementText,
  )
import WebDriverPreCore.Extended.HTTP.Base.Protocol (ElementId)
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator, attribute')
import WebDriverPreCore.Extended.Locators.Internal (CaseSensitivity (..), MatchType (..))
import Data.List (singleton)

-- ################ Base Eff Actions ################

actions :: forall es. (IOE :> es, WebDriverHttp :> es) => Eff es (L.LocateActions (Eff es))
actions =
  pure $
    L.MkLocateActions
      { throw = throwIO,
        catch,
        -- noOp trace - tracing is disabled unless a real trace function is supplied
        trace = \_ -> pure (),
        -- Alternative: log a pretty printed WDTrace to stdout
        -- trace = \traceEntry -> liftIO . putStrLn . unpack $ "WDTrace: " <> txt traceEntry,
        findElement,
        findElementFromElement,
        findElements,
        findElementsFromElement,
        executeScript,
        getElementAttribute,
        getElementText
      }

beforeAll_ :: forall a. IO a -> TestTree -> TestTree
beforeAll_ action tree = withResource (action >> pure ()) (\_ -> pure ()) (\_ -> tree)

-- like withResource but does not dispose of anything, just runs the action before the test tree
beforeAll :: forall a. IO a -> (IO a -> TestTree) -> TestTree
beforeAll action mkTree = withResource action (\_ -> pure ()) mkTree

locateHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException ElementId)
locateHttp opts loc = actions >>= \a -> L.locateHttp a opts loc

locateAllHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> Locator -> Eff es (Either L.LocateException [ElementId])
locateAllHttp opts loc = actions >>= \a -> L.locateAllHttp a opts loc

locateFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator -> Eff es (Either L.LocateException ElementId)
locateFromElementHttp opts elmId' loc = actions >>= \a -> L.locateFromElementHttp a opts elmId' loc

locateAllFromElementHttp :: (IOE :> es, WebDriverHttp :> es) => L.HttpLocateOpts -> ElementId -> Locator -> Eff es (Either L.LocateException [ElementId])
locateAllFromElementHttp opts elmId' loc = actions >>= \a -> L.locateAllFromElementHttp a opts elmId' loc

-- ################ Element Inspection ################

-- | Get outerHTML for a list of element IDs
getOuterHtmls :: (WebDriverHttp :> es) => [ElementId] -> Eff es [Text]
getOuterHtmls = traverse getOuterHtml
  where
    getOuterHtml :: (WebDriverHttp :> es) => ElementId -> Eff es Text
    getOuterHtml el = do
      mVal <- getElementProperty el "outerHTML"
      pure $ case mVal of
        Just (String html) -> html
        _ -> "<unable to retrieve outerHTML>"

-- | Format outer HTMLs as a single text with separators
formatOuterHtmls :: [Text] -> Text
formatOuterHtmls htmls = T.intercalate "\n---------\n" htmls

-- | Fail with element outerHTML information appended
liftFailWithElements :: (IOE :> es, WebDriverHttp :> es) => Either L.LocateException [ElementId] -> Text -> [ElementId] -> Eff es a
liftFailWithElements locRslt msg elms = do
  htmls <- getOuterHtmls elms
  let htmlSection = if null htmls then "" else "\n\nFailure Elements:\n" <> formatOuterHtmls htmls
  liftIO . assertFailure . unpack $ msg <> "\n\nLocateResult:\n" <> txt locRslt <> htmlSection

-- | Convert a singleton 'Either' result to a list 'Either' result.
mapSingleton :: Either L.LocateException ElementId -> Either L.LocateException [ElementId]
mapSingleton = fmap singleton

-- | Fail with element outerHTML information appended (singleton variant).
liftFailWithElement :: (IOE :> es, WebDriverHttp :> es) => Either L.LocateException ElementId -> Text -> ElementId -> Eff es a
liftFailWithElement locRslt msg el = liftFailWithElements (mapSingleton locRslt) msg [el]

-- | Check with element outerHTML information on failure
liftChkWithElements :: (IOE :> es, WebDriverHttp :> es) => Either L.LocateException [ElementId] -> Text -> [ElementId] -> Maybe Text -> Eff es ()
liftChkWithElements locRslt testTitle elms mErr = 
  mErr & maybe (pure ()) (\erMsg -> liftFailWithElements locRslt (testTitle <> " - " <> erMsg) elms)

-- | Check with element outerHTML information on failure (singleton variant).
liftChkWithElement :: (IOE :> es, WebDriverHttp :> es) => Either L.LocateException ElementId -> Text -> ElementId -> Maybe Text -> Eff es ()
liftChkWithElement locRslt testTitle el mErr =
  liftChkWithElements (mapSingleton locRslt) testTitle [el] mErr

-- ################ Checks ################

chkLocException :: (IOE :> es, Show a) => Text -> (L.LocateException -> Maybe Text) -> Either L.LocateException a -> Eff es ()
chkLocException errMsg p locRslt =
  either
    (\ex -> liftChk locRslt (errMsg <> ": LocateException check failed: " <> txt ex) $ p ex)
    (const . liftFail locRslt $ errMsg <> ": expected Left LocateException but got Right")
    locRslt

chkElms :: (IOE :> es, WebDriverHttp :> es) => Text -> ([ElementId] -> Maybe Text) -> Either L.LocateException [ElementId] -> Eff es ()
chkElms errMsg p locRslt =
  either
    (liftFail locRslt . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (\elms -> liftChkWithElements locRslt (errMsg <> ": element list check failed") elms $ p elms)
    locRslt

-- | Singleton variant of 'chkElms'.
chkElm :: (IOE :> es, WebDriverHttp :> es) => Text -> (ElementId -> Maybe Text) -> Either L.LocateException ElementId -> Eff es ()
chkElm errMsg p = chkElms errMsg 
                    (\case [x] -> p x
                           _   -> error "chkElm: expected singleton element but got multiple") . mapSingleton

chkElmsM :: (IOE :> es, WebDriverHttp :> es) => Text -> Either L.LocateException [ElementId] -> ([ElementId] -> Eff es (Maybe Text)) -> Eff es ()
chkElmsM testTitle locRslt chkM =
  locRslt & either
    (\err -> liftFail locRslt $ testTitle <> " - locate failed: " <> txt err)
    (\elms -> chkM elms >>= liftChkWithElements locRslt (testTitle <> " - element list check failed") elms)

-- | Singleton variant of 'chkElmsM'.
chkElmM :: (IOE :> es, WebDriverHttp :> es) => Text -> Either L.LocateException ElementId -> (ElementId -> Eff es (Maybe Text)) -> Eff es ()
chkElmM testTitle locRslt chkM = chkElmsM testTitle (mapSingleton locRslt) (\case 
                                                                             [x] -> chkM x
                                                                             _   -> error . unpack $ testTitle <> " - expected singleton element but got multiple")

chkAttribute :: forall es. (IOE :> es, WebDriverHttp :> es) => Text -> Either L.LocateException [ElementId] -> Text -> (Text -> Maybe Text) -> Eff es ()
chkAttribute testTitle locRslt attrName attrValChkM =
  chkElmsM testTitle locRslt elmChk
  where
    elmChk :: [ElementId] -> Eff es (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- getElementAttribute el attrName
        pure $ maybe (Just $ testTitle <> " - attribute not found: " <> txt attrName) attrValChkM attr
      elms -> pure $ Just $ testTitle <> " - expected singlet locate resultlist but got " <> txt (length elms) <> " elms"

-- | Singleton variant of 'chkAttribute'.
chkAttributeElm :: forall es. (IOE :> es, WebDriverHttp :> es) => Text -> Either L.LocateException ElementId -> Text -> (Text -> Maybe Text) -> Eff es ()
chkAttributeElm testTitle locRslt attrName attrValChkM =
  chkAttribute testTitle (mapSingleton locRslt) attrName attrValChkM

chkAttributeEq :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Text -> Either L.LocateException [ElementId] -> Eff es ()
chkAttributeEq testTitle attrName expctd locrslt =
  chkAttribute testTitle locrslt attrName $ \actual ->
    if actual == expctd
      then Nothing
      else Just $ testTitle <> " - expected attribute value: " <> txt expctd <> " but got: " <> txt actual

-- | Singleton variant of 'chkAttributeEq'.
chkAttributeEqElm :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Text -> Either L.LocateException ElementId -> Eff es ()
chkAttributeEqElm testTitle attrName expctd = chkAttributeEq testTitle attrName expctd . mapSingleton

liftFail :: (IOE :> es, Show a) => Either L.LocateException a -> Text -> Eff es b
liftFail locRslt msg = liftIO . assertFailure . unpack $ msg <> "\n\nLocateResult:\n" <> txt locRslt

liftChk :: (IOE :> es, Show a) => Either L.LocateException a -> Text -> Maybe Text -> Eff es ()
liftChk locRslt testTitle mErr = mErr & maybe (pure ()) (\erMsg -> liftFail locRslt $ testTitle <> " - " <> erMsg)

chkEq :: (IOE :> es, Show a, Eq a) => Text -> a -> a -> Eff es ()
chkEq msg a b = liftIO $ assertEqual (unpack msg) a b

-- ################ Predicates ################

chkCount :: Int -> [a] -> Maybe Text
chkCount expected actual
  | length actual == expected = Nothing
  | otherwise = Just $ "Expected " <> txt expected <> " elements but got " <> txt (length actual)

chkSingleton :: [a] -> Maybe Text
chkSingleton = chkCount 1

chkEmpty :: [a] -> Maybe Text
chkEmpty = chkCount 0

-- ################ Config ################

autoId :: Text -> Locator
autoId = attribute' "auto-id" Full CaseSensitive

defOpts :: L.HttpLocateOpts
defOpts =
  L.MkHttpLocateOpts
    { extendedRoleLocation = L.ExtLocateNever,
      jsRecheckDisplayed = L.DisplayedCheckAlways,
      singletonCardinality = L.Unique,
      mkDefaultLoc = autoId
    }

defAllOpts :: L.HttpLocateOpts
defAllOpts =
  L.MkHttpLocateOpts
    { extendedRoleLocation = L.ExtLocateNever,
      jsRecheckDisplayed = L.DisplayedCheckAlways,
      singletonCardinality = L.First,
      mkDefaultLoc = autoId
    }

-- ################ Test Helpers ################

-- | Check an element's attribute value matches expected
-- Takes a test runner, locate function, test name, locator, attribute name, and expected value
atrrChk ::
  (Text -> BaseHTTPEffs () -> TestTree) ->
  (forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es (Either L.LocateException [ElementId])) ->
  Text ->
  Locator ->
  Text ->
  Text ->
  TestTree
atrrChk testRunner locateFn testName loc attrName expctd =
  testRunner testName $ locateFn loc >>= chkAttributeEq (txt loc) attrName expctd

-- | Check an element's auto-id attribute matches expected value
-- Takes a test runner, locate function, test name, locator, and expected auto-id value
chkAutoId ::
  (Text -> BaseHTTPEffs () -> TestTree) ->
  (forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es (Either L.LocateException [ElementId])) ->
  Text ->
  Locator ->
  Text ->
  TestTree
chkAutoId testRunner locateFn testName loc expctd =
  atrrChk testRunner locateFn testName loc "auto-id" expctd

-- | Check an element's attribute value matches expected (singleton result variant).
atrrChkElm ::
  (Text -> m () -> TestTree) ->
  (Locator -> m (Either L.LocateException ElementId) )->
  Text ->
  Locator ->
  Text ->
  Text ->
  TestTree
atrrChkElm testRunner locateFn testName loc attrName expctd =
  testRunner testName $ locateFn loc >>= chkAttributeEqElm (txt loc) attrName expctd

-- | Check an element's auto-id attribute matches expected value (singleton result variant).
chkAutoIdElm ::
  (Text -> m () -> TestTree) ->
  (Locator -> m (Either L.LocateException ElementId)) ->
  Text ->
  Locator ->
  Text ->
  TestTree
chkAutoIdElm testRunner locateFn testName loc expctd =
  atrrChkElm testRunner locateFn testName loc "auto-id" expctd

-- | Locate all elements and check with custom predicate
-- Takes a test runner, locateAll function, test name, locator, and checker function
chkAll ::
  (Text -> BaseHTTPEffs () -> TestTree) ->
  (forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es (Either L.LocateException [ElementId])) ->
  Text ->
  Locator ->
  ([ElementId] -> Maybe Text) ->
  TestTree
chkAll testRunner locateAllFn testName loc chk =
  testRunner testName $ do
    locRslt <- locateAllFn loc
    chkElms (txt loc) chk locRslt

-- | Locate all elements (with DisplayedCheckNever) and check with custom predicate
-- Takes a test runner, locateAll function, test name, locator, and checker function
chkAllNever ::
  (Text -> BaseHTTPEffs () -> TestTree) ->
  (forall es. (IOE :> es, WebDriverHttp :> es) => Locator -> Eff es (Either L.LocateException [ElementId])) ->
  Text ->
  Locator ->
  ([ElementId] -> Maybe Text) ->
  TestTree
chkAllNever testRunner locateAllFn testName loc chk =
  testRunner testName $ do
    locRslt <- locateAllFn loc
    chkElms (txt loc) chk locRslt

-- | Check that located element has the expected auto-id attribute value
chkElmsWithAutoId :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Either L.LocateException [ElementId] -> Eff es ()
chkElmsWithAutoId testTitle expctd locRslt =
  locRslt & either
    (\err -> liftFail locRslt $ testTitle <> " - locate failed: " <> txt err)
    (\elms -> elmChk elms >>= liftChkWithElements locRslt (testTitle <> " - element check failed") elms)
  where
    elmChk :: forall es'. (WebDriverHttp :> es') => [ElementId] -> Eff es' (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- getElementAttribute el "auto-id"
        pure $ case attr of
          Just actual | actual == expctd -> Nothing
          Just actual -> Just $ testTitle <> " - expected auto-id: " <> expctd <> " but got: " <> actual
          Nothing -> Just $ testTitle <> " - auto-id attribute not found"
      elms -> pure $ Just $ testTitle <> " - expected single element but got " <> txt (length elms)

-- | Singleton variant of 'chkElmsWithAutoId'.
chkElmWithAutoId :: (IOE :> es, WebDriverHttp :> es) => Text -> Text -> Either L.LocateException ElementId -> Eff es ()
chkElmWithAutoId testTitle expctd = chkElmsWithAutoId testTitle expctd . mapSingleton
