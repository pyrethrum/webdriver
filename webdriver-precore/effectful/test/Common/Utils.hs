module Common.Utils
  ( -- * LocateActions
    actions,
    locateHttp,
    locateAllHttp,
    locateFromElementHttp,
    locateAllFromElementHttp,

    beforeAll,
    beforeAll_,

    -- * Driver Actions
    DriverActions (..),

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

data DriverActions m = MkDriverActions { 
    testRunner :: Text -> m () -> TestTree,
    getProperty :: ElementId -> Text -> m (Maybe Value),
    getAttribute :: ElementId -> Text -> m (Maybe Text),
    locateFn :: Locator -> m (Either L.LocateException ElementId),
    locateAllFn :: Locator -> m (Either L.LocateException [ElementId])
  }

-- | Get outerHTML for a list of element IDs
getOuterHtmls :: forall m. Monad m => (ElementId -> Text -> m (Maybe Value)) -> [ElementId] -> m [Text]
getOuterHtmls getProp = traverse getOuterHtml
  where
    getOuterHtml :: ElementId -> m Text
    getOuterHtml el = do
      mVal <- getProp el "outerHTML"
      pure $ case mVal of
        Just (String html) -> html
        _ -> "<unable to retrieve outerHTML>"

-- | Format outer HTMLs as a single text with separators
formatOuterHtmls :: [Text] -> Text
formatOuterHtmls htmls = T.intercalate "\n---------\n" htmls

-- | Fail with element outerHTML information appended
liftFailWithElements :: forall m a. MonadIO m => DriverActions m -> Either L.LocateException [ElementId] -> Text -> [ElementId] -> m a
liftFailWithElements (MkDriverActions {getProperty = getProp}) locRslt msg elms = do
  htmls <- getOuterHtmls getProp elms
  let htmlSection = if null htmls then "" else "\n\nFailure Elements:\n" <> formatOuterHtmls htmls
  liftIO . assertFailure . unpack $ msg <> "\n\nLocateResult:\n" <> txt locRslt <> htmlSection

-- | Convert a singleton 'Either' result to a list 'Either' result.
mapSingleton :: Either L.LocateException ElementId -> Either L.LocateException [ElementId]
mapSingleton = fmap singleton

-- | Fail with element outerHTML information appended (singleton variant).
liftFailWithElement :: forall m a. MonadIO m => DriverActions m -> Either L.LocateException ElementId -> Text -> ElementId -> m a
liftFailWithElement driverActions locRslt msg el = liftFailWithElements driverActions (mapSingleton locRslt) msg [el]

-- | Check with element outerHTML information on failure
liftChkWithElements :: forall m. MonadIO m => DriverActions m -> Either L.LocateException [ElementId] -> Text -> [ElementId] -> Maybe Text -> m ()
liftChkWithElements driverActions locRslt testTitle elms mErr = 
  mErr & maybe (pure ()) (\erMsg -> liftFailWithElements driverActions locRslt (testTitle <> " - " <> erMsg) elms)

-- | Check with element outerHTML information on failure (singleton variant).
liftChkWithElement :: forall m. MonadIO m => DriverActions m -> Either L.LocateException ElementId -> Text -> ElementId -> Maybe Text -> m ()
liftChkWithElement driverActions locRslt testTitle el mErr =
  liftChkWithElements driverActions (mapSingleton locRslt) testTitle [el] mErr

-- ################ Checks ################

chkLocException :: forall m a. (MonadIO m, Show a) => Text -> (L.LocateException -> Maybe Text) -> Either L.LocateException a -> m ()
chkLocException errMsg p locRslt =
  either
    (\ex -> liftChk locRslt (errMsg <> ": LocateException check failed: " <> txt ex) $ p ex)
    (const . liftFail locRslt $ errMsg <> ": expected Left LocateException but got Right")
    locRslt

chkElms :: MonadIO m => DriverActions m -> Text -> ([ElementId] -> Maybe Text) -> Either L.LocateException [ElementId] -> m ()
chkElms driverActions errMsg p locRslt =
  either
    (liftFail locRslt . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (\elms -> liftChkWithElements driverActions locRslt (errMsg <> ": element list check failed") elms $ p elms)
    locRslt

-- | Singleton variant of 'chkElms'.
chkElm :: MonadIO m => DriverActions m -> Text -> (ElementId -> Maybe Text) -> Either L.LocateException ElementId -> m ()
chkElm driverActions errMsg p = chkElms driverActions errMsg 
                           (\case [x] -> p x
                                  _   -> error "chkElm: expected singleton element but got multiple") . mapSingleton

chkElmsM :: MonadIO m => DriverActions m -> Text -> Either L.LocateException [ElementId] -> ([ElementId] -> m (Maybe Text)) -> m ()
chkElmsM driverActions testTitle locRslt chkM =
  locRslt & either
    (\err -> liftFail locRslt $ testTitle <> " - locate failed: " <> txt err)
    (\elms -> chkM elms >>= liftChkWithElements driverActions locRslt (testTitle <> " - element list check failed") elms)

-- | Singleton variant of 'chkElmsM'.
chkElmM :: MonadIO m => DriverActions m -> Text -> Either L.LocateException ElementId -> (ElementId -> m (Maybe Text)) -> m ()
chkElmM driverActions testTitle locRslt chkM = chkElmsM driverActions testTitle (mapSingleton locRslt) (\case 
                                                                             [x] -> chkM x
                                                                             _   -> error . unpack $ testTitle <> " - expected singleton element but got multiple")

chkAttribute :: forall m. MonadIO m => DriverActions m -> Text -> Either L.LocateException [ElementId] -> Text -> (Text -> Maybe Text) -> m ()
chkAttribute driverActions@(MkDriverActions {getAttribute = getAttr}) testTitle locRslt attrName attrValChkM =
  chkElmsM driverActions testTitle locRslt elmChk
  where
    elmChk :: [ElementId] -> m (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- getAttr el attrName
        pure $ maybe (Just $ testTitle <> " - attribute not found: " <> txt attrName) attrValChkM attr
      elms -> pure $ Just $ testTitle <> " - expected singlet locate resultlist but got " <> txt (length elms) <> " elms"

-- | Singleton variant of 'chkAttribute'.
chkAttributeElm :: forall m. MonadIO m => DriverActions m -> Text -> Either L.LocateException ElementId -> Text -> (Text -> Maybe Text) -> m ()
chkAttributeElm driverActions testTitle locRslt attrName attrValChkM =
  chkAttribute driverActions testTitle (mapSingleton locRslt) attrName attrValChkM

chkAttributeEq :: forall m. MonadIO m => DriverActions m -> Text -> Text -> Text -> Either L.LocateException [ElementId] -> m ()
chkAttributeEq driverActions testTitle attrName expctd actual =
  chkAttribute driverActions testTitle actual attrName $ \actVal ->
    if actVal == expctd
      then Nothing
      else Just $ testTitle <> " - expected attribute value: " <> txt expctd <> " but got: " <> txt actVal

-- | Singleton variant of 'chkAttributeEq'.
chkAttributeEqElm ::  forall m. MonadIO m => DriverActions m -> Text -> Text -> Text -> Either L.LocateException ElementId -> m ()
chkAttributeEqElm driverActions testTitle attrName expctd = chkAttributeEq driverActions testTitle attrName expctd . mapSingleton

liftFail :: (MonadIO m, Show a) => Either L.LocateException a -> Text -> m b
liftFail locRslt msg = liftIO . assertFailure . unpack $ msg <> "\n\nLocateResult:\n" <> txt locRslt

liftChk :: (MonadIO m, Show a) =>  Either L.LocateException a -> Text -> Maybe Text -> m ()
liftChk locRslt testTitle mErr = mErr & maybe (pure ()) (\erMsg -> liftFail locRslt $ testTitle <> " - " <> erMsg)

chkEq :: (MonadIO m, Eq a, Show a) => Text -> a -> a -> m ()
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
-- Takes driver actions, test name, locator, attribute name, and expected value
atrrChk :: forall m. MonadIO m =>
  DriverActions m ->
  Text ->
  Locator ->
  Text ->
  Text ->
  TestTree
atrrChk driverActions@(MkDriverActions {testRunner = mkTest, locateAllFn = locateAll}) testName loc attrName expctd =
  mkTest testName $ locateAll loc >>= chkAttributeEq driverActions (txt loc) attrName expctd

-- | Check an element's auto-id attribute matches expected value
-- Takes driver actions, test name, locator, and expected auto-id value
chkAutoId :: MonadIO m =>
  DriverActions m ->
  Text ->
  Locator ->
  Text ->
  TestTree
chkAutoId driverActions testName loc expctd =
  atrrChk driverActions testName loc "auto-id" expctd

-- | Check an element's attribute value matches expected (singleton result variant).
atrrChkElm :: MonadIO m =>
  DriverActions m ->
  Text ->
  Locator ->
  Text ->
  Text ->
  TestTree
atrrChkElm driverActions@(MkDriverActions {testRunner = mkTest, locateFn = locate}) testName loc attrName expctd =
  mkTest testName $ locate loc >>= chkAttributeEqElm driverActions (txt loc) attrName expctd

-- | Check an element's auto-id attribute matches expected value (singleton result variant).
chkAutoIdElm :: MonadIO m =>
  DriverActions m ->
  Text ->
  Locator ->
  Text ->
  TestTree
chkAutoIdElm driverActions testName loc expctd =
  atrrChkElm driverActions testName loc "auto-id" expctd

-- | Locate all elements and check with custom predicate
-- Takes driver actions, test name, locator, and checker function
chkAll :: MonadIO m =>
  DriverActions m ->
  Text ->
  Locator ->
  ([ElementId] -> Maybe Text) ->
  TestTree
chkAll driverActions@(MkDriverActions {testRunner = mkTest, locateAllFn = locateAll}) testName loc chk =
  mkTest testName $ do
    locRslt <- locateAll loc
    chkElms driverActions (txt loc) chk locRslt

-- | Locate all elements (with DisplayedCheckNever) and check with custom predicate
-- Takes driver actions, test name, locator, and checker function
chkAllNever :: MonadIO m =>
  DriverActions m ->
  Text ->
  Locator ->
  ([ElementId] -> Maybe Text) ->
  TestTree
chkAllNever driverActions@(MkDriverActions {testRunner = mkTest, locateAllFn = locateAll}) testName loc chk =
  mkTest testName $ do
    locRslt <- locateAll loc
    chkElms driverActions (txt loc) chk locRslt

-- | Check that located element has the expected auto-id attribute value
chkElmsWithAutoId :: forall m. MonadIO m => DriverActions m -> Text -> Text -> Either L.LocateException [ElementId] -> m ()
chkElmsWithAutoId driverActions@(MkDriverActions {getAttribute = getAttr}) testTitle expctd locRslt =
  locRslt & either
    (\err -> liftFail locRslt $ testTitle <> " - locate failed: " <> txt err)
    (\elms -> elmChk elms >>= liftChkWithElements driverActions locRslt (testTitle <> " - element check failed") elms)
  where
    elmChk :: [ElementId] -> m (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- getAttr el "auto-id"
        pure $ case attr of
          Just actual | actual == expctd -> Nothing
          Just actual -> Just $ testTitle <> " - expected auto-id: " <> expctd <> " but got: " <> actual
          Nothing -> Just $ testTitle <> " - auto-id attribute not found"
      elms -> pure $ Just $ testTitle <> " - expected single element but got " <> txt (length elms)

-- | Singleton variant of 'chkElmsWithAutoId'.
chkElmWithAutoId :: forall m. MonadIO m => DriverActions m -> Text -> Text -> Either L.LocateException ElementId -> m ()
chkElmWithAutoId driverActions testTitle expctd = chkElmsWithAutoId driverActions testTitle expctd . mapSingleton
