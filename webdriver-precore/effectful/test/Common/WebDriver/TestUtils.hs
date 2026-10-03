module Common.WebDriver.TestUtils
  ( -- * Element Inspection
    getOuterHtmlsWD,
    formatOuterHtmls,
    failWithElements,
    failWithElement,
    chkWithElements,
    chkWithElement,

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

    -- * Test Helpers (list result)
    atrrChk,
    chkAutoId,
    chkAll,
    chkAllNever,

    -- * Test Helpers (singleton result)
    atrrChkElm,
    chkAutoIdElm,

    -- * Predicates
    chkCount,
    chkSingleton,
    chkEmpty,

    -- * Config
    autoId,
  )
where

import Data.Aeson (Value (String))
import Data.Function ((&))
import Data.List (singleton)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Effectful
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, assertEqual)
import Common.WebDriver.Effect qualified as WD
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator, attribute')
import WebDriverPreCore.Extended.Common.Locators.Internal (CaseSensitivity (..), MatchType (..))
import WebDriverPreCore.Utils.Utils (txt)

-- ################ Element Inspection ################

-- | Get outerHTML for a list of elements using WebDriver effect
getOuterHtmlsWD :: forall elm es. (WD.WebDriver elm :> es) => [elm] -> Eff es [Text]
getOuterHtmlsWD = traverse getOuterHtmlWD
  where
    getOuterHtmlWD :: elm -> Eff es Text
    getOuterHtmlWD el = do
      mVal <- WD.getProperty el "outerHTML"
      pure $ case mVal of
        Just (String html) -> html
        _ -> "<unable to retrieve outerHTML>"

-- | Format outer HTMLs as a single text with separators
formatOuterHtmls :: [Text] -> Text
formatOuterHtmls htmls = T.intercalate "\n---------\n" htmls

-- | Fail with element outerHTML information appended (WebDriver variant)
failWithElements :: forall elm es a. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Either L.LocateException [elm] -> Text -> [elm] -> Eff es a
failWithElements locRslt msg elms = do
  htmls <- getOuterHtmlsWD elms
  let htmlSection = if null htmls then "" else "\n\nFailure Elements:\n" <> formatOuterHtmls htmls
  liftIO . assertFailure . unpack $ msg <> "\n\nLocateResult:\n" <> txt locRslt <> htmlSection

-- | Convert a singleton 'Either' result to a list 'Either' result.
mapSingleton :: Either L.LocateException elm -> Either L.LocateException [elm]
mapSingleton = fmap singleton

-- | Fail with element outerHTML information appended (singleton variant, WebDriver)
failWithElement :: forall elm es a. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Either L.LocateException elm -> Text -> elm -> Eff es a
failWithElement locRslt msg el = failWithElements (mapSingleton locRslt) msg [el]

-- | Check with element outerHTML information on failure (WebDriver variant)
chkWithElements :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Either L.LocateException [elm] -> Text -> [elm] -> Maybe Text -> Eff es ()
chkWithElements locRslt testTitle elms mErr =
  mErr & maybe (pure ()) (\erMsg -> failWithElements locRslt (testTitle <> " - " <> erMsg) elms)

-- | Check with element outerHTML information on failure (singleton variant, WebDriver)
chkWithElement :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Either L.LocateException elm -> Text -> elm -> Maybe Text -> Eff es ()
chkWithElement locRslt testTitle el mErr =
  chkWithElements (mapSingleton locRslt) testTitle [el] mErr

-- ################ Checks ################

-- | Check that a locate result is an exception matching a predicate
chkLocException :: forall es a. (IOE :> es, Show a) => Text -> (L.LocateException -> Maybe Text) -> Either L.LocateException a -> Eff es ()
chkLocException errMsg p locRslt =
  either
    (\ex -> liftChk locRslt (errMsg <> ": LocateException check failed: " <> txt ex) $ p ex)
    (const . liftFail locRslt $ errMsg <> ": expected Left LocateException but got Right")
    locRslt

-- | Check a list of elements against a predicate
chkElms :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> ([elm] -> Maybe Text) -> Either L.LocateException [elm] -> Eff es ()
chkElms errMsg p locRslt =
  either
    (liftFail locRslt . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (\elms -> chkWithElements locRslt (errMsg <> ": element list check failed") elms $ p elms)
    locRslt

-- | Singleton variant of 'chkElms'.
chkElm :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> (elm -> Maybe Text) -> Either L.LocateException elm -> Eff es ()
chkElm errMsg p =
  chkElms
    errMsg
    ( \case
        [x] -> p x
        _ -> error "chkElm: expected singleton element but got multiple"
    )
    . mapSingleton

-- | Check a list of elements with a monadic predicate
chkElmsM :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Either L.LocateException [elm] -> ([elm] -> Eff es (Maybe Text)) -> Eff es ()
chkElmsM testTitle locRslt chkM =
  locRslt
    & either
      (\err -> liftFail locRslt $ testTitle <> " - locate failed: " <> txt err)
      (\elms -> chkM elms >>= chkWithElements locRslt (testTitle <> " - element list check failed") elms)

-- | Singleton variant of 'chkElmsM'.
chkElmM :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Either L.LocateException elm -> (elm -> Eff es (Maybe Text)) -> Eff es ()
chkElmM testTitle locRslt chkM =
  chkElmsM
    testTitle
    (mapSingleton locRslt)
    ( \case
        [x] -> chkM x
        _ -> error . unpack $ testTitle <> " - expected singleton element but got multiple"
    )

-- | Check an element's attribute against a predicate
chkAttribute :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Either L.LocateException [elm] -> Text -> (Text -> Maybe Text) -> Eff es ()
chkAttribute testTitle locRslt attrName attrValChkM =
  chkElmsM testTitle locRslt elmChk
  where
    elmChk :: [elm] -> Eff es (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- WD.getAttribute el attrName
        pure $ maybe (Just $ testTitle <> " - attribute not found: " <> txt attrName) attrValChkM attr
      elms -> pure $ Just $ testTitle <> " - expected singleton element but got " <> txt (length elms) <> " elms"

-- | Singleton variant of 'chkAttribute'.
chkAttributeElm :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Either L.LocateException elm -> Text -> (Text -> Maybe Text) -> Eff es ()
chkAttributeElm testTitle locRslt attrName attrValChkM =
  chkAttribute testTitle (mapSingleton locRslt) attrName attrValChkM

-- | Check that an element's attribute equals an expected value
chkAttributeEq :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Text -> Text -> Either L.LocateException [elm] -> Eff es ()
chkAttributeEq testTitle attrName expctd actual =
  chkAttribute testTitle actual attrName $ \actVal ->
    if actVal == expctd
      then Nothing
      else Just $ testTitle <> " - expected attribute value: " <> txt expctd <> " but got: " <> txt actVal

-- | Singleton variant of 'chkAttributeEq'.
chkAttributeEqElm :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Text -> Text -> Either L.LocateException elm -> Eff es ()
chkAttributeEqElm testTitle attrName expctd = chkAttributeEq testTitle attrName expctd . mapSingleton

-- | Fail with detailed locate result information
liftFail :: forall elm es a. (IOE :> es, Show elm) => Either L.LocateException elm -> Text -> Eff es a
liftFail locRslt msg = liftIO . assertFailure . unpack $ msg <> "\n\nLocateResult:\n" <> txt locRslt

-- | Check with optional error message, fail if present
liftChk :: forall elm es. (IOE :> es, Show elm) => Either L.LocateException elm -> Text -> Maybe Text -> Eff es ()
liftChk locRslt testTitle mErr = mErr & maybe (pure ()) (\erMsg -> liftFail locRslt $ testTitle <> " - " <> erMsg)

-- | Assert equality of two values
chkEq :: forall es a. (IOE :> es, Eq a, Show a) => Text -> a -> a -> Eff es ()
chkEq msg a b = liftIO $ assertEqual (unpack msg) a b

-- | Check that an element has the expected auto-id attribute
chkElmsWithAutoId :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Text -> Either L.LocateException [elm] -> Eff es ()
chkElmsWithAutoId testTitle expctd locRslt =
  locRslt
    & either
      (\err -> liftFail locRslt $ testTitle <> " - locate failed: " <> txt err)
      (\elms -> elmChk elms >>= chkWithElements locRslt (testTitle <> " - element check failed") elms)
  where
    elmChk :: [elm] -> Eff es (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- WD.getAttribute el "auto-id"
        pure $ case attr of
          Just actual | actual == expctd -> Nothing
          Just actual -> Just $ testTitle <> " - expected auto-id: " <> expctd <> " but got: " <> actual
          Nothing -> Just $ testTitle <> " - auto-id attribute not found"
      elms -> pure $ Just $ testTitle <> " - expected single element but got " <> txt (length elms)

-- | Singleton variant of 'chkElmsWithAutoId'.
chkElmWithAutoId :: forall elm es. (Show elm, IOE :> es, WD.WebDriver elm :> es) => Text -> Text -> Either L.LocateException elm -> Eff es ()
chkElmWithAutoId testTitle expctd = chkElmsWithAutoId testTitle expctd . mapSingleton

-- ################ Test Helpers ################

atrrChk ::
  forall elm es.
  (Show elm, IOE :> es, WD.WebDriver elm :> es) =>
  (Text -> Eff es () -> TestTree) ->
  Text ->
  Locator ->
  Text ->
  Text ->
  TestTree
atrrChk mkTest testName loc attrName expctd =
  mkTest testName $ WD.locateAll loc >>= chkAttributeEq (txt loc) attrName expctd

chkAutoId ::
  forall elm es.
  (Show elm, IOE :> es, WD.WebDriver elm :> es) =>
  (Text -> Eff es () -> TestTree) ->
  Text ->
  Locator ->
  Text ->
  TestTree
chkAutoId mkTest testName loc expctd =
  atrrChk mkTest testName loc "auto-id" expctd

atrrChkElm ::
  forall elm es.
  (Show elm, IOE :> es, WD.WebDriver elm :> es) =>
  (Text -> Eff es () -> TestTree) ->
  Text ->
  Locator ->
  Text ->
  Text ->
  TestTree
atrrChkElm mkTest testName loc attrName expctd =
  mkTest testName $ WD.locate loc >>= chkAttributeEqElm (txt loc) attrName expctd

chkAutoIdElm ::
  forall elm es.
  (Show elm, IOE :> es, WD.WebDriver elm :> es) =>
  (Text -> Eff es () -> TestTree) ->
  Text ->
  Locator ->
  Text ->
  TestTree
chkAutoIdElm mkTest testName loc expctd =
  atrrChkElm mkTest testName loc "auto-id" expctd

chkAll ::
  forall elm es.
  (Show elm, IOE :> es, WD.WebDriver elm :> es) =>
  (Text -> Eff es () -> TestTree) ->
  Text ->
  Locator ->
  ([elm] -> Maybe Text) ->
  TestTree
chkAll mkTest testName loc chk =
  mkTest testName $ do
    locRslt <- WD.locateAll loc
    chkElms (txt loc) chk locRslt

chkAllNever ::
  forall elm es.
  (Show elm, IOE :> es, WD.WebDriver elm :> es) =>
  (Text -> Eff es () -> TestTree) ->
  Text ->
  Locator ->
  ([elm] -> Maybe Text) ->
  TestTree
chkAllNever mkTest testName loc chk =
  mkTest testName $ do
    locRslt <- WD.locateAll loc
    chkElms (txt loc) chk locRslt

-- ################ Predicates ################
chkCount :: Int -> [a] -> Maybe Text
chkCount expected actual
  | length actual == expected = Nothing
  | otherwise = Just $ "Expected " <> txt expected <> " elements but got " <> txt (length actual)

-- | Check that a list contains exactly one element
chkSingleton :: [a] -> Maybe Text
chkSingleton = chkCount 1

-- | Check that a list is empty
chkEmpty :: [a] -> Maybe Text
chkEmpty = chkCount 0

-- ################ Config ################

-- | Construct an auto-id locator for an attribute value
autoId :: Text -> Locator
autoId = attribute' "auto-id" Full CaseSensitive
