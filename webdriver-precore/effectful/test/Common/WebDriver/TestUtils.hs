module Common.WebDriver.TestUtils
  ( -- * Element Inspection
    getOuterHtmls,
    formatOuterHtmls,
    failWithElements,
    failWithElement,
    chkWithElements,
    chkWithElement,

    -- * Checkers (list result)
    chkElms,
    -- chkElmsM,
    chkElmsWithAutoId,
    chkAttribute,
    chkAttributesEq,
    chkLocException,
    chkEq,
    locateFail,

    -- * Checkers (singleton result)
    chkElm,
    -- chkElmM,
    chkElmWithAutoId,
    chkAttributeElm,
    chkAttributeEq,

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

import Common.WebDriver.Effect
  ( WebDriver,
    getAttribute,
    getProperty,
    locate,
    locateAll,
  )
import Control.Monad (unless)
import Data.Aeson (Value (String))
import Data.Function ((&))
import Data.List (singleton)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Dynamic
import WebDriverPreCore.Extended.Common.Locators.Internal (CaseSensitivity (..), MatchType (..))
import WebDriverPreCore.Extended.Locate qualified as L
import WebDriverPreCore.Extended.Locators (Locator, attribute')
import WebDriverPreCore.Utils.Utils (txt)
import Prelude hiding (fail)

-- ################ Element Inspection ################

-- | Get outerHTML for a list of elements using WebDriver effect
getOuterHtmls :: forall elm es. (WebDriver elm :> es) => [elm] -> Eff es [Text]
getOuterHtmls = traverse getOuterHtml
  where
    getOuterHtml :: elm -> Eff es Text
    getOuterHtml el = do
      mVal <- getProperty el "outerHTML"
      pure $ case mVal of
        Just (String html) -> html
        _ -> "<unable to retrieve outerHTML>"

-- | Format outer HTMLs as a single text with separators
formatOuterHtmls :: [Text] -> Text
formatOuterHtmls htmls = T.intercalate "\n---------\n" htmls

fail :: (Error Text :> es) => Text -> Eff es a
fail = throwError

failLeft :: forall a e es. (Show e, Error Text :> es) => Either e a -> Eff es a
failLeft = either (fail . txt) pure

-- | Fail with element outerHTML information appended (WebDriver variant)
failWithElements :: forall elm es a. (Show elm, WebDriver elm :> es, Error Text :> es) => Either L.LocateException [elm] -> Text -> [elm] -> Eff es a
failWithElements locRslt msg elms = do
  htmls <- getOuterHtmls elms
  let htmlSection = if null htmls then "" else "\n\nFailure Elements:\n" <> formatOuterHtmls htmls
  fail $ msg <> "\n\nLocateResult:\n" <> txt locRslt <> htmlSection

-- | Convert a singleton 'Either' result to a list 'Either' result.
mapSingleton :: Either L.LocateException elm -> Either L.LocateException [elm]
mapSingleton = fmap singleton

-- | Fail with element outerHTML information appended (singleton variant, WebDriver)
failWithElement :: forall elm es a. (Show elm, WebDriver elm :> es, Error Text :> es) => Either L.LocateException elm -> Text -> elm -> Eff es a
failWithElement locRslt msg el = failWithElements (mapSingleton locRslt) msg [el]

-- | Check with element outerHTML information on failure (WebDriver variant)
chkWithElements :: forall elm es. (Show elm, WebDriver elm :> es, Error Text :> es) => Either L.LocateException [elm] -> Text -> [elm] -> Maybe Text -> Eff es ()
chkWithElements locRslt testTitle elms mErr =
  mErr & maybe (pure ()) (\erMsg -> failWithElements locRslt (testTitle <> " - " <> erMsg) elms)

-- | Check with element outerHTML information on failure (singleton variant, WebDriver)
chkWithElement :: forall elm es. (Show elm, WebDriver elm :> es, Error Text :> es) => Either L.LocateException elm -> Text -> elm -> Maybe Text -> Eff es ()
chkWithElement locRslt testTitle el mErr =
  chkWithElements (mapSingleton locRslt) testTitle [el] mErr

-- ################ Checks ################

-- | Check that a locate result is an exception matching a predicate
chkLocException :: forall es a. (Show a, Error Text :> es) => Text -> (L.LocateException -> Bool) -> Either L.LocateException a -> Eff es ()
chkLocException errMsg p locRslt =
  locRslt & either
    (\ex -> unless (p ex) $ fail (errMsg <> " (LocateException check failed)\n" <> txt ex))
    (\_ -> fail (errMsg <> ": expected Left LocateException but got Right\n" <> txt locRslt))

-- | Check a list of elements against a predicate
chkElms :: forall elm es. (Show elm, WebDriver elm :> es, Error Text :> es) => Text -> ([elm] -> Maybe Text) -> Either L.LocateException [elm] -> Eff es ()
chkElms errMsg p locRslt =
  either
    (locateFail locRslt . (errMsg <>) . (<> ": expected Right elements but got Left: ") . txt)
    (\elms -> chkWithElements locRslt (errMsg <> ": element list check failed") elms $ p elms)
    locRslt

-- | Singleton variant of 'chkElms'.
chkElm :: forall elm es. (Show elm, WebDriver elm :> es, Error Text :> es) => Text -> (elm -> Maybe Text) -> Either L.LocateException elm -> Eff es ()
chkElm errMsg p =
  chkElms
    errMsg
    ( \case
        [x] -> p x
        _ -> error "chkElm: expected singleton element but got multiple"
    )
    . mapSingleton


-- | Check an element's attribute against a predicate
chkAttribute :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> [elm] -> Text -> (Text -> Eff es ()) -> Eff es ()
chkAttribute testTitle locRslt attrName attrValChk =
  case locRslt of
    [el] ->
      getAttribute el attrName
        >>= maybe
          (fail $ testTitle <> " - attribute not found: " <> txt attrName)
          attrValChk
    elms -> fail $ testTitle <> " - expected singleton element but got " <> txt (length elms) <> " elms"

-- | Singleton variant of 'chkAttribute'.
chkAttributeElm :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> elm -> Text -> (Text -> Eff es ()) -> Eff es ()
chkAttributeElm testTitle locRslt attrName attrValChk = chkAttribute testTitle [locRslt] attrName attrValChk

-- | Check that an element's attribute equals an expected value
chkAttributesEq :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> Text -> Text -> [elm] -> Eff es ()
chkAttributesEq testTitle attrName expctd actual =
  chkAttribute testTitle actual attrName \actVal ->
    unless (actVal == expctd)
      $ fail
      $ testTitle <> " - expected attribute value: " <> txt expctd <> " but got: " <> txt actVal

-- | Singleton variant of 'chkAttributeEq'.
chkAttributeEq :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> Text -> Text -> elm -> Eff es ()
chkAttributeEq testTitle attrName expctd = chkAttributesEq testTitle attrName expctd . pure

-- | Fail with detailed locate result information
locateFail :: forall elm es a. (Show elm, Error Text :> es) => Either L.LocateException elm -> Text -> Eff es a
locateFail locRslt msg = fail $ msg <> "\n\nLocateResult:\n" <> txt locRslt

-- | Assert equality of two values
chkEq :: forall es a. (Eq a, Show a, Error Text :> es) => Text -> a -> a -> Eff es ()
chkEq msg expt act = unless (expt == act) $ fail $ msg <> ":\n  expected: " <> txt expt <> "\n  but got: " <> txt act

-- | Check that an element has the expected auto-id attribute
chkElmsWithAutoId :: forall elm es. (Show elm, WebDriver elm :> es, Error Text :> es) => Text -> Text -> Either L.LocateException [elm] -> Eff es ()
chkElmsWithAutoId testTitle expctd locRslt =
  locRslt
    & either
      (\err -> fail $ testTitle <> " - locate failed: " <> txt err)
      (\elms -> elmChk elms >>= chkWithElements locRslt (testTitle <> " - element check failed") elms)
  where
    elmChk :: [elm] -> Eff es (Maybe Text)
    elmChk = \case
      [el] -> do
        attr <- getAttribute el "auto-id"
        pure $ case attr of
          Just actual | actual == expctd -> Nothing
          Just actual -> Just $ testTitle <> " - expected auto-id: " <> expctd <> " but got: " <> actual
          Nothing -> Just $ testTitle <> " - auto-id attribute not found"
      elms -> pure $ Just $ testTitle <> " - expected single element but got " <> txt (length elms)

-- | Singleton variant of 'chkElmsWithAutoId'.
chkElmWithAutoId :: forall elm es. (Show elm, WebDriver elm :> es, Error Text :> es) => Text -> Text -> Either L.LocateException elm -> Eff es ()
chkElmWithAutoId testTitle expctd = chkElmsWithAutoId testTitle expctd . mapSingleton

-- ################ Test Helpers ################

atrrChk ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Text ->
  Eff es ()
atrrChk loc attrName expctd = do
  locRslt <- locateAll @elm loc
  failLeft locRslt
    >>= chkAttributesEq (txt loc) attrName expctd

chkAutoId ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Eff es ()
chkAutoId loc expctd =
  atrrChk @elm loc "auto-id" expctd

atrrChkElm ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Text ->
  Eff es ()
atrrChkElm loc attrName expctd =
  locate @elm loc >>= failLeft >>= chkAttributeEq @elm (txt loc) attrName expctd

chkAutoIdElm ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Eff es ()
chkAutoIdElm loc expctd =
  atrrChkElm @elm loc "auto-id" expctd

chkAll ::
  forall elm es.
  (Show elm, WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  ([elm] -> Maybe Text) ->
  Eff es ()
chkAll loc chk =
  do
    locRslt <- locateAll loc
    chkElms (txt loc) chk locRslt

chkAllNever ::
  forall elm es.
  (Show elm, WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  ([elm] -> Maybe Text) ->
  Eff es ()
chkAllNever loc chk =
  locateAll loc >>= chkElms (txt loc) chk

-- ################ Predicates ################

chkCount :: (Error Text :> es) => Int -> [a] -> Eff es ()
chkCount expected actual =
  unless
    (expected == actualCount)
    (fail $ "Expected " <> txt expected <> " elements but got " <> txt actualCount)
  where
    actualCount = length actual

-- | Check that a list contains exactly one element
chkSingleton :: (Error Text :> es) => [a] -> Eff es ()
chkSingleton = chkCount 1

-- | Check that a list is empty
chkEmpty :: (Error Text :> es) => [a] -> Eff es ()
chkEmpty = chkCount 0

-- ################ Config ################

-- | Construct an auto-id locator for an attribute value
autoId :: Text -> Locator
autoId = attribute' "auto-id" Full CaseSensitive
