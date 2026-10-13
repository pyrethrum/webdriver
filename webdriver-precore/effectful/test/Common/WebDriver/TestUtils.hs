module Common.WebDriver.TestUtils
  ( -- * locateAll checks
    locateChkAll,
    locateAllChkAttr,
    locatAllChkAutoId,

    -- * locate checks
    locateChkAttr,
    locateChkAutoId,

    -- * Element Inspection
    getOuterHtmls,
    formatOuterHtmls,

    -- * Failure Related
    locateFail,
    chkLocException,

    -- * Elm / Attribute Predicates
    chkElm,
    chkElms,
    chkAttribute,
    chkAttributeEq,
    chkAttributeSingleton,
    chkAttributesSingletonEq,

    -- * Predicates
    chkEq,
    chkCount,
    chkSingleton,
    chkEmpty,

    -- * Config
    autoId,
  )
where

import Common.WebDriver.Effect (WebDriver)
import Common.WebDriver.Effect qualified as E
import Control.Monad (unless, (>=>))
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

-- ################ Error Conversion ################

getAttribute :: forall elm es. (WebDriver elm :> es, Error Text :> es) => elm -> Text -> Eff es Text
getAttribute el attr = E.getAttribute el attr >>= failNothing ("Attribute not found: " <> attr)

getProperty :: forall elm es. (WebDriver elm :> es, Error Text :> es) => elm -> Text -> Eff es Value
getProperty elm prop = E.getProperty elm prop >>= failNothing ("Property not found: " <> prop)

locate :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Locator -> Eff es elm
locate = E.locate >=> failLeft

locateAll :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Locator -> Eff es [elm]
locateAll = E.locateAll >=> failLeft

fail :: (Error e :> es, Show e) => e -> Eff es a
fail = throwError

failLeft :: forall a e es. (Show e, Error Text :> es) => Either e a -> Eff es a
failLeft = either (fail . txt) pure

failNothing :: forall a es. (Error Text :> es) => Text -> Maybe a -> Eff es a
failNothing errMsg = maybe (fail errMsg) pure

-- ################ Element Inspection ################

-- | Get outerHTML for a list of elements using WebDriver effect
getOuterHtmls :: forall elm es. (WebDriver elm :> es, Error Text :> es) => [elm] -> Eff es [Text]
getOuterHtmls = traverse getOuterHtml
  where
    getOuterHtml :: elm -> Eff es Text
    getOuterHtml =
      flip getProperty "outerHTML"
        >=> \case
          String html -> pure html
          wrong -> fail $ "unable to retrieve outerHTML - expected a String but got: " <> txt wrong

-- | Format outer HTMLs as a single text with separators
formatOuterHtmls :: [Text] -> Text
formatOuterHtmls htmls = T.intercalate "\n---------\n" htmls

-- ################ Checks ################

-- | Check that a locate result is an exception matching a predicate
chkLocException :: forall es a. (Show a, Error Text :> es) => Text -> (L.LocateException -> Bool) -> Either L.LocateException a -> Eff es ()
chkLocException errMsg p locRslt =
  locRslt
    & either
      (\ex -> unless (p ex) $ fail (errMsg <> " (LocateException check failed)\n" <> txt ex))
      (const $ fail (errMsg <> ": expected Left LocateException but got Right:\n" <> txt locRslt))

-- | Check a list of elements against a predicate
chkElms :: forall elm es. (Show elm, Error Text :> es) => Text -> ([elm] -> Bool) -> [elm] -> Eff es ()
chkElms errMsg p elms =
  unless (p elms) $ fail (errMsg <> ": element list check failed for\n" <> txt elms)

-- | Singleton variant of 'chkElms'.
chkElm :: forall elm es. (Show elm, Error Text :> es) => Text -> (elm -> Bool) -> elm -> Eff es ()
chkElm errMsg p elm =
  unless (p elm) $ fail (errMsg <> ": element check failed for\n" <> txt elm)

-- | Check an element's attribute against a predicate
chkAttributeSingleton :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> Text -> (Text -> Eff es ()) -> [elm] -> Eff es ()
chkAttributeSingleton testTitle attrName attrValChk elms =
  case elms of
    [el] -> getAttribute el attrName >>= attrValChk
    _ -> fail $ testTitle <> " - expected singleton element but got " <> txt (length elms) <> " elms"

-- | Singleton variant of 'chkAttribute'.
chkAttribute :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> Text -> (Text -> Eff es ()) -> elm -> Eff es ()
chkAttribute testTitle attrName attrValChk = chkAttributeSingleton testTitle attrName attrValChk . singleton

-- | Check that an element's attribute equals an expected value
chkAttributesSingletonEq :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> Text -> Text -> [elm] -> Eff es ()
chkAttributesSingletonEq testTitle attrName expctd =
  chkAttributeSingleton testTitle attrName (chkEq (testTitle <> " - attribute " <> txt attrName <> " not as expected") expctd)

-- | Singleton variant of 'chkAttributeEq'.
chkAttributeEq :: forall elm es. (WebDriver elm :> es, Error Text :> es) => Text -> Text -> Text -> elm -> Eff es ()
chkAttributeEq testTitle attrName expctd = chkAttributesSingletonEq testTitle attrName expctd . singleton

-- | Fail with detailed locate result information
locateFail :: forall elm es a. (Show elm, Error Text :> es) => Either L.LocateException elm -> Text -> Eff es a
locateFail locRslt msg = fail $ msg <> "\n\nLocateResult:\n" <> txt locRslt

-- ################ Test Helpers ################

locateAllChkAttr ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Text ->
  Eff es ()
locateAllChkAttr loc attrName expctd = do
  locateAll @elm loc >>= chkAttributesSingletonEq (txt loc) attrName expctd

locateChkAttr ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Text ->
  Eff es ()
locateChkAttr loc attrName expctd =
  locate @elm loc >>= chkAttributeEq @elm (txt loc) attrName expctd

locatAllChkAutoId ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Eff es ()
locatAllChkAutoId loc expctd =
  locateAllChkAttr @elm loc "auto-id" expctd

locateChkAutoId ::
  forall elm es.
  (WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  Text ->
  Eff es ()
locateChkAutoId loc expctd =
  locateChkAttr @elm loc "auto-id" expctd

locateChkAll ::
  forall elm es.
  (Show elm, WebDriver elm :> es, Error Text :> es) =>
  Locator ->
  ([elm] -> Bool) ->
  Eff es ()
locateChkAll loc chk = locateAll loc >>= chkElms (txt loc) chk

-- ################ Predicates ################

-- | Assert equality of two values
chkEq :: forall es a. (Eq a, Show a, Error Text :> es) => Text -> a -> a -> Eff es ()
chkEq msg expt act = unless (expt == act) $ fail $ msg <> ":\n  expected: " <> txt expt <> "\n  but got: " <> txt act

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
