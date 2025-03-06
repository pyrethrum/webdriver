module JSONParsingTest where

import Control.Applicative (pure)
import Data.Aeson
import Data.Bool (Bool)
import Data.Either (either)
import Data.Enum (Bounded (minBound), Enum, maxBound)
import Data.Eq (Eq ((==)))
import Data.Foldable (traverse_)
import Data.Function (($), (&), (.))
import Data.Functor ((<$>))
import Data.List (drop, (!!))
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import Data.Semigroup ((<>))
import Data.Set as S (Set, difference, fromList, null)
import Data.Text
import Data.Text as T (Text, intercalate, lines, null, pack, replace, split, strip, unwords, words)
import GHC.Base (Applicative (..), Bool (..), Int, error)
import GHC.Err (undefined)
import GHC.Show (Show (..))
import GHC.Utils.Misc (filterOut)
import Test.Falsify.Generator as G
import Test.Falsify.Generator as G (Gen, frequency, inRange, list)
import Test.Falsify.Predicate qualified as FP
import Test.Falsify.Range as R (between, enum, skewedBy)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify
import Test.Tasty.HUnit as HUnit (Assertion, assertBool, (@=?))
import Text.RawString.QQ (r)
import WebDriverPreCore.Internal.Utils (enumerate)
import WebDriverPreCore.Spec
import WebDriverPreCore.Spec (UrlPath (..))

genMaybe :: G.Gen a -> G.Gen (Maybe a)
genMaybe gen' =
  G.frequency
    [ (1, pure Nothing),
      (3, Just <$> gen')
    ]

genOneOf :: forall a. (Enum a, Bounded a) => G.Gen a
genOneOf = G.inRange $ R.enum (minBound @a, maxBound @a)

genMEnum :: forall a. (Enum a, Bounded a) => G.Gen (Maybe a)
genMEnum = genMaybe $ genOneOf @a

genVendorSpecific :: G.Gen VendorSpecific
genVendorSpecific =
  G.frequency
    [ (1, ChromeOptions <$> genMaybeTextList <*> genMaybeText <*> genMaybeTextList),
      (1, FirefoxOptions <$> genMaybeTextList <*> genMaybeText <*> genMaybeText),
      (1, SafariOptions <$> genMaybeBool <*> genMaybeBool)
    ]

genBool :: G.Gen Bool
genBool = bool False

genMaybeBool :: G.Gen (Maybe Bool)
genMaybeBool = genMaybe genBool

genMaybeTimeout :: G.Gen (Maybe Int)
genMaybeTimeout = genMaybe $ genInt 0 10000

genTimeouts :: G.Gen Timeouts
genTimeouts =
  MkTimeouts
    <$> genMaybeTimeout
      <*> genMaybeTimeout
      <*> genMaybeTimeout

genText :: G.Gen Text
genText =
  G.frequency
    [ (1, pure ""),
      (3, pure "Awd34rtf")
    ]

genTextList :: G.Gen [Text]
genTextList = list (R.between (0, 4)) genText

genMaybeTextList :: G.Gen (Maybe [Text])
genMaybeTextList = genMaybe genTextList

genMaybeText :: G.Gen (Maybe Text)
genMaybeText = genMaybe genText

genManualProxy :: G.Gen Proxy
genManualProxy =
  Manual
    <$> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybe genSocksProxy
      <*> (genMaybe genTextList)

genInt :: Int -> Int -> G.Gen Int
genInt lb ub = G.inRange $ between (lb, ub)

genSocksProxy :: G.Gen SocksProxy
genSocksProxy = MkSocksProxy <$> genText <*> genInt 1 7

genProxy :: G.Gen Proxy
genProxy =
  G.frequency
    [ (1, pure Direct),
      (1, genManualProxy),
      (1, pure AutoDetect),
      (1, pure System),
      (1, Pac <$> genText)
    ]

-- Generate random Capabilities
genCapabilities :: G.Gen Capabilities
genCapabilities = do
  browserName <- genMEnum
  browserVersion <- genMaybe genText
  platformName <- genMEnum
  strictFileInteractability <- genMaybe genBool
  unhandledPromptBehavior <- genMEnum
  acceptInsecureCerts <- genMaybe genBool
  pageLoadStrategy <- genMEnum
  proxy <- genMaybe genProxy
  timeouts <- genMaybe genTimeouts
  vendorSpecific <- genMaybe genVendorSpecific
  pure MkCapabilities {..}
