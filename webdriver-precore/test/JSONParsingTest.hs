module JSONParsingTest where

import Data.Set as S (Set, difference, fromList, null)
import Data.Text as T (Text, lines, null, pack, replace, strip, unwords, words, split, intercalate)
import GHC.Utils.Misc (filterOut)
import Test.Tasty.HUnit as HUnit ( assertBool, Assertion, (@=?) )
import Text.RawString.QQ (r)
import WebDriverPreCore.Spec
import GHC.Show (Show (..))
import Data.Eq (Eq ((==)))
import Data.Ord (Ord)
import Data.Function (($), (.), (&))
import Data.Semigroup ((<>))
import Data.List ((!!), drop)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import GHC.Base (error, Bool (..))
import WebDriverPreCore.Internal.Utils (enumerate)
import Data.Foldable (traverse_)
import Data.Either (either)
import WebDriverPreCore.Spec (UrlPath(..))

import Test.Falsify.Generator as G (Gen, frequency, inRange, list)
import Test.Falsify.Predicate qualified as FP
import Test.Falsify.Range as R (between, skewedBy, enum) 
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Falsify

import Test.Falsify.Generator as G
import Data.Aeson
import Data.Enum (Enum, Bounded (minBound), maxBound)
import GHC.Err (undefined)
import Control.Applicative (pure)
import Data.Bool (Bool)

genMaybe :: G.Gen a -> G.Gen (Maybe a)
genMaybe gen' = G.frequency
  [ (1, pure Nothing)
  , (3, Just <$> gen')
  ]

genOneOf :: forall a. (Enum a, Bounded a ) => G.Gen a
genOneOf = G.inRange $ R.enum (minBound @a, maxBound @a)

genMEnum :: forall a. (Enum a, Bounded a )  => G.Gen (Maybe a)
genMEnum = genMaybe $ genOneOf @a

genVendorSpecific :: G.Gen VendorSpecific
genVendorSpecific = undefined

genBool :: G.Gen Bool
genBool = bool False

genProxy :: G.Gen Proxy
genProxy = undefined

genTimeouts :: G.Gen Timeouts
genTimeouts = undefined

genText :: G.Gen Text
genText = undefined

-- Generate random Capabilities
genCapabilities :: G.Gen Capabilities
genCapabilities = do
  browserName <- genMEnum @BrowserName
  browserVersion <- genMaybe genText
  platformName <- genMEnum 
  strictFileInteractability <- genMaybe genBool
  unhandledPromptBehavior <- genMEnum @UnhandledPromptBehavior
  acceptInsecureCerts <- genMaybe genBool
  pageLoadStrategy <- genMEnum @PageLoadStrategy
  proxy <- genMaybe genProxy
  timeouts <- genMaybe genTimeouts
  vendorSpecific <- genMaybe genVendorSpecific
  pure $ 
    MkCapabilities { ..}

