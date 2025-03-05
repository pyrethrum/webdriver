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
import GHC.Base (error)
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



-- Generate a random Value (very simplistic)
genJSONValue :: G.Gen Value
genJSONValue = G.frequency
  [ (1, pure A.Null)
  , (1, pure $ object ["key" .= ("example" :: Text)])
  ]


genOneOf :: forall a. Enum a => G.Gen a
genOneOf = G.inRange $ R.enum (minBound @a, maxBound @a)

genMaybeOf :: forall a. Enum a => G.Gen (Maybe a)
genMaybeOf gen' = G.frequency
  [ (1, pure Nothing)
  , (3, Just <$> gen')
  ]

-- Generate a random BrowserName
-- genBrowserName :: G.Gen BrowserName
-- genBrowserName = genOneOf

-- Generate a random UnhandledPromptBehavior
genUnhandledPromptBehavior :: G.Gen UnhandledPromptBehavior
genUnhandledPromptBehavior = G.frequency
  [ (1, pure Dismiss)
  , (1, pure Accept)
  , (1, pure DismissAndNotify)
  , (1, pure AcceptAndNotify)
  , (1, pure Ignore)
  ]

-- Generate random Capabilities
genCapabilities :: G.Gen Capabilities
genCapabilities = do

  pure $ undefined
    MkCapabilities
      { browserName = mbBrowser
      , strictFileInteractability = mbStrictFileInteract
      , unhandledPromptBehavior = mbUnhandledPrompt
      , vendorSpecific = mbVendorSpecific
      }


