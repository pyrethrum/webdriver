module JSONParsingTest where

import Data.Aeson (Value, decode, encode)
import Data.Bool (Bool, (&&), (||))
import Data.Enum (Bounded (minBound), Enum, maxBound)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))
import Data.Text (Text, unpack)
import Data.Text qualified as T
import GHC.Base (Applicative (..), Bool (..), Eq (..), Int, const)
import GHC.Data.Maybe (maybe)
import Test.Falsify.Generator as G
  ( Gen,
    bool,
    frequency,
    inRange,
    list,
  )
import Test.Falsify.Predicate (dot, expect, fn, (.$))
import Test.Falsify.Range as R (between, enum)
import Test.Tasty (TestTree)
import Test.Tasty.Falsify
  ( ExpectFailure (DontExpectFailure),
    TestOptions (..),
    Verbose (Verbose),
    assert,
    gen,
    info,
    testPropertyWith, Property,
  )
import Text.Show.Pretty (ppShow)
import WebDriverPreCore.Internal.Utils (jsonToText)
import WebDriverPreCore
  ( Capabilities (..),
    Proxy (AutoDetect, Direct, Manual, Pac, System),
    SocksProxy (MkSocksProxy),
    Timeouts (MkTimeouts),
    VendorSpecific (..)
  )
import Data.Foldable (null, all)
import Data.String (String)

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
      (1, pure "Awd34rtf"),
      (1, pure "f"),
      (1, pure "sfdsds"),
      (1, pure "5"),
      (1, pure "ttttttttttttt"),
      (1, pure "a"),
      (1, pure "zzzzz"),
      (1, pure "rr"),
      (1, pure "aa"),
      (1, pure "pppp")
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

options :: TestOptions
options =
  TestOptions
    { expectFailure = DontExpectFailure,
      overrideVerbose = if wantLogging then Just Verbose else Nothing,
      overrideMaxShrinks = Nothing,
      overrideNumTests = Just 1000,
      overrideMaxRatio = Nothing
    }

jsonEq :: Capabilities -> Maybe Capabilities -> Bool
jsonEq expected =
  maybe
    False
    ( \actual ->
        let noVendor caps = caps {vendorSpecific = Nothing}
            ev = expected.vendorSpecific
            av = actual.vendorSpecific
            vendorsEq = empty ev && empty av || ev == av

            emptyTxt :: Maybe Text -> Bool
            emptyTxt = maybe True T.null

            emptyList :: Maybe [a] -> Bool
            emptyList = maybe True null

            emptyTextList :: Maybe [Text] -> Bool
            emptyTextList ml = emptyList ml || maybe True (all T.null) ml

            empty :: Maybe VendorSpecific -> Bool
            empty = \case
              Nothing -> True
              Just vs -> case vs of
                ChromeOptions
                  { chromeArgs,
                    chromeBinary,
                    chromeExtensions
                  } ->
                    emptyTextList chromeArgs
                      && emptyTxt chromeBinary
                      && emptyTextList chromeExtensions
                FirefoxOptions
                  { firefoxArgs,
                    firefoxBinary,
                    firefoxProfile
                  } ->
                    emptyTextList firefoxArgs
                      && emptyTxt firefoxBinary
                      && emptyTxt firefoxProfile
                SafariOptions Nothing Nothing -> True
                SafariOptions _ _ -> False
         in noVendor expected == noVendor actual && vendorsEq
    )

wantLogging :: Bool
wantLogging = False

log :: String -> Property ()
log = if wantLogging then info else const $ pure ()


test_round_trip :: TestTree
test_round_trip = testPropertyWith options "Roundtrip Capabilities Parsing" $ do
  c <- gen genCapabilities
  let encoded = encode c
      showEncode = jsonToText <$> decode @Value encoded
      decoded = decode @Capabilities encoded
      asExpected = jsonEq c

  log ""
  log "Initial Capabilities:"
  log $ ppShow c
  log ""
  log "Encoded Capabilities:"
  log $ maybe "Nothing" unpack showEncode
  log ""
  log "Decoded Capabilities:"
  log $ maybe "Nothing" ppShow decoded
  assert $ expect True `dot` fn ("matches encoded", asExpected) .$ ("decoded", decoded)


