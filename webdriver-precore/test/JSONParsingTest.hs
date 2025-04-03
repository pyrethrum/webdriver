module JSONParsingTest where

import Data.Aeson (ToJSON (toJSON), Value (Bool), decode, encode)
import Data.Bool (Bool, (&&), (||))
import Data.Enum (Bounded (minBound), Enum, maxBound)
import Data.Foldable (all, null)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Map.Strict qualified as M
import Data.Maybe (Maybe (..), isNothing)
import Data.String (String)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import GHC.Base (Applicative (..), Bool (..), Eq (..), Int, const, undefined)
import GHC.Data.Maybe (maybe)
import GHC.Float (Double)
import GHC.IO (FilePath)
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
    Property,
    TestOptions (..),
    Verbose (Verbose),
    assert,
    gen,
    info,
    testPropertyWith,
  )
import Text.Show.Pretty (ppShow)
import WebDriverPreCore
  ( Capabilities (..),
    DeviceMetrics (..),
    LogLevel (..),
    MobileEmulation (..),
    PerfLoggingPrefs (..),
    Proxy (..),
    SocksProxy (..),
    Timeouts (..),
    VendorSpecific (..), LogSettings (MkLogSettings)
  )
import WebDriverPreCore.Internal.Utils (jsonToText)
import GHC.Real (Integral, Fractional (..), fromIntegral)
import Data.Bits (FiniteBits)
import GHC.Num (Num(..))

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

genTextValueMap :: G.Gen (M.Map Text Value)
genTextValueMap =
  M.fromList
    <$> G.list
      (R.between (0, 4))
      ((,) <$> genText <*> genValue)
  where
    genValue :: G.Gen Value
    genValue =
      G.frequency
        [ (1, pure $ toJSON False),
          (1, pure $ toJSON ""),
          (1, pure $ toJSON 0),
          (1, pure $ toJSON 1),
          (1, pure $ toJSON 2),
          (1, pure $ toJSON 3),
          (1, pure $ toJSON 4),
          (1, pure $ toJSON 5),
          (1, pure $ toJSON 6),
          (1, pure $ toJSON 7),
          (1, pure $ toJSON 8),
          (1, pure $ toJSON 9)
        ]

genDeviseMetrics :: Gen DeviceMetrics
genDeviseMetrics = do
  width <- genInt 0 10000
  height <- genInt 0 10000
  pixelRatio <- genDouble 0.0 10000.0
  touch <- genBool
  pure $ MkDeviceMetrics {..}

genMobileEmulation :: Gen MobileEmulation
genMobileEmulation = MkMobileEmulation <$> genMaybeText <*> genMaybe genDeviseMetrics <*> genMaybeText

genPerfLoggingPrefs :: G.Gen PerfLoggingPrefs
genPerfLoggingPrefs = do
  enableNetwork <- genMaybeBool
  enablePage <- genMaybeBool
  enableTimeline <- genMaybeBool
  traceCategories <- genMaybeText
  bufferUsageReportingInterval <- genMaybeInt
  pure $ MkPerfLoggingPrefs {..}

genVendorSpecific :: G.Gen VendorSpecific
genVendorSpecific =
  G.frequency
    [ ( 1,
        -- edge and chrome options are the same at this stage
        ChromeOptions
          <$> genMaybeTextList
            <*> genMaybeText
            <*> genMaybeTextList
            <*> genMaybe genTextValueMap
            <*> genMaybe genMobileEmulation
            <*> genMaybe genTextValueMap
            <*> genMaybeBool
            <*> genMaybe genText
            <*> genMaybeTextList
            <*> genMaybe genFilePath
            <*> genMaybe genPerfLoggingPrefs
            <*> genMaybeTextList
      ),
      ( 1,
        EdgeOptions
          <$> genMaybeTextList
            <*> genMaybeText
            <*> genMaybeTextList
            <*> genMaybe genTextValueMap
            <*> genMaybe genMobileEmulation
            <*> genMaybe genTextValueMap
            <*> genMaybeBool
            <*> genMaybe genText
            <*> genMaybeTextList
            <*> genMaybe genFilePath
            <*> genMaybe genPerfLoggingPrefs
            <*> genMaybeTextList
      ),
      (1, FirefoxOptions <$> genMaybeTextList <*> genMaybeText <*> genMaybeText <*> genMaybe genLogSettings),
      (1, SafariOptions <$> genMaybeBool <*> genMaybeBool)
    ]

genLogLevel :: Gen LogLevel
genLogLevel =
  G.frequency
    [ (1, pure Trace),
      (1, pure Debug),
      (1, pure Info),
      (1, pure Warning),
      (1, pure Error),
      (1, pure Fatal),
      (1, pure Off)
    ]

genLogSettings :: G.Gen LogSettings
genLogSettings = do
  level <- genLogLevel
  pure $ MkLogSettings level

genBool :: G.Gen Bool
genBool = bool False

genMaybeBool :: G.Gen (Maybe Bool)
genMaybeBool = genMaybe genBool

genMaybeTimeout :: G.Gen (Maybe Int)
genMaybeTimeout = genMaybeInt

genMaybeInt :: G.Gen (Maybe Int)
genMaybeInt = genMaybe $ genInt 0 10000

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

genFilePath :: G.Gen FilePath
genFilePath =
  G.frequency
    [ (1, pure "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe"),
      (1, pure "C:\\Program Files\\Mozilla Firefox\\firefox.exe"),
      (1, pure "C:\\Program Files\\Safari\\Safari.exe"),
      (1, pure "C:\\Program Files\\Microsoft\\Edge\\Application\\msedge.exe"),
      (1, pure "C:\\Program Files (x86)\\Internet Explorer\\iexplore.exe"),
      (1, pure "C:\\Program Files (x86)\\Opera Software\\Opera Stable\\opera.exe")
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


genFromRange :: (Integral a,  FiniteBits a) => a -> a -> Gen a
genFromRange lb ub = G.inRange $ R.between (lb, ub)

genInt :: Int -> Int -> G.Gen Int
genInt = genFromRange @Int

genDouble :: Double -> Double -> G.Gen Double
genDouble lb ub = do
  -- Generate an integer in [0..1000000].
  bits <- G.inRange (R.between (0, 1000000 :: Int))
  -- Convert bits to a fraction in [0..1], then interpolate between lb and ub.
  let fraction = fromIntegral bits / 1000000
  pure $ lb + fraction * (ub - lb)

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


subEmt :: forall a. (a -> Bool) -> Maybe a -> Maybe a
subEmt f = maybe Nothing (\x -> if f x then Nothing else Just x)

subEmptyTxt :: Maybe Text -> Maybe Text
subEmptyTxt = subEmt T.null

subEmptyTxtLst :: Maybe [Text] -> Maybe [Text]
subEmptyTxtLst = subEmt emptyTextList

subEmptyVendor :: Maybe VendorSpecific -> Maybe VendorSpecific
subEmptyVendor = subEmt (allPropsNull . subEmptFields)
 where 
  allPropsNull :: VendorSpecific -> Bool
  allPropsNull = undefined

  subEmptFields :: VendorSpecific -> VendorSpecific
  subEmptFields = undefined


emptyTxt :: Maybe Text -> Bool
emptyTxt = maybe True T.null

emptyList :: Maybe [a] -> Bool
emptyList = maybe True null

emptyTextList :: [Text] -> Bool
emptyTextList ml = null ml || (all T.null) ml


isNothingProxy :: Proxy -> Bool
isNothingProxy = \case
  Direct -> False
  Manual ftpProxy httpProxy sslProxy noProxy socksProxy-> 
    all isNothing [ftpProxy, httpProxy, sslProxy] && isNothing noProxy && isNothing socksProxy
  AutoDetect -> False
  Pac url -> T.null url
  System -> False

subEmptyProxy :: Maybe Proxy -> Maybe Proxy
subEmptyProxy p = subEmt isNothingProxy $ subEmptProps <$> p
  where
    subEmptProps :: Proxy -> Proxy
    subEmptProps p' = case p' of
      Manual {..} ->
        Manual
          { ftpProxy = subEmptyTxt ftpProxy,
            httpProxy = subEmptyTxt httpProxy,
            sslProxy = subEmptyTxt sslProxy,
            noProxy = subEmptyTxtLst noProxy,
            socksProxy
          }
      Direct -> p'
      AutoDetect -> p'
      Pac {} -> p'
      System -> p'


subEmptyTimeouts :: Maybe Timeouts -> Maybe Timeouts
subEmptyTimeouts = subEmt isNothingTimeouts 
  where
    isNothingTimeouts :: Timeouts -> Bool
    isNothingTimeouts = \case
      MkTimeouts {..} ->
        all isNothing [implicit, pageLoad, script]

emptyFieldsToNothing :: Capabilities -> Capabilities
emptyFieldsToNothing caps@MkCapabilities {..} = caps {
  browserVersion = subEmptyTxt browserVersion,
  vendorSpecific = subEmptyVendor vendorSpecific,
  platformName,
  acceptInsecureCerts,
  pageLoadStrategy,
  proxy = subEmptyProxy proxy,
  timeouts = subEmptyTimeouts timeouts,
  strictFileInteractability,
  unhandledPromptBehavior
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


            emptyMobileEmulation :: Maybe MobileEmulation -> Bool
            emptyMobileEmulation = \case
              Nothing -> True
              Just me -> case me of
                MkMobileEmulation {
                  deviceName,
                  deviceMetrics,
                  userAgent
                } -> emptyTxt deviceName 
                    && isNothing deviceMetrics
                    && emptyTxt userAgent
            
            emptyPerfLoggingPrefs :: Maybe PerfLoggingPrefs -> Bool
            emptyPerfLoggingPrefs = maybe
              True
              \plp -> case plp of
                MkPerfLoggingPrefs {
                  enableNetwork,
                  enablePage,
                  enableTimeline,
                  traceCategories,
                  bufferUsageReportingInterval
                } -> emptyTxt traceCategories 
                    && isNothing bufferUsageReportingInterval
                    && isNothing enableNetwork
                    && isNothing enablePage
                    && isNothing enableTimeline

            empty :: Maybe VendorSpecific -> Bool
            empty = maybe
              True
              \case
                ChromeOptions {..} ->
                    emptyTextList chromeArgs
                      && emptyTxt chromeBinary
                      && emptyTextList chromeExtensions
                      && emptyList (M.toList <$> chromeLocalState)
                      && emptyList (M.toList <$> chromePrefs)
                      && isNothing chromeDetach
                      && emptyTxt chromeDebuggerAddress
                      && emptyTextList chromeExcludeSwitches
                      && emptyMobileEmulation chromeMobileEmulation
                      && emptyTxt (T.pack <$> chromeMinidumpPath)
                      && emptyPerfLoggingPrefs chromePerfLoggingPrefs
                      && emptyTextList chromeWindowTypes

                EdgeOptions {..} ->
                    emptyTextList edgeArgs
                      && emptyTxt edgeBinary
                      && emptyTextList edgeExtensions
                      && emptyList (M.toList <$> edgeLocalState)
                      && emptyList (M.toList <$> edgePrefs)
                      && isNothing edgeDetach
                      && emptyTxt edgeDebuggerAddress
                      && emptyTextList edgeExcludeSwitches
                      && emptyMobileEmulation edgeMobileEmulation
                      && emptyTxt (T.pack <$> edgeMinidumpPath)
                      && emptyPerfLoggingPrefs edgePerfLoggingPrefs
                      && emptyTextList edgeWindowTypes

                  
                FirefoxOptions
                  { firefoxArgs,
                    firefoxBinary,
                    firefoxProfile,
                    firefoxLog
                  } ->
                    emptyTextList firefoxArgs
                      && emptyTxt firefoxBinary
                      && emptyTxt firefoxProfile
                      && isNothing firefoxLog

                SafariOptions Nothing Nothing -> True
                SafariOptions _ _ -> False
         in noVendor expected == noVendor actual && vendorsEq
    )

wantLogging :: Bool
wantLogging = True

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
