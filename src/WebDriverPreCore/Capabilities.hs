module WebDriverPreCore.Capabilities
  ( 
    FullCapabilities (..),
    StandardCapabilities (..),
    UnhandledPromptBehavior (..),
    PageLoadStrategy (..),
    BrowserName (..),
    PlatformName (..),
    Proxy (..),
    Timeouts (..),
    VendorSpecific (..),
    MatchCapabilities (..),
    minStandardCapabilities,
    minFullCapabilities,
    minFirefoxCapabilities,
    minChromeCapabilities,
  )
where

import Control.Applicative (Alternative (..), Applicative (..))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Object,
    Pair,
    Parser,
    ToJSON (toJSON, toEncoding),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?), genericToEncoding, defaultOptions, Encoding,
  )
import Data.Bool (bool, Bool)
import Data.Maybe (catMaybes, Maybe (..), maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.Internal.Utils (opt)
import Data.Int (Int)
import GHC.Show (Show (..))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Control.Monad (Monad(..), MonadFail (..))
import Data.Semigroup (Semigroup(..))

{- references:
- https://https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities

 - https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities
 - https://mucsi96.gitbook.io/w3c-webdriver/capabilities

 -}

data MatchCapabilities = MkMatchCapabilities
   { 
    alwaysMatch :: Maybe StandardCapabilities,
    firstMatch :: [StandardCapabilities]
  }
  deriving (Show, Generic)

instance ToJSON MatchCapabilities
instance FromJSON MatchCapabilities
newtype FullCapabilities = MkFullCapabilities
  { 
    capabilities :: MatchCapabilities
  }
  deriving (Show, Generic)

instance ToJSON FullCapabilities where
  toEncoding :: FullCapabilities -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FullCapabilities

minFullCapabilities :: BrowserName -> FullCapabilities
minFullCapabilities browserName =
  MkFullCapabilities $ MkMatchCapabilities
    { alwaysMatch = Just $ minStandardCapabilities browserName,
      firstMatch = []
    }

minStandardCapabilities :: BrowserName -> StandardCapabilities
minStandardCapabilities browserName =
  MkStandardCapabilities
    { browserName = Just browserName,
      browserVersion = Nothing,
      platformName = Nothing,
      acceptInsecureCerts = Nothing,
      pageLoadStrategy = Nothing,
      proxy = Nothing,
      timeouts = Nothing,
      strictFileInteractability = Nothing,
      unhandledPromptBehavior = Nothing,
      vendorSpecific = Nothing
    }

minFirefoxCapabilities :: FullCapabilities
minFirefoxCapabilities = minFullCapabilities Firefox

minChromeCapabilities :: FullCapabilities
minChromeCapabilities = minFullCapabilities Chrome

-- Custom Types for Enums
data UnhandledPromptBehavior
  = Dismiss
  | Accept
  | DismissAndNotify
  | AcceptAndNotify
  | Ignore
  deriving (Show, Generic)

data PageLoadStrategy
  = None
  | Eager
  | Normal
  deriving (Show, Generic)

data BrowserName
  = Chrome
  | Firefox
  | Safari
  | Edge
  | InternetExplorer
  deriving (Show, Generic)

data PlatformName
  = Windows
  | Mac
  | Linux
  | Android
  | IOS
  deriving (Show, Generic)

-- Core Capabilities
data StandardCapabilities = MkStandardCapabilities
  { browserName :: Maybe BrowserName,
    browserVersion :: Maybe Text,
    platformName :: Maybe PlatformName,
    acceptInsecureCerts :: Maybe Bool,
    pageLoadStrategy :: Maybe PageLoadStrategy,
    proxy :: Maybe Proxy,
    timeouts :: Maybe Timeouts,
    strictFileInteractability :: Maybe Bool,
    unhandledPromptBehavior :: Maybe UnhandledPromptBehavior,
    vendorSpecific :: Maybe VendorSpecific
  }
  deriving (Show, Generic)

data SocksProxy = SocksProxy
  { socksProxy :: Text,
    socksVersion :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON SocksProxy where
  toJSON :: SocksProxy -> Value
  toJSON SocksProxy {..} =
    object
      [ "socksProxy" .= socksProxy,
        "socksVersion" .= socksVersion
      ]

instance FromJSON SocksProxy where
  parseJSON :: Value -> Parser SocksProxy
  parseJSON = withObject "SocksProxy" $ \v ->
    SocksProxy
      <$> v .: "socksProxy"
      <*> v .: "socksVersion"

data Proxy
  = Direct
  | Manual
      { ftpProxy :: Maybe Text,
        httpProxy :: Maybe Text,
        sslProxy :: Maybe Text,
        socksProxy :: Maybe SocksProxy,
        noProxy :: Maybe [Text]
      }
  | AutoDetect
  | System
  | Pac
      { proxyAutoconfigUrl :: Text
      }
  deriving (Show, Eq)

instance ToJSON Proxy where
  toJSON :: Proxy -> Value
  toJSON = \case
    Direct -> "direct"
    AutoDetect -> "autodetect"
    System -> "system"
    Pac {..} -> object ["proxyAutoconfigUrl" .= proxyAutoconfigUrl]
    Manual {..} ->
      object $
        catMaybes
          [ opt "ftpProxy" ftpProxy,
            opt "httpProxy" httpProxy,
            opt "sslProxy" sslProxy,
            opt "socksProxy" socksProxy,
            opt "noProxy" noProxy
          ]

-- TODO :: test esp manual
instance FromJSON Proxy where
  parseJSON :: Value -> Parser Proxy
  parseJSON = withObject "Proxy" $
    \v -> do
      let direct = v .: "direct" >>= bool (fail "Invalid Proxy") (pure Direct)
          autoDetect = v .: "autodetect" >>= bool (fail "Invalid Proxy") (pure AutoDetect)
          system = v .: "system" >>= bool (fail "Invalid Proxy") (pure System)
          pac = Pac <$> v .: "proxyAutoconfigUrl"
          manual = Manual <$> v .:? "ftpProxy" <*> v .:? "httpProxy" <*> v .:? "sslProxy" <*> v .:? "socksProxy" <*> v .:? "noProxy"
      direct <|> autoDetect <|> system <|> pac <|> manual

-- Vendor-Specific Capabilities
data VendorSpecific
  = ChromeOptions
      { chromeArgs :: Maybe [Text],
        chromeBinary :: Maybe Text,
        chromeExtensions :: Maybe [Text]
      }
  | FirefoxOptions
      { firefoxArgs :: Maybe [Text],
        firefoxBinary :: Maybe Text,
        firefoxProfile :: Maybe Text
      }
  | SafariOptions
      { safariAutomaticInspection :: Maybe Bool,
        safariAutomaticProfiling :: Maybe Bool
      }
  deriving (Show, Generic)

-- ToJSON Instance for VendorSpecific
instance ToJSON VendorSpecific where
  toJSON :: VendorSpecific -> Value
  toJSON vs =
    object $ catMaybes props
    where
      props = case vs of
        ChromeOptions {chromeArgs, chromeBinary, chromeExtensions} ->
          [ opt "args" chromeArgs,
            opt "binary" chromeBinary,
            opt "extensions" chromeExtensions
          ]
        FirefoxOptions {firefoxArgs, firefoxBinary, firefoxProfile} ->
          [ opt "args" firefoxArgs,
            opt "binary" firefoxBinary,
            opt "profile" firefoxProfile
          ]
        SafariOptions {safariAutomaticInspection, safariAutomaticProfiling} ->
          [ opt "automaticInspection" safariAutomaticInspection,
            opt "automaticProfiling" safariAutomaticProfiling
          ]

instance FromJSON VendorSpecific where
  parseJSON :: Value -> Parser VendorSpecific
  parseJSON = withObject "VendorSpecific" $ \v -> do
    let chromeOptions = ChromeOptions <$> v .:? "args" <*> v .:? "binary" <*> v .:? "extensions"
        firefoxOptions = FirefoxOptions <$> v .:? "args" <*> v .:? "binary" <*> v .:? "profile" 
        safariOptions = SafariOptions <$> v .:? "automaticInspection" <*> v .:? "automaticProfiling" 
    chromeOptions <|> firefoxOptions <|> safariOptions

-- ToJSON Instances
instance ToJSON StandardCapabilities where
  toJSON :: StandardCapabilities -> Value
  toJSON
    MkStandardCapabilities
      { browserName,
        browserVersion,
        platformName,
        acceptInsecureCerts,
        pageLoadStrategy,
        proxy,
        timeouts,
        strictFileInteractability,
        unhandledPromptBehavior,
        vendorSpecific
      } =
      object $
        [ "browserName" .= browserName
        ]
          <> catMaybes
            [ opt "browserVersion" browserVersion,
              opt "platformName" platformName,
              opt "acceptInsecureCerts" acceptInsecureCerts,
              opt "pageLoadStrategy" pageLoadStrategy,
              opt "proxy" proxy,
              opt "timeouts" timeouts,
              opt "strictFileInteractability" strictFileInteractability,
              opt "unhandledPromptBehavior" unhandledPromptBehavior
            ]
          <> vendorSpecificToJSON vendorSpecific

vendorSpecificToJSON :: Maybe VendorSpecific -> [Pair]
vendorSpecificToJSON = maybe [] vendorSpecificToJSON'
  where
    vendorSpecificToJSON' :: VendorSpecific -> [Pair]
    vendorSpecificToJSON' vs = [(fromText (propName vs), toJSON vs)]

    propName :: VendorSpecific -> Text
    propName = \case
      ChromeOptions {} -> "goog:chromeOptions"
      FirefoxOptions {} -> "moz:firefoxOptions"
      SafariOptions {} -> "safari:options"

-- ToJSON Instances for Custom Types
instance ToJSON UnhandledPromptBehavior where
  toJSON :: UnhandledPromptBehavior -> Value
  toJSON = \case
    Dismiss -> "dismiss"
    Accept -> "accept"
    DismissAndNotify -> "dismiss and notify"
    AcceptAndNotify -> "accept and notify"
    Ignore -> "ignore"

instance ToJSON PageLoadStrategy where
  toJSON :: PageLoadStrategy -> Value
  toJSON = \case
    None -> "none"
    Eager -> "eager"
    Normal -> "normal"

instance ToJSON BrowserName where
  toJSON :: BrowserName -> Value
  toJSON = \case
    Chrome -> "chrome"
    Firefox -> "firefox"
    Safari -> "safari"
    Edge -> "edge"
    InternetExplorer -> "internet explorer"

instance ToJSON PlatformName where
  toJSON :: PlatformName -> Value
  toJSON = \case
    Windows -> "windows"
    Mac -> "mac"
    Linux -> "linux"
    Android -> "android"
    IOS -> "ios"

instance FromJSON UnhandledPromptBehavior where
  parseJSON :: Value -> Parser UnhandledPromptBehavior
  parseJSON = withText "UnhandledPromptBehavior" $ \case
    "dismiss" -> pure Dismiss
    "accept" -> pure Accept
    "dismiss and notify" -> pure DismissAndNotify
    "accept and notify" -> pure AcceptAndNotify
    "ignore" -> pure Ignore
    other -> fail $ "UnhandledPromptBehavior: " <> show other

instance FromJSON PageLoadStrategy where
  parseJSON :: Value -> Parser PageLoadStrategy
  parseJSON = withText "PageLoadStrategy" $ \case
    "none" -> pure None
    "eager" -> pure Eager
    "normal" -> pure Normal
    _ -> fail "Invalid PageLoadStrategy"

instance FromJSON BrowserName where
  parseJSON :: Value -> Parser BrowserName
  parseJSON = withText "BrowserName" $ \case
    "chrome" -> pure Chrome
    "firefox" -> pure Firefox
    "safari" -> pure Safari
    "edge" -> pure Edge
    "internet explorer" -> pure InternetExplorer
    _ -> fail "Invalid BrowserName"

instance FromJSON PlatformName where
  parseJSON :: Value -> Parser PlatformName
  parseJSON = withText "PlatformName" $ \case
    "windows" -> pure Windows
    "mac" -> pure Mac
    "linux" -> pure Linux
    "android" -> pure Android
    "ios" -> pure IOS
    _ -> fail "Invalid PlatformName"

-- FromJSON Instances for Data Structures
instance FromJSON StandardCapabilities where
  parseJSON :: Value -> Parser StandardCapabilities
  parseJSON = withObject "Capabilities" $ \v ->
    -- TODO: make by name
    MkStandardCapabilities
      <$> v .: "browserName"
      <*> v .:? "browserVersion"
      <*> v .:? "platformName"
      <*> v .:? "acceptInsecureCerts"
      <*> v .:? "pageLoadStrategy"
      <*> v .:? "proxy"
      <*> v .:? "timeouts"
      <*> v .:? "strictFileInteractability"
      <*> v .:? "unhandledPromptBehavior"
      <*> parseVendorSpecific v

parseVendorSpecific :: Object -> Parser (Maybe VendorSpecific)
parseVendorSpecific v =
  v
    .:? "goog:chromeOptions"
    <|> v
    .:? "moz:firefoxOptions"
    <|> v
    .:? "safari:options"

data Timeouts = MkTimeouts
  { implicit :: Maybe Int, -- field order needs to be the same as FromJSON below
    pageLoad :: Maybe Int,
    script :: Maybe Int
  }
  deriving (Show, Generic, Eq)
 
instance FromJSON Timeouts where
  parseJSON :: Value -> Parser Timeouts
  parseJSON = withObject "Timeouts" $ \v ->
    MkTimeouts
      <$> v .:? "implicit"
      <*> v .:? "pageLoad"
      <*> v .:? "script"

instance ToJSON Timeouts where
  toJSON :: Timeouts -> Value
  toJSON MkTimeouts {..} =
    object
      [ "implicit" .= implicit,
        "pageLoad" .= pageLoad,
        "script" .= script
      ]
