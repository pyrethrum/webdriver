{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Spec.Capabilities
  ( FullCapabilities (..),
    Capabilities (..),
    UnhandledPromptBehavior (..),
    PageLoadStrategy (..),
    BrowserName (..),
    PlatformName (..),
    Proxy (..),
    Timeouts (..),
    VendorSpecific (..),
    SocksProxy (..),
    minCapabilities,
    minFullCapabilities,
    minFirefoxCapabilities,
    minChromeCapabilities,
  )
where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (Monad ((>>=)), MonadFail (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Object,
    ToJSON (toJSON),
    Value (Array),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
  )
import Data.Aeson.Key (fromText)
import Data.Aeson.Types
  ( Pair,
    Parser,
  )
import Data.Bool (Bool, bool)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (..), catMaybes, maybe)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Vector (fromList)
import GHC.Generics (Generic)
import GHC.Show (Show (..))
import WebDriverPreCore.Internal.Utils (opt)

{- references:
- https://https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities

 - https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities
 - https://mucsi96.gitbook.io/w3c-webdriver/capabilities

 -}

-- | 'FullCapabilities' is the object that is passed to webdriver to define the properties of the session via the 'Spec.newSession' function.
--   It is a combination of 'alwaysMatch' and 'firstMatch' properties. 
--
--   See [spec](https://https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
--
--   See also: 'Capabilities' and related constructors such as 'minCapabilities', 'minFullCapabilities', 'minFirefoxCapabilities' and 'minChromeCapabilities'
data FullCapabilities = MkFullCapabilities
  { 
    alwaysMatch :: Maybe Capabilities, -- ^ capabilities that are always matched
    firstMatch :: [Capabilities] -- ^ a list of capabilities that are matched in order, the first matching capabilities that matches the capabilities of the session is used
  }
  deriving (Show, Generic)


instance ToJSON FullCapabilities where
  toJSON :: FullCapabilities -> Value
  toJSON MkFullCapabilities {alwaysMatch, firstMatch} =
    object $
      [ "capabilities" .= (object $ catMaybes [opt "alwaysMatch" $ alwaysMatch]),
        "firstMatch" .= firstMatch'
      ]
    where
      firstMatch' :: Value
      firstMatch' = Array . fromList $ toJSON <$> firstMatch

instance FromJSON FullCapabilities where
  parseJSON :: Value -> Parser FullCapabilities
  parseJSON = 
    withObject "FullCapabilities" $ \v ->
    do
      capabilities <- v .: "capabilities"
      alwaysMatch <- capabilities .:? "alwaysMatch"
      firstMatch <- capabilities .: "firstMatch"
      pure MkFullCapabilities {..}

-- | Returns the minimal FullCapabilities object for a given browser
-- The browserName in the 'alwaysMatch' field is the only field populated
-- See [spec](https://https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
minFullCapabilities :: BrowserName -> FullCapabilities
minFullCapabilities browserName =
  MkFullCapabilities
    { alwaysMatch = Just $ minCapabilities browserName,
      firstMatch = []
    }

-- | Returns the minimal Capabilities object for a given browser
-- The browserName is the only field populated
-- See [spec](https://https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
minCapabilities :: BrowserName -> Capabilities
minCapabilities browserName =
  MkCapabilities
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

-- | Returns the minimal FullCapabilities object for Firefox
minFirefoxCapabilities :: FullCapabilities
minFirefoxCapabilities = minFullCapabilities Firefox

-- | Returns the minimal FullCapabilities object for Chrome
minChromeCapabilities :: FullCapabilities
minChromeCapabilities = minFullCapabilities Chrome

-- Custom Types for Enums
-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data UnhandledPromptBehavior
  = Dismiss
  | Accept
  | DismissAndNotify
  | AcceptAndNotify
  | Ignore
  deriving (Show, Generic)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data PageLoadStrategy
  = None'
  | Eager
  | Normal
  deriving (Show, Generic)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data BrowserName
  = Chrome
  | Firefox
  | Safari
  | Edge
  | InternetExplorer
  deriving (Show, Generic)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data PlatformName
  = Windows
  | Mac
  | Linux
  | Android
  | IOS
  deriving (Show, Generic)

-- | 'Capabilities' define the properties of the session and are passed to the webdriver
-- via fields of the 'FullCapabilities' object.
--
-- See [spec](https://https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
-- See also: 'FullCapabilities' and related constructors such as 'minCapabilities',
-- 'minFullCapabilities', 'minFirefoxCapabilities' and 'minChromeCapabilities'
data Capabilities = MkCapabilities
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

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#proxy)
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
    do
      socksProxy <- v .: "socksProxy"
      socksVersion <- v .: "socksVersion"
      pure SocksProxy {..}

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#proxy)
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
-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#extensions-0)
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
instance ToJSON Capabilities where
  toJSON :: Capabilities -> Value
  toJSON
    MkCapabilities
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
    None' -> "none"
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
    "none" -> pure None'
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
instance FromJSON Capabilities where
  parseJSON :: Value -> Parser Capabilities
  parseJSON = withObject "Capabilities" $ \v ->
    do
      browserName <- v .: "browserName"
      browserVersion <- v .:? "browserVersion"
      platformName <- v .:? "platformName"
      acceptInsecureCerts <- v .:? "acceptInsecureCerts"
      pageLoadStrategy <- v .:? "pageLoadStrategy"
      proxy <- v .:? "proxy"
      timeouts <- v .:? "timeouts"
      strictFileInteractability <- v .:? "strictFileInteractability"
      unhandledPromptBehavior <- v .:? "unhandledPromptBehavior"
      vendorSpecific <- parseVendorSpecific v
      pure MkCapabilities {..}

parseVendorSpecific :: Object -> Parser (Maybe VendorSpecific)
parseVendorSpecific v =
  v
    .:? "goog:chromeOptions"
    <|> v
    .:? "moz:firefoxOptions"
    <|> v
    .:? "safari:options"

-- | Timeouts in milliseconds
-- [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#timeouts)
data Timeouts = MkTimeouts
  { implicit :: Maybe Int, -- field order needs to be the same as FromJSON below
    pageLoad :: Maybe Int,
    script :: Maybe Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Timeouts where
  parseJSON :: Value -> Parser Timeouts
  parseJSON = withObject "Timeouts" $ \v ->
    do
      implicit <- v .:? "implicit"
      pageLoad <- v .:? "pageLoad"
      script <- v .:? "script"
      pure MkTimeouts {..}

instance ToJSON Timeouts where
  toJSON :: Timeouts -> Value
  toJSON MkTimeouts {..} =
    object
      [ "implicit" .= implicit,
        "pageLoad" .= pageLoad,
        "script" .= script
      ]
