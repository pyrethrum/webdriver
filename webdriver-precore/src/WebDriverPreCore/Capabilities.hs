{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Capabilities
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
    PerfLoggingPrefs (..),
    MobileEmulation (..),
    LogLevel (..),
    LogSettings (..),
    DeviceMetrics (..),
    minCapabilities,
    minFullCapabilities,
    minFirefoxCapabilities,
    minChromeCapabilities,
  )
where

import Control.Applicative (Applicative (..), asum)
import Control.Monad (Monad ((>>=)), MonadFail (..))
import Data.Aeson
  ( FromJSON (parseJSON),
    Key,
    KeyValue ((.=)),
    Object,
    ToJSON (toJSON),
    Value,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
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
    Value (..),
    omitNothingFields,
    parseField,
    parseFieldMaybe,
  )
import Data.Bool (Bool (..))
import Data.Enum (Enum)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Map.Strict (Map)
import Data.Maybe (Maybe (..), catMaybes, maybe)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Vector (fromList)
import GHC.Enum (Bounded)
import GHC.Float (Double)
import GHC.Generics (Generic)
import GHC.IO (FilePath)
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
  { -- | capabilities that are always matched
    alwaysMatch :: Maybe Capabilities,
    -- | a list of capabilities that are matched in order, the first matching capabilities that matches the capabilities of the session is used
    firstMatch :: [Capabilities]
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
  deriving (Show, Generic, Enum, Bounded, Eq)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data PageLoadStrategy
  = None'
  | Eager
  | Normal
  deriving (Show, Generic, Enum, Bounded, Eq)

instance ToJSON PageLoadStrategy where
  toJSON :: PageLoadStrategy -> Value
  toJSON = \case
    None' -> "none"
    Eager -> "eager"
    Normal -> "normal"

instance FromJSON PageLoadStrategy where
  parseJSON :: Value -> Parser PageLoadStrategy
  parseJSON = withText "PageLoadStrategy" $ \case
    "none" -> pure None'
    "eager" -> pure Eager
    "normal" -> pure Normal
    _ -> fail "Invalid PageLoadStrategy"

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data BrowserName
  = Chrome
  | Firefox
  | Safari
  | Edge
  | InternetExplorer
  deriving (Show, Generic, Enum, Bounded, Eq)

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#capabilities)
data PlatformName
  = Windows
  | Mac
  | Linux
  | Android
  | IOS
  deriving (Show, Generic, Enum, Bounded, Eq)

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
  deriving (Show, Generic, Eq)

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

instance FromJSON Capabilities where
  parseJSON :: Value -> Parser Capabilities
  parseJSON = withObject "Capabilities" $ \v ->
    do
      let m :: forall a. (FromJSON a) => Key -> Parser (Maybe a)
          m = parseFieldMaybe v
      browserName <- v .: "browserName"
      browserVersion <- m "browserVersion"
      platformName <- m "platformName"
      acceptInsecureCerts <- m "acceptInsecureCerts"
      pageLoadStrategy <- m "pageLoadStrategy"
      proxy <- m "proxy"
      timeouts <- m "timeouts"
      strictFileInteractability <- m "strictFileInteractability"
      unhandledPromptBehavior <- m "unhandledPromptBehavior"
      vendorSpecific <- parseVendorSpecific v
      pure MkCapabilities {..}

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#proxy)
data SocksProxy = MkSocksProxy
  { socksProxy :: Text,
    socksVersion :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON SocksProxy where
  toJSON :: SocksProxy -> Value
  toJSON MkSocksProxy {..} =
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
      pure MkSocksProxy {..}

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
  toJSON p =
    object $
      [ "proxyType" .= proxyType
      ]
        <> details
    where
      proxyType = case p of
        Direct -> "direct"
        AutoDetect -> "autodetect"
        System -> "system"
        Pac {} -> "pac"
        Manual {} -> "manual"
      details = case p of
        Direct -> []
        AutoDetect -> []
        System -> []
        Pac {..} -> ["proxyAutoconfigUrl" .= proxyAutoconfigUrl]
        Manual {..} ->
          catMaybes
            [ opt "ftpProxy" ftpProxy,
              opt "httpProxy" httpProxy,
              opt "sslProxy" sslProxy,
              opt "socksProxy" socksProxy,
              opt "noProxy" noProxy
            ]

instance FromJSON Proxy where
  parseJSON :: Value -> Parser Proxy
  parseJSON = withObject "Proxy" $
    \v ->
      v .: "proxyType" >>= \case
        String "direct" -> pure Direct
        String "autodetect" -> pure AutoDetect
        String "system" -> pure System
        String "pac" -> Pac <$> v .: "proxyAutoconfigUrl"
        String "manual" ->
          do
            ftpProxy <- v .:? "ftpProxy"
            httpProxy <- v .:? "httpProxy"
            sslProxy <- v .:? "sslProxy"
            socksProxy <- v .:? "socksProxy"
            noProxy <- v .:? "noProxy"
            pure Manual {..}
        _ -> fail "Invalid Proxy"

-- Vendor-Specific Capabilities

-- | [spec](https://www.w3.org/TR/2025/WD-webdriver2-20250210/#extensions-0)
data VendorSpecific
  = -- | Chrome-specific capabilities
    ChromeOptions
      { chromeArgs :: Maybe [Text],
        chromeBinary :: Maybe Text,
        chromeExtensions :: Maybe [Text], -- Base64-encoded
        chromeLocalState :: Maybe (Map Text Value), -- Local state preferences
        chromeMobileEmulation :: Maybe MobileEmulation,
        chromePrefs :: Maybe (Map Text Value), -- User preferences
        chromeDetach :: Maybe Bool, -- Keep browser running after driver exit
        chromeDebuggerAddress :: Maybe Text, -- Remote debugger address
        chromeExcludeSwitches :: Maybe [Text], -- Chrome switches to exclude
        chromeMinidumpPath :: Maybe FilePath, -- Crash dump directory
        chromePerfLoggingPrefs :: Maybe PerfLoggingPrefs,
        chromeWindowTypes :: Maybe [Text] -- Window types to create
      }
  | EdgeOptions
      { edgeArgs :: Maybe [Text],
        edgeBinary :: Maybe Text,
        edgeExtensions :: Maybe [Text], -- Base64-encoded
        edgeLocalState :: Maybe (Map Text Value), -- Local state preferences
        edgeMobileEmulation :: Maybe MobileEmulation,
        edgePrefs :: Maybe (Map Text Value), -- User preferences
        edgeDetach :: Maybe Bool, -- Keep browser running after driver exit
        edgeDebuggerAddress :: Maybe Text, -- Remote debugger address
        edgeExcludeSwitches :: Maybe [Text], -- Chrome switches to exclude
        edgeMinidumpPath :: Maybe FilePath, -- Crash dump directory
        edgePerfLoggingPrefs :: Maybe PerfLoggingPrefs,
        edgeWindowTypes :: Maybe [Text] -- Window types to create
      }
  | FirefoxOptions
      { firefoxArgs :: Maybe [Text],
        firefoxBinary :: Maybe Text,
        firefoxProfile :: Maybe Text, -- Base64-encoded profile
        firefoxLog :: Maybe LogSettings
      }
  | SafariOptions
      { safariAutomaticInspection :: Maybe Bool,
        safariAutomaticProfiling :: Maybe Bool
      }
  deriving (Show, Generic, Eq)

data PerfLoggingPrefs = MkPerfLoggingPrefs
  { enableNetwork :: Maybe Bool,
    enablePage :: Maybe Bool,
    enableTimeline :: Maybe Bool,
    traceCategories :: Maybe Text,
    bufferUsageReportingInterval :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON PerfLoggingPrefs where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON PerfLoggingPrefs where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

data MobileEmulation = MkMobileEmulation
  { deviceName :: Maybe Text,
    deviceMetrics :: Maybe DeviceMetrics,
    userAgent :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON MobileEmulation where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

instance ToJSON MobileEmulation where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- | Browser log levels as defined in vendor specs
data LogLevel
  = -- | Most verbose logging
    Trace
  | -- | Debug-level information
    Debug
  | -- | Configuration details
    Config
  | -- | General operational logs
    Info
  | -- | Potential issues
    Warning
  | -- | Recoverable errors
    Error
  | -- | Critical failures
    Fatal
  | -- | No logging
    Off
  deriving (Show, Eq, Enum, Bounded, Generic)

instance ToJSON LogLevel where
  toJSON :: LogLevel -> Value
  toJSON =
    String . \case
      Trace -> "trace"
      Debug -> "debug"
      Config -> "config"
      Info -> "info"
      Warning -> "warn"
      Error -> "error"
      Fatal -> "fatal"
      Off -> "off"

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $ \case
    "trace" -> pure Trace
    "debug" -> pure Debug
    "config" -> pure Config
    "info" -> pure Info
    "warn" -> pure Warning
    "error" -> pure Error
    "fatal" -> pure Fatal
    "off" -> pure Off
    other -> fail $ "Invalid log level: " <> show other

-- | Log settings structure for vendor capabilities
data LogSettings = MkLogSettings
  { level :: LogLevel
  }
  deriving (Show, Eq, Generic)

instance FromJSON LogSettings where
  parseJSON = withObject "LogSettings" $ \v ->
    do
      level <- v .: "level"
      pure MkLogSettings {..}

instance ToJSON LogSettings where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data DeviceMetrics = MkDeviceMetrics
  { width :: Int,
    height :: Int,
    pixelRatio :: Double,
    touch :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeviceMetrics where
  parseJSON = withObject "DeviceMetrics" $ \v ->
    do
      let m :: forall a. (FromJSON a) => Key -> Parser a
          m = parseField v
      width <- m "width"
      height <- m "height"
      pixelRatio <- m "pixelRatio"
      touch <- m "touch"
      pure MkDeviceMetrics {..}

instance ToJSON DeviceMetrics where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

-- | ToJSON Instance for VendorSpecific

-- ToJSON Instance for VendorSpecific
instance ToJSON VendorSpecific where
  toJSON :: VendorSpecific -> Value
  toJSON vs =
    object $ catMaybes props
    where
      props = case vs of
        ChromeOptions {..} ->
          [ opt "args" chromeArgs,
            opt "binary" chromeBinary,
            opt "extensions" chromeExtensions,
            opt "mobileEmulation" chromeMobileEmulation,
            opt "prefs" chromePrefs,
            opt "detach" chromeDetach,
            opt "debuggerAddress" chromeDebuggerAddress,
            opt "excludeSwitches" chromeExcludeSwitches,
            opt "minidumpPath" chromeMinidumpPath,
            opt "perfLoggingPrefs" chromePerfLoggingPrefs,
            opt "windowTypes" chromeWindowTypes
          ]
        EdgeOptions {..} ->
          [ opt "args" edgeArgs,
            opt "binary" edgeBinary,
            opt "extensions" edgeExtensions,
            opt "localState" edgeLocalState,
            opt "prefs" edgePrefs,
            opt "detach" edgeDetach,
            opt "debuggerAddress" edgeDebuggerAddress,
            opt "excludeSwitches" edgeExcludeSwitches,
            opt "minidumpPath" edgeMinidumpPath,
            opt "mobileEmulation" edgeMobileEmulation,
            opt "perfLoggingPrefs" edgePerfLoggingPrefs,
            opt "windowTypes" edgeWindowTypes
          ]
        FirefoxOptions {..} ->
          [ opt "args" firefoxArgs,
            opt "binary" firefoxBinary,
            opt "profile" firefoxProfile,
            opt "log" firefoxLog
          ]
        SafariOptions {..} ->
          [ opt "automaticInspection" safariAutomaticInspection,
            opt "automaticProfiling" safariAutomaticProfiling
          ]

vendorSpecificPropName :: VendorSpecific -> Text
vendorSpecificPropName = \case
  ChromeOptions {} -> "goog:chromeOptions"
  EdgeOptions {} -> "ms:edgeOptions"
  FirefoxOptions {} -> "moz:firefoxOptions"
  SafariOptions {} -> "safari:options"

vendorSpecificToJSON :: Maybe VendorSpecific -> [Pair]
vendorSpecificToJSON = maybe [] vendorSpecificToJSON'
  where
    vendorSpecificToJSON' :: VendorSpecific -> [Pair]
    vendorSpecificToJSON' vs = [(fromText (vendorSpecificPropName vs), toJSON vs)]

-- ToJSON Instances for Custom Types
instance ToJSON UnhandledPromptBehavior where
  toJSON :: UnhandledPromptBehavior -> Value
  toJSON = \case
    Dismiss -> "dismiss"
    Accept -> "accept"
    DismissAndNotify -> "dismiss and notify"
    AcceptAndNotify -> "accept and notify"
    Ignore -> "ignore"

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
parseVendorSpecific :: Object -> Parser (Maybe VendorSpecific)
parseVendorSpecific v =
  asum
    [ Just <$> (v .: "goog:chromeOptions" >>= parseChromeOptions),
      Just <$> (v .: "ms:edgeOptions" >>= parseEdgeOptions),
      Just <$> (v .: "moz:firefoxOptions" >>= parseFirefoxOptions),
      Just <$> (v .: "safari:options" >>= parseSafariOptions),
      pure Nothing
    ]
  where
    parseChromeOptions o = do
      chromeArgs <- m "args"
      chromeBinary <- m "binary"
      chromeExtensions <- m "extensions"
      chromeLocalState <- m "localState" -- Local state preferences
      chromeMobileEmulation <- m "mobileEmulation"
      chromePrefs <- m "prefs"
      chromeDetach <- m "detach"
      chromeDebuggerAddress <- m "debuggerAddress"
      chromeExcludeSwitches <- m "excludeSwitches"
      chromeMinidumpPath <- m "minidumpPath"
      chromePerfLoggingPrefs <- m "perfLoggingPrefs"
      chromeWindowTypes <- m "windowTypes"
      pure $ ChromeOptions {..}
      where
        m :: forall a. (FromJSON a) => Key -> Parser (Maybe a)
        m = parseFieldMaybe o

    parseEdgeOptions o = do
      edgeArgs <- m "args"
      edgeBinary <- m "binary"
      edgeExtensions <- m "extensions"
      edgeLocalState <- m "localState"
      edgePrefs <- m "prefs"
      edgeDetach <- m "detach"
      edgeDebuggerAddress <- m "debuggerAddress"
      edgeExcludeSwitches <- m "excludeSwitches"
      edgeMinidumpPath <- m "minidumpPath"
      edgeMobileEmulation <- m "mobileEmulation"
      edgePerfLoggingPrefs <- m "perfLoggingPrefs"
      edgeWindowTypes <- m "windowTypes"
      pure EdgeOptions {..}
      where
        m :: forall a. (FromJSON a) => Key -> Parser (Maybe a)
        m = parseFieldMaybe o

    parseFirefoxOptions o = do
      firefoxArgs <- m "args"
      firefoxBinary <- m "binary"
      firefoxProfile <- m "profile"
      firefoxLog <- m "log"
      pure FirefoxOptions {..}
      where
        m :: forall a. (FromJSON a) => Key -> Parser (Maybe a)
        m = parseFieldMaybe o

    parseSafariOptions o = do
      safariAutomaticInspection <- m "automaticInspection"
      safariAutomaticProfiling <- m "automaticProfiling"
      pure SafariOptions {..}
      where
        m = parseFieldMaybe o

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
