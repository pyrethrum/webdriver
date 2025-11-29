module WebDriverPreCore.BiDi.Log
  ( -- * Log Level
    Level (..),

    -- * Log Entry Types
    BaseLogEntry (..),
    GenericLogEntry (..),
    ConsoleLogEntry (..),
    JavascriptLogEntry (..),
    LogEntry (..),
    LogEvent (..)
  )
where

import Control.Monad (MonadFail (..))
import Data.Aeson (FromJSON, Value (..), withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Functor ((<&>))
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import WebDriverPreCore.BiDi.Script (RemoteValue, Source, StackTrace)
import WebDriverPreCore.Internal.AesonUtils (fromJSONCamelCase, parseJSONOmitNothing)
import Prelude (Eq, Maybe, Semigroup (..), Show, ($), (<$>), (.))

-- ######### Local #########
-- Note: log module does not have a remote end

-- | Represents the log level
data Level = Debug | Info | Warn | Error
  deriving (Show, Eq, Generic)

instance FromJSON Level where
  parseJSON :: Value -> Parser Level
  parseJSON = fromJSONCamelCase

-- | Base structure for all log entries
data BaseLogEntry = MkBaseLogEntry
  { level :: Level,
    source :: Source,
    text :: Maybe Text,
    timestamp :: JSUInt,
    stackTrace :: Maybe StackTrace
  }
  deriving (Show, Eq, Generic)

instance FromJSON BaseLogEntry where
  parseJSON :: Value -> Parser BaseLogEntry
  parseJSON = parseJSONOmitNothing

-- | Generic log entry type
data GenericLogEntry = MkGenericLogEntry
  { level :: Level,
    source :: Source,
    text :: Maybe Text,
    timestamp :: JSUInt,
    stackTrace :: Maybe StackTrace,
    entryType :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GenericLogEntry where
  parseJSON :: Value -> Parser GenericLogEntry
  parseJSON = parseJSONOmitNothing

-- | Console log entry type
data ConsoleLogEntry = MkConsoleLogEntry
  { level :: Level,
    source :: Source,
    text :: Maybe Text,
    timestamp :: JSUInt,
    stackTrace :: Maybe StackTrace,
    method :: Text,
    args :: [RemoteValue]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ConsoleLogEntry where 
  parseJSON :: Value -> Parser ConsoleLogEntry
  parseJSON = parseJSONOmitNothing

-- | JavaScript log entry type
data JavascriptLogEntry = MkJavascriptLogEntry
  { level :: Level,
    source :: Source,
    text :: Maybe Text,
    timestamp :: JSUInt,
    stackTrace :: Maybe StackTrace
  }
  deriving (Show, Eq, Generic)

instance FromJSON JavascriptLogEntry where
  parseJSON :: Value -> Parser JavascriptLogEntry
  parseJSON = parseJSONOmitNothing

-- | Union type for all log entry types
data LogEntry
  = GenericEntry GenericLogEntry
  | ConsoleEntry ConsoleLogEntry
  | JavascriptEntry JavascriptLogEntry
  deriving (Show, Eq, Generic)

-- | Event wrapper for log entries (consistent with other event types)
newtype LogEvent = MkLogEvent LogEntry
  deriving (Show, Eq, Generic)

instance FromJSON LogEvent where
  parseJSON :: Value -> Parser LogEvent
  parseJSON = withObject "LogEvent" $ \o -> do
    params <- o .: "params"
    entryType <- params .: "type"
    let parsedPrms :: forall a b. (FromJSON a) => (a -> b) -> Parser b
        parsedPrms = (<&>) (parseJSON (Object params))
    case entryType of
      "generic" -> parsedPrms (MkLogEvent . GenericEntry)
      "console" -> parsedPrms (MkLogEvent . ConsoleEntry)
      "javascript" -> parsedPrms (MkLogEvent . JavascriptEntry)
      _ -> fail $ "Unknown log entry type: " <> entryType

instance FromJSON LogEntry where
  parseJSON :: Value -> Parser LogEntry
  parseJSON = withObject "LogEntry" $ \o -> do
    entryType <- o .: "type"
    let val = Object o
    case entryType of
      "generic" -> GenericEntry <$> parseJSON val
      "console" -> ConsoleEntry <$> parseJSON val
      "javascript" -> JavascriptEntry <$> parseJSON val
      _ -> fail $ "Unknown log entry type: " <> entryType
