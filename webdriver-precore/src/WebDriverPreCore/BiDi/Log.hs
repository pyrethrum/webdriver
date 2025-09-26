module WebDriverPreCore.BiDi.Log (
  -- * Log Level
  Level (..),

  -- * Log Entry Types
  BaseLogEntry (..),
  GenericLogEntry (..),
  ConsoleLogEntry (..),
  JavascriptLogEntry (..),
  LogEntry (..),
  EntryAdded (..),
  LogEvent (..),

) where
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Script (Source, StackTrace, RemoteValue)
import Prelude (Show, Eq, Maybe, Semigroup (..), (<$>), ($))
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import Data.Aeson (FromJSON, Value (..), (.:))
import Data.Aeson.Types (Parser, FromJSON (..))
import Control.Monad (MonadFail(..))
import WebDriverPreCore.Internal.AesonUtils (fromJSONCamelCase, parseJSONOmitNothing)

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
  { level :: Level
  , source :: Source
  , text :: Maybe Text
  , timestamp :: JSUInt
  , stackTrace :: Maybe StackTrace
  } deriving (Show, Eq, Generic)

instance FromJSON BaseLogEntry where
  parseJSON :: Value -> Parser BaseLogEntry
  parseJSON = parseJSONOmitNothing


-- | Generic log entry type
data GenericLogEntry = MkGenericLogEntry
  { baseEntry :: BaseLogEntry
  , entryType :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON GenericLogEntry 

-- | Console log entry type
data ConsoleLogEntry = MkConsoleLogEntry
  { baseEntry :: BaseLogEntry
  , method :: Text
  , args :: [RemoteValue]
  } deriving (Show, Eq, Generic)

instance FromJSON ConsoleLogEntry 


-- | JavaScript log entry type
newtype JavascriptLogEntry = MkJavascriptLogEntry
  { baseEntry :: BaseLogEntry
  } deriving (Show, Eq, Generic)

instance FromJSON JavascriptLogEntry

-- | Union type for all log entry types
data LogEntry
  = GenericEntry GenericLogEntry
  | ConsoleEntry ConsoleLogEntry
  | JavascriptEntry JavascriptLogEntry
  deriving (Show, Eq, Generic)


instance FromJSON LogEntry where
  parseJSON :: Value -> Parser LogEntry
  parseJSON = \case
    v@(Object obj) -> do
      entryType <- obj .: "type"
      case entryType of
        "generic" -> GenericEntry <$> parseJSON v
        "console" -> ConsoleEntry <$> parseJSON v
        "javascript" -> JavascriptEntry <$> parseJSON v
        _ -> fail $ "Unknown log entry type: " <> entryType
    _ -> fail "Expected log entry to be an object"  

-- | Event emitted when a log entry is added
newtype EntryAdded = MkEntryAdded
  { params :: LogEntry
  } deriving (Show, Eq, Generic)

-- | Union type for all log events
newtype LogEvent = MkLogEvent
  { entryAdded :: EntryAdded
  } deriving (Show, Eq, Generic)

