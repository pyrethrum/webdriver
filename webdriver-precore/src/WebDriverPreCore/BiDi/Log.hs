module WebDriverPreCore.BiDi.Log where
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Script (Source, StackTrace, RemoteValue)
import Prelude (Show, Eq, Maybe)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)

-- ######### Local ######### 
-- Note: log module does not have a remote end

-- | Represents the log level
data Level = Debug | Info | Warn | Error
  deriving (Show, Eq, Generic)

-- | Base structure for all log entries
data BaseLogEntry = MkBaseLogEntry
  { level :: Level
  , source :: Source
  , text :: Maybe Text
  , timestamp :: JSUInt
  , stackTrace :: Maybe StackTrace
  } deriving (Show, Eq, Generic)

-- | Generic log entry type
data GenericLogEntry = MkGenericLogEntry
  { baseEntry :: BaseLogEntry
  , entryType :: Text
  } deriving (Show, Eq, Generic)

-- | Console log entry type
data ConsoleLogEntry = MkConsoleLogEntry
  { baseEntry :: BaseLogEntry
  , method :: Text
  , args :: [RemoteValue]
  } deriving (Show, Eq, Generic)

-- | JavaScript log entry type
newtype JavascriptLogEntry = MkJavascriptLogEntry
  { baseEntry :: BaseLogEntry
  } deriving (Show, Eq, Generic)

-- | Union type for all log entry types
data Entry
  = GenericEntry GenericLogEntry
  | ConsoleEntry ConsoleLogEntry
  | JavascriptEntry JavascriptLogEntry
  deriving (Show, Eq, Generic)

-- | Event emitted when a log entry is added
newtype EntryAdded = MkEntryAdded
  { params :: Entry
  } deriving (Show, Eq, Generic)

-- | Union type for all log events
newtype LogEvent = MkLogEvent
  { entryAdded :: EntryAdded
  } deriving (Show, Eq, Generic)

