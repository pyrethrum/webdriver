module WebDriverPreCore.BiDi.Log
  ( -- * Log Level
    Level (..),

    -- * Log Entry Types
    BaseLogEntry (..),
    GenericLogEntry (..),
    ConsoleLogEntry (..),
    JavascriptLogEntry (..),
    LogEntry (..)
  )
where

import Control.Monad (MonadFail (..))
import Data.Aeson (FromJSON, Value (..), withObject, (.:))
import Data.Aeson.Types (FromJSON (..), Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import WebDriverPreCore.BiDi.Script (RemoteValue, Source, StackTrace)
import WebDriverPreCore.Internal.AesonUtils (asObject, fromJSONCamelCase, objectOrThrow, parseJSONOmitNothing)
import Prelude (Eq, Maybe, Semigroup (..), Show, ($), (<$>))

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

instance FromJSON LogEntry where
  parseJSON :: Value -> Parser LogEntry
  parseJSON = withObject "LogEntry" $ \obj -> do
    -- params <- obj .: "params"
    entryType <- obj .: "type"
    -- let paramVal = Object params\
    let val = Object obj
    case entryType of
      "generic" -> GenericEntry <$> parseJSON val
      "console" -> ConsoleEntry <$> parseJSON val
      "javascript" -> JavascriptEntry <$> parseJSON val
      _ -> fail $ "Unknown log entry type: " <> entryType


  {-  

  {
    "method": "log.entryAdded",
    "params": {
        "args": [
            {
                "type": "string",
                "value": "Console log test page loaded"
            }
        ],
        "level": "info",
        "method": "log",
        "source": {
            "context": "60af8cb2-2bb0-4b17-a2e0-5bb1af2f1224",
            "realm": "0bcbcdba-74c5-4238-8e8d-4936e1d9a93d"
        },
        "text": "Console log test page loaded",
        "timestamp": 1760511242278,
        "type": "console"
    },
    "type": "event"
})
  
  -}
