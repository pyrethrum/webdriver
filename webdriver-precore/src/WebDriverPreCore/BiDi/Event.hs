module WebDriverPreCore.BiDi.Event where

import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextEvent)
import WebDriverPreCore.BiDi.CoreTypes (EmptyResult)
import WebDriverPreCore.BiDi.Input (FileDialogOpened)
import WebDriverPreCore.BiDi.Log (Entry)
import Prelude

-- typ :: Text, -- "event"
data Event = MkEvent
  { eventData :: EventData,
    extensions :: EmptyResult
  }
  deriving (Show, Generic)

data EventData
  = BrowsingContextEvent BrowsingContextEvent
  | -- | InputEvent InputEvent
    InputEvent FileDialogOpened
  | -- method: "log.entryAdded"
    LogEvent Entry
  deriving
    ( Show,
      Generic
    )

{-
{
  "type": "event",
  "method": "module.eventName",
  "params": {
    // Event-specific parameters
  }
}


Event = {
  type: "event",
  EventData,
  Extensible
}

{
  "type": "event", 
  "method": "browsingContext.navigationStarted",
  "params": {
    "context": "context-id",
    "navigation": "navigation-id", 
    "timestamp": 1234567890,
    "url": "https://example.com"
  }
}

{
  "type": "event",
  "method": "log.entryAdded", 
  "params": {
    "level": "info",
    "text": "Hello world",
    "timestamp": 1234567890,
    "source": {
      "realm": "realm-id",
      "context": "context-id"
    }
  }
}

-}