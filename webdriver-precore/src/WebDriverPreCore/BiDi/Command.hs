module WebDriverPreCore.BiDi.Command where

import Data.Aeson
  ( Object,
    ToJSON,
    Value (..),
    object,
    (.=),
  )
import Data.Function ((&))
import Data.Text (Text)
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (bidiMethod), JSUInt)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow)
import Prelude (Maybe (..), maybe, ($), (.), (<>))

data Command c r = MkCommand
  { method :: Text,
    params :: c,
    extended :: Maybe Object
  }


-- TODO: check exceptions eg test with unsupported command - currently not getting to main thread
commandValue :: (ToJSON c) => Command c r -> JSUInt -> Value
commandValue MkCommand {method, params, extended} id' =
    object
    [ "id" .= id',
      "method" .= method,
      "params" .= maybe cmdObj (cmdObj <>) extended
    ]
  where
    cmdObj = objectOrThrow "CommandData will always be an Object" params

-- command :: (ToJSON a) => Text -> a -> JSUInt -> Value
-- command method cmd = baseCommand method cmd Nothing

-- extendedCommand :: (ToJSON a) => Text -> a -> Object -> JSUInt -> Value
-- extendedCommand method cmd = baseCommand method cmd . Just
-- Command types

-- data Command
--   = BrowserCommand BrowserCommand
--   | BrowsingContext BrowsingContextCommand
--   | EmulationCommand EmulationCommand
--   | Input InputCommand
--   | Network NetworkCommand
--   | Script ScriptCommand
--   | Session SessionCommand
--   | Storage StorageCommand
--   | WebExtension WebExtensionCommand
--   deriving (Show, Eq, Generic)

