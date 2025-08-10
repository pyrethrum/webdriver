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

-- ######### Local #########

-- TODO: check exceptions eg test with unsupported command - currently not getting to main thread
jsonCommand :: Text -> Object -> JSUInt -> Value
jsonCommand methodName params id =
  object
    [ "id" .= id,
      "method" .= methodName,
      "params" .= params
    ]

-- TODO: check exceptions eg test with unsupported command - currently not getting to main thread
baseCommand :: (BiDiMethod a, ToJSON a) => a -> Maybe Object -> JSUInt -> Value
baseCommand cmd extensions =
  jsonCommand (bidiMethod cmd) $ maybe cmdObj (cmdObj <>) extensions 
  where
    cmdObj = objectOrThrow "CommandData will always be an Object" cmd

command :: (BiDiMethod a, ToJSON a) => a -> JSUInt -> Value
command cmd = baseCommand cmd Nothing

extendedCommand :: (BiDiMethod a, ToJSON a) => a -> Object -> JSUInt -> Value
extendedCommand cmd = baseCommand cmd . Just
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

