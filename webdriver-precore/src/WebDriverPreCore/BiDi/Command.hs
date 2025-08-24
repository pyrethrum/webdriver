module WebDriverPreCore.BiDi.Command where

import Data.Aeson
  ( Object,
    ToJSON,
    Value (..),
    object,
    (.=),
  )
import Data.Text (Text)
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow)
import Prelude (Maybe (..), maybe, (<>), Show, Eq)
import Data.Aeson.KeyMap qualified as KM

data Command c r = MkCommand
  { method :: Text,
    params :: c,
    extended :: Maybe Object
  } deriving (Show, Eq)

mkCommand :: forall c r. Text -> c -> Command c r
mkCommand method params = MkCommand {method, params, extended = Nothing}

emptyCommand :: forall r. Text -> Command Object r
emptyCommand method = mkCommand method KM.empty

setExtension :: Command c r -> Object -> Command c r
setExtension MkCommand {method, params} extended = MkCommand {method, params, extended = Just extended}

commandValue :: (ToJSON c) => Command c r -> JSUInt -> Value
commandValue MkCommand {method, params, extended} id' =
    object
    [ "id" .= id',
      "method" .= method,
      "params" .= maybe cmdObj (cmdObj <>) extended
    ]
  where
    cmdObj = objectOrThrow "CommandData will always be an Object" params