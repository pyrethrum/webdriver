module WebDriverPreCore.BiDi.Command where

import Data.Aeson
  ( FromJSON,
    Object,
    ToJSON,
    Value (..),
    object,
    (.=),
  )
import Data.Aeson.Types (ToJSON (..))
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.Browser (BrowserCommand, BrowserResult)
import WebDriverPreCore.BiDi.BrowsingContext qualified as BC 
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextCommand, BrowsingContextEvent, BrowsingContextResult)
import WebDriverPreCore.BiDi.CoreTypes (BiDiMethod (bidiMethod), JSUInt)
import WebDriverPreCore.BiDi.Emulation (EmulationCommand)
import WebDriverPreCore.BiDi.Error (ErrorCode)
import WebDriverPreCore.BiDi.Input (FileDialogOpened, InputCommand)
import WebDriverPreCore.BiDi.Log (Entry)
import WebDriverPreCore.BiDi.Network (NetworkCommand)
import WebDriverPreCore.BiDi.Script (RemoteValue, ScriptCommand, Source, StackTrace)
import WebDriverPreCore.BiDi.Session (SessionCommand, SessionSubscriptionRequest, SessionUnsubscribeParameters)
import WebDriverPreCore.BiDi.Session qualified as S
import WebDriverPreCore.BiDi.Storage (StorageCommand)
import WebDriverPreCore.BiDi.WebExtensions (WebExtensionCommand)
import WebDriverPreCore.Internal.AesonUtils (objectOrThrow, parseObject)
import Prelude (Bool, Eq, Maybe (..), Show, error, maybe, ($), (.), (<$>), (<>))
import WebDriverPreCore.BiDi.Capabilities (Capabilities)

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
  jsonCommand (bidiMethod cmd) $ extensions & maybe cmdObj (cmdObj <>)
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

