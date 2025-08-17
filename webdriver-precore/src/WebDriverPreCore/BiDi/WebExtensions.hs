{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module WebDriverPreCore.BiDi.WebExtensions where

import Data.Aeson (FromJSON, ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.Internal.AesonUtils (enumCamelCase)
import Prelude (Eq, Show)

{- prompt
create types to represent the remote end for storage as prer the cddl in this file:

1. preface singleton data constructors (ie the constructor for types with only one type constructor) with Mk
2. use newtypes where possible
3. ordering - order types such that types that are used by a type are declared immediately below that type in the order they are used
4. derive Show, Eq and Generic for all types
5. use Text rather than String
5. use the cddl in this file remote first under the -- ######### Remote ######### header
7. Avoid using Parameters suffix in type and data constructor names
8. leave this comment at the top of the file
-}

-- ######### Remote #########

-- WebExtensionCommand represents possible commands
data WebExtensionCommand
  = Install WebExtensionData
  | Uninstall WebExtension
  deriving (Show, Eq, Generic)

-- WebExtension type
newtype WebExtension = MkWebExtension Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- ExtensionData represents different ways to provide extension data
data WebExtensionData
  = ExtensionPath Text
  | ExtensionArchivePath Text
  | ExtensionBase64Encoded Text
  deriving (Show, Eq, Generic)

-- Path type for extension data
data WebExtensionPath = MkWebExtensionPath
  { path :: Text
  }
  deriving (Show, Eq, Generic)

-- ArchivePath type for extension data
data WebExtensionArchivePath = MkWebExtensionArchivePath
  { path :: Text
  }
  deriving (Show, Eq, Generic)

-- Base64Encoded type for extension data
data WebExtensionBase64Encoded = MkWebExtensionBase64Encoded
  { value :: Text
  }
  deriving (Show, Eq, Generic)

-- ######### Remote #########

-- | Represents a command to install a web extension
data WebExtensionResult = WebExtensionInstallResult
  { extension :: WebExtension
  }
  deriving (Show, Eq, Generic, ToJSON)

-- ToJSON instances
instance ToJSON WebExtensionCommand where
  toJSON = enumCamelCase

instance ToJSON WebExtensionData where
  toJSON :: WebExtensionData -> Value
  toJSON = enumCamelCase

instance ToJSON WebExtensionPath where
  toJSON :: WebExtensionPath -> Value
  toJSON (MkWebExtensionPath path) =
    object
      [ "type" .= "path",
        "path" .= path
      ]

instance ToJSON WebExtensionArchivePath where
  toJSON :: WebExtensionArchivePath -> Value
  toJSON (MkWebExtensionArchivePath path) =
    object
      [ "type" .= "archivePath",
        "path" .= path
      ]

instance ToJSON WebExtensionBase64Encoded where
  toJSON :: WebExtensionBase64Encoded -> Value
  toJSON (MkWebExtensionBase64Encoded value) =
    object
      [ "type" .= "base64",
        "value" .= value
      ]

-- FromJSON instances for WebExtensions module
instance FromJSON WebExtensionResult
instance FromJSON WebExtensionData
instance FromJSON WebExtensionArchivePath
instance FromJSON WebExtensionBase64Encoded
