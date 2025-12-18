{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module WebDriverPreCore.BiDi.WebExtensions
  ( WebExtensionID (..),
    WebExtensionInstall (..),
    WebExtensionUninstall (..),
    WebExtensionResult (..),
  )
where

import Data.Aeson (FromJSON, ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

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

-- WebExtension type
newtype WebExtensionID = MkWebExtensionID Text
  deriving newtype (Show, Eq, FromJSON, ToJSON)



-- ExtensionData represents different ways to provide extension data
data WebExtensionInstall
  = ExtensionPath Text
  | ExtensionArchivePath Text
  | ExtensionBase64Encoded Text
  deriving (Show, Eq, Generic)

instance ToJSON WebExtensionInstall where
  toJSON :: WebExtensionInstall -> Value
  toJSON ex =
    object
      [ "method" .= "webExtension.install",
        "extensionData" .= extensionData
      ]
    where
      extensionData = case ex of
        ExtensionPath path ->
          object
            [ "type" .= "path",
              "path" .= path
            ]
        ExtensionArchivePath path ->
          object
            [ "type" .= "archivePath",
              "path" .= path
            ]
        ExtensionBase64Encoded value ->
          object
            [ "type" .= "base64",
              "value" .= value
            ]

newtype WebExtensionUninstall = MkWebExtensionUninstall
  { extension :: WebExtensionID
  }
  deriving (Show, Eq, Generic)

instance ToJSON WebExtensionUninstall where
  toJSON :: WebExtensionUninstall -> Value
  toJSON (MkWebExtensionUninstall extId) =
    object
      [ "method" .= "webExtension.uninstall",
        "extension" .= extId
      ]


-- ######### Local #########

-- | Represents a command to install a web extension
data WebExtensionResult = WebExtensionInstallResult
  { extension :: WebExtensionID
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON WebExtensionResult
