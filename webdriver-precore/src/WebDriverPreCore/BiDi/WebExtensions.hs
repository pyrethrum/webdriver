module WebDriverPreCore.BiDi.WebExtensions where

import GHC.Generics (Generic)
import Data.Text (Text)
import Prelude (Show, Eq)

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
-- Note: webextensions module does not have a local end

-- WebExtensionCommand represents possible commands
data WebExtensionCommand 
   = Install WebExtensionData
   | Uninstall WebExtension
   deriving (Show, Eq, Generic)

-- WebExtension type
newtype WebExtension = MkWebExtension Text
   deriving (Show, Eq, Generic)

-- ExtensionData represents different ways to provide extension data
data WebExtensionData
   = ExtensionPath Text
   | ExtensionArchivePath Text
   | ExtensionBase64Encoded Text
   deriving (Show, Eq, Generic)

-- Path type for extension data
data WebExtensionPath = MkWebExtensionPath
   { dataType :: Text  -- "path"
   , path :: Text
   } deriving (Show, Eq, Generic)

-- ArchivePath type for extension data
data WebExtensionArchivePath = MkWebExtensionArchivePath
   { dataType :: Text  -- "archivePath"
   , path :: Text
   } deriving (Show, Eq, Generic)

-- Base64Encoded type for extension data
data WebExtensionBase64Encoded = MkWebExtensionBase64Encoded
   { dataType :: Text  -- "base64"
   , value :: Text
   } deriving (Show, Eq, Generic)

