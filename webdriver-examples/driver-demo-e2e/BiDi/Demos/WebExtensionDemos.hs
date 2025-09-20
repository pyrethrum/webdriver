module BiDi.Demos.WebExtensionDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.WebExtensions
import WebDriverPreCore.BiDi.Protocol
import WebDriverPreCore.Internal.Utils (txt)
import Data.Text (Text)
import Prelude hiding (log, putStrLn)

{-
WebExtension Module Commands (2 total):

1. webExtension.install - Installs a web extension in the remote end
2. webExtension.uninstall - Uninstalls a web extension from the remote end

WebExtension Module Types:
- webExtension.Extension - Web extension id within a remote end
- webExtension.ExtensionData - Union type for extension data sources
- webExtension.ExtensionPath - Extension specified by filesystem path
- webExtension.ExtensionArchivePath - Extension specified by archive path  
- webExtension.ExtensionBase64Encoded - Extension specified by base64 data

Key Concepts:
- Extension installation from various sources (path, archive, base64)
- Extension lifecycle management
- Temporary vs persistent installation
- Extension ID management and tracking
- Zip archive extraction and validation
- Cross-platform file system considerations

Complexity factors:
- Multiple installation methods (path, archivePath, base64)
- Zip archive processing and validation
- File system path resolution and security
- Extension manifest validation
- Installation error handling
- Platform-specific extension behavior
- Temporary installation cleanup
-}

-- TODO: Implement webExtension.install demo (path-based)
-- Demonstrates installing extension from filesystem path
-- Should show:
-- - Basic path-based installation
-- - Extension ID retrieval
-- - Installation validation
-- - Error handling for invalid paths

-- TODO: Implement webExtension.install demo (archive-based)
-- Demonstrates installing extension from zip archive
-- Should show:
-- - Archive path installation
-- - Zip file validation
-- - Extraction process
-- - Archive format error handling

-- TODO: Implement webExtension.install demo (base64-based)
-- Demonstrates installing extension from base64 data
-- Should show:
-- - Base64 encoded extension data
-- - Data decoding and validation
-- - Memory-based installation
-- - Encoding error handling

-- TODO: Implement webExtension.uninstall demo
-- Demonstrates removing installed extensions
-- Should show:
-- - Extension removal by ID
-- - Cleanup verification
-- - Error handling for non-existent extensions
-- - Multiple extension management

-- TODO: Implement extension validation demo
-- Demonstrates extension format validation
-- Should show:
-- - Manifest validation
-- - Required files checking
-- - Extension metadata inspection
-- - Invalid extension detection

-- TODO: Implement extension lifecycle demo
-- Demonstrates complete extension management workflow
-- Should show:
-- - Install → Verify → Use → Uninstall cycle
-- - Extension state tracking
-- - Temporary vs persistent behavior
-- - Browser restart considerations

-- Demo helper for extension file management
-- TODO: Implement helper functions for extension data preparation

-- Demo helper for extension validation
-- TODO: Implement helper functions for extension validation