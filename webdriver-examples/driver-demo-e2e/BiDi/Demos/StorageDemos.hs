module BiDi.Demos.StorageDemos where

import BiDi.BiDiRunner (Commands (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.Storage
import WebDriverPreCore.BiDi.Network (Cookie)
import WebDriverPreCore.BiDi.BrowsingContext (BrowsingContextId)
import WebDriverPreCore.BiDi.Protocol
import WebDriverPreCore.Internal.Utils (txt)
import Data.Text (Text)
import Prelude hiding (log, putStrLn)

{-
Storage Module Commands (3 total):

1. storage.getCookies - Retrieves zero or more cookies which match a set of provided parameters
2. storage.setCookie - Sets a cookie in the specified storage partition  
3. storage.deleteCookies - Deletes cookies from the specified storage partition

Storage Module Types:
- storage.PartitionKey - Uniquely identifies a storage partition
- storage.CookieFilter - Filter parameters for cookie matching
- storage.BrowsingContextPartitionDescriptor - Context-based partition descriptor
- storage.StorageKeyPartitionDescriptor - Storage key-based partition descriptor
- storage.PartitionDescriptor - Union of partition descriptor types

Key Concepts:
- Storage partitions: Namespaces for persistent data (cookies, localStorage, etc.)
- Partition keys: Maps that uniquely identify storage partitions
- Cookie filtering: Matching cookies based on name, value, domain, path, etc.
- Cross-origin storage considerations
- User context isolation

Complexity factors:
- Multiple partition types and descriptors
- Complex cookie filtering logic
- Storage partition key expansion and validation
- Cookie serialization/deserialization
- Integration with browsing contexts and user contexts
-}

-- TODO: Implement storage.getCookies demo
-- Demonstrates retrieving cookies with various filter criteria
-- Should show:
-- - Getting all cookies (no filter)
-- - Filtering by name, domain, path
-- - Context-based vs storage-key-based partitions
-- - Cookie property inspection (httpOnly, secure, sameSite, etc.)

-- TODO: Implement storage.setCookie demo  
-- Demonstrates setting cookies in different storage partitions
-- Should show:
-- - Basic cookie creation
-- - Setting cookies with various attributes (domain, path, secure, etc.)
-- - Cross-origin cookie considerations
-- - User context isolation

-- TODO: Implement storage.deleteCookies demo
-- Demonstrates deleting cookies with filter criteria
-- Should show:
-- - Deleting specific cookies by name
-- - Bulk deletion with filters
-- - Partition-specific deletion
-- - Verification of deletion success

-- TODO: Implement partition key demo
-- Demonstrates different ways to specify storage partitions
-- Should show:
-- - Context-based partitions (using browsing context)
-- - Storage-key-based partitions (using userContext, sourceOrigin)
-- - Default partition behavior
-- - Cross-context storage isolation

-- TODO: Implement cookie filter demo
-- Demonstrates various cookie filtering options
-- Should show:
-- - Name-based filtering
-- - Domain and path filtering  
-- - Security attribute filtering (httpOnly, secure)
-- - SameSite policy filtering
-- - Size and expiry filtering
-- - Complex multi-criteria filters

-- Demo helper for partition descriptor creation
-- TODO: Implement helper functions for creating partition descriptors

-- Demo helper for cookie filter creation  
-- TODO: Implement helper functions for creating cookie filters

-- Demo helper for cookie validation
-- TODO: Implement helper functions for validating cookie operations