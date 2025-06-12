module WebDriverPreCore.BiDi.Storage where

{-
create types to represent the remote and local end for storage:

1. preface singleton data constructors (ie the constructor for types with only one type constructor) with Mk
2. use newtypes where possible
3. ordering - order types such that types that are used by a type are declared immediately below that type in the order they are used
4. derive Show, Eq and Generic for all types
5. use Text rather than String
5. use the cddl in this file remote first under the -- ######### Remote ######### header 
  then local under the -- ######### Local ######### header
7. Avoid using Parameters suffix in type and data constructor names
8. leave this comment at the top of the file
-}

import GHC.Generics (Generic)
import Data.Text (Text)
import WebDriverPreCore.BiDi.Network qualified as Network
import  WebDriverPreCore.BiDi.BrowsingContext qualified as BrowsingContext
import Prelude (Show, Eq, Maybe, Int, Bool)

-- ######### Remote #########

-- | Represents a storage result
data StorageResult
  = DeleteCookiesResult DeleteCookiesResult
  | GetCookiesResult GetCookiesResult
  | SetCookieResult SetCookieResult
  deriving (Show, Eq, Generic)

-- | Partition key for storage operations
data PartitionKey = MkPartitionKey
  { userContext :: Maybe Text
  , sourceOrigin :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Result of getting cookies
data GetCookiesResult = MkGetCookiesResult
  { cookies :: [Network.Cookie]
  , partitionKey :: PartitionKey
  } deriving (Show, Eq, Generic)

-- | Result of setting a cookie
newtype SetCookieResult = MkSetCookieResult
  { partitionKey :: PartitionKey
  } deriving (Show, Eq, Generic)

-- | Result of deleting cookies
newtype DeleteCookiesResult = MkDeleteCookiesResult
  { partitionKey :: PartitionKey
  } deriving (Show, Eq, Generic)

-- ######### Local #########

-- | Storage commands
data StorageCommand
  = DeleteCookies DeleteCookies
  | GetCookies GetCookies
  | SetCookie SetCookie
  deriving (Show, Eq, Generic)

-- | Parameters for getting cookies
data GetCookies = MkGetCookies
  { filter :: Maybe CookieFilter
  , partition :: Maybe PartitionDescriptor
  } deriving (Show, Eq, Generic)

-- | Filter for cookie operations
data CookieFilter = MkCookieFilter
  { name :: Maybe Text
  , value :: Maybe Network.BytesValue
  , domain :: Maybe Text
  , path :: Maybe Text
  , size :: Maybe Int
  , httpOnly :: Maybe Bool
  , secure :: Maybe Bool
  , sameSite :: Maybe Network.SameSite
  , expiry :: Maybe Int
  } deriving (Show, Eq, Generic)

-- | Descriptor for a partition
data PartitionDescriptor
  = BrowsingContextPartition BrowsingContextPartitionDescriptor
  | StorageKeyPartition StorageKeyPartitionDescriptor
  deriving (Show, Eq, Generic)

-- | Browsing context partition descriptor
newtype BrowsingContextPartitionDescriptor = MkBrowsingContextPartitionDescriptor
  { context :: BrowsingContext.BrowsingContextId
  } deriving (Show, Eq, Generic)

-- | Storage key partition descriptor
data StorageKeyPartitionDescriptor = MkStorageKeyPartitionDescriptor
  { userContext :: Maybe Text
  , sourceOrigin :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Parameters for setting a cookie
data SetCookie = MkSetCookie
  { cookie :: PartialCookie
  , partition :: Maybe PartitionDescriptor
  } deriving (Show, Eq, Generic)

-- | Partial cookie for setting
data PartialCookie = MkPartialCookie
  { name :: Text
  , value :: Network.BytesValue
  , domain :: Text
  , path :: Maybe Text
  , httpOnly :: Maybe Bool
  , secure :: Maybe Bool
  , sameSite :: Maybe Network.SameSite
  , expiry :: Maybe Int
  } deriving (Show, Eq, Generic)

-- | Parameters for deleting cookies
data DeleteCookies = MkDeleteCookies
  { filter :: Maybe CookieFilter
  , partition :: Maybe PartitionDescriptor
  } deriving (Show, Eq, Generic)

