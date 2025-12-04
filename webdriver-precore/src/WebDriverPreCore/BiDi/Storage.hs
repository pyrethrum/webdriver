{-# LANGUAGE DeriveAnyClass #-}

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

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import WebDriverPreCore.BiDi.CoreTypes (BrowsingContext, UserContext)
import WebDriverPreCore.BiDi.Network qualified as Network
import WebDriverPreCore.Internal.AesonUtils (fromJSONCamelCase, opt, toJSONOmitNothing)

-- ######### Remote #########

-- | Partition key for storage operations
data PartitionKey = MkPartitionKey
  { userContext :: Maybe Text,
    sourceOrigin :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PartitionKey

instance ToJSON PartitionKey

-- | Result of getting cookies
data GetCookiesResult = MkGetCookiesResult
  { cookies :: [Network.Cookie],
    partitionKey :: PartitionKey
  }
  deriving (Show, Eq, Generic)

instance FromJSON GetCookiesResult

-- | Result of setting a cookie
newtype SetCookieResult = MkSetCookieResult
  { partitionKey :: PartitionKey
  }
  deriving (Show, Eq, Generic)

instance FromJSON SetCookieResult

-- | Result of deleting cookies
newtype DeleteCookiesResult = MkDeleteCookiesResult
  { partitionKey :: PartitionKey
  }
  deriving (Show, Eq, Generic)

instance FromJSON DeleteCookiesResult

-- ######### Local #########

-- | Parameters for getting cookies
data GetCookies = MkGetCookies
  { filter :: Maybe CookieFilter,
    partition :: Maybe PartitionDescriptor
  }
  deriving (Show, Eq, Generic)

instance ToJSON GetCookies where
  toJSON :: GetCookies -> Value
  toJSON = toJSONOmitNothing

-- | Filter for cookie operations
data CookieFilter = MkCookieFilter
  { name :: Maybe Text,
    value :: Maybe Network.BytesValue,
    domain :: Maybe Text,
    path :: Maybe Text,
    size :: Maybe Int,
    httpOnly :: Maybe Bool,
    secure :: Maybe Bool,
    sameSite :: Maybe Network.SameSite,
    expiry :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON CookieFilter where
  parseJSON :: Value -> Parser CookieFilter
  parseJSON = fromJSONCamelCase

instance ToJSON CookieFilter where
  toJSON :: CookieFilter -> Value
  toJSON = toJSONOmitNothing

-- | Descriptor for a partition
data PartitionDescriptor
  = BrowsingContextPartition
      { context :: BrowsingContext
      }
  | StorageKeyPartition
      { userContext :: Maybe UserContext,
        sourceOrigin :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON PartitionDescriptor where
  parseJSON :: Value -> Parser PartitionDescriptor
  parseJSON = withObject "PartitionDescriptor" $ \o -> do
    typ <- o .: "type"
    case typ of
      "context" -> BrowsingContextPartition <$> o .: "context"
      "storageKey" -> StorageKeyPartition <$> o .: "userContext" <*> o .: "sourceOrigin"
      _ -> fail $ "Unknown partition type: " ++ show typ

instance ToJSON PartitionDescriptor where
  toJSON :: PartitionDescriptor -> Value
  toJSON = \case
    BrowsingContextPartition ctx ->
      object
        [ "type" .= "context",
          "context" .= ctx
        ]
    StorageKeyPartition userCtx srcOrigin ->
      object $
        [ "type" .= "storageKey"
        ]
          <> catMaybes
            [ opt "userContext" userCtx,
              opt "sourceOrigin" srcOrigin
            ]

-- | Parameters for setting a cookie
data SetCookie = MkSetCookie
  { cookie :: PartialCookie,
    partition :: Maybe PartitionDescriptor
  }
  deriving (Show, Eq, Generic)

instance ToJSON SetCookie where
  toJSON :: SetCookie -> Value
  toJSON = toJSONOmitNothing

-- | Partial cookie for setting
data PartialCookie = MkPartialCookie
  { name :: Text,
    value :: Network.BytesValue,
    domain :: Text,
    path :: Maybe Text,
    httpOnly :: Maybe Bool,
    secure :: Maybe Bool,
    sameSite :: Maybe Network.SameSite,
    expiry :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON PartialCookie where
  toJSON :: PartialCookie -> Value
  toJSON = toJSONOmitNothing

-- | Parameters for deleting cookies
data DeleteCookies = MkDeleteCookies
  { filter :: Maybe CookieFilter,
    partition :: Maybe PartitionDescriptor
  }
  deriving (Show, Eq, Generic)

instance ToJSON DeleteCookies where
  toJSON :: DeleteCookies -> Value
  toJSON = toJSONOmitNothing
