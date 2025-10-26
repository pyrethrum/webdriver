module BiDi.Demos.StorageDemos where

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.CoreTypes (StringValue (MkStringValue))
import WebDriverPreCore.BiDi.Protocol
import WebDriverPreCore.BiDi.Storage
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

-- >>> runDemo storageGetCookiesDemo
storageGetCookiesDemo :: BiDiDemo
storageGetCookiesDemo =
  demo "Storage - Get Cookies" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Get all cookies (no filter)"
      let getAllCookies =
            MkGetCookies
              { filter = Nothing,
                partition = Just $ BrowsingContextPartition bc
              }
      result1 <- storageGetCookies getAllCookies
      logShow "All cookies" result1
      pause

      logTxt "Test 2: Get cookies with name filter"
      let nameFilter =
            MkCookieFilter
              { name = Just "test-cookie",
                value = Nothing,
                domain = Nothing,
                path = Nothing,
                size = Nothing,
                httpOnly = Nothing,
                secure = Nothing,
                sameSite = Nothing,
                expiry = Nothing
              }
      let filteredCookies =
            MkGetCookies
              { filter = Just nameFilter,
                partition = Just $ BrowsingContextPartition bc
              }
      result2 <- storageGetCookies filteredCookies
      logShow "Filtered cookies by name" result2
      pause

      logTxt "Test 3: Get secure cookies only"
      let secureFilter =
            MkCookieFilter
              { name = Nothing,
                value = Nothing,
                domain = Nothing,
                path = Nothing,
                size = Nothing,
                httpOnly = Nothing,
                secure = Just True,
                sameSite = Nothing,
                expiry = Nothing
              }
      let secureCookies =
            MkGetCookies
              { filter = Just secureFilter,
                partition = Just $ BrowsingContextPartition bc
              }
      result3 <- storageGetCookies secureCookies
      logShow "Secure cookies only" result3
      pause

-- >>> runDemo storageSetCookieDemo
storageSetCookieDemo :: BiDiDemo
storageSetCookieDemo =
  demo "Storage - Set Cookie" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Set basic cookie"
      let basicCookie =
            MkPartialCookie
              { name = "demo-cookie",
                value = TextBytesValue $ MkStringValue "demo-value",
                domain = "example.com",
                path = Nothing,
                httpOnly = Nothing,
                secure = Nothing,
                sameSite = Nothing,
                expiry = Nothing
              }
      let setCookieBasic =
            MkSetCookie
              { cookie = basicCookie,
                partition = Just $ BrowsingContextPartition bc
              }
      result1 <- storageSetCookie setCookieBasic
      logShow "Basic cookie set" result1
      pause

      logTxt "Test 2: Set secure HTTP-only cookie"
      let secureCookie =
            MkPartialCookie
              { name = "secure-cookie",
                value = TextBytesValue $ MkStringValue "secure-value",
                domain = "example.com",
                path = Just "/",
                httpOnly = Just True,
                secure = Just True,
                sameSite = Just Strict,
                expiry = Nothing
              }
      let setCookieSecure =
            MkSetCookie
              { cookie = secureCookie,
                partition = Just $ BrowsingContextPartition bc
              }
      result2 <- storageSetCookie setCookieSecure
      logShow "Secure cookie set" result2
      pause

      logTxt "Test 3: Set cookie with storage key partition"
      let partitionCookie =
            MkPartialCookie
              { name = "partition-cookie",
                value = TextBytesValue $ MkStringValue "partition-value",
                domain = "example.com",
                path = Nothing,
                httpOnly = Nothing,
                secure = Nothing,
                sameSite = Nothing,
                expiry = Nothing
              }
      let storageKeyPartition = StorageKeyPartition (Just $ MkUserContext "default") (Just "https://example.com")
      let setCookiePartition =
            MkSetCookie
              { cookie = partitionCookie,
                partition = Just storageKeyPartition
              }
      result3 <- storageSetCookie setCookiePartition
      logShow "Partition cookie set" result3
      pause

-- >>> runDemo storageDeleteCookiesDemo
storageDeleteCookiesDemo :: BiDiDemo
storageDeleteCookiesDemo =
  demo "Storage - Delete Cookies" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Delete cookies by name"
      let nameFilter =
            MkCookieFilter
              { name = Just "demo-cookie",
                value = Nothing,
                domain = Nothing,
                path = Nothing,
                size = Nothing,
                httpOnly = Nothing,
                secure = Nothing,
                sameSite = Nothing,
                expiry = Nothing
              }
      let deleteCookiesByName =
            MkDeleteCookies
              { filter = Just nameFilter,
                partition = Just $ BrowsingContextPartition bc
              }
      result1 <- storageDeleteCookies deleteCookiesByName
      logShow "Cookies deleted by name" result1
      pause

      logTxt "Test 2: Delete all cookies in partition"
      let deleteAllCookies =
            MkDeleteCookies
              { filter = Nothing,
                partition = Just $ BrowsingContextPartition bc
              }
      result2 <- storageDeleteCookies deleteAllCookies
      logShow "All cookies deleted" result2
      pause

      logTxt "Test 3: Delete secure cookies only"
      let secureFilter =
            MkCookieFilter
              { name = Nothing,
                value = Nothing,
                domain = Nothing,
                path = Nothing,
                size = Nothing,
                httpOnly = Nothing,
                secure = Just True,
                sameSite = Nothing,
                expiry = Nothing
              }
      let deleteSecureCookies =
            MkDeleteCookies
              { filter = Just secureFilter,
                partition = Just $ BrowsingContextPartition bc
              }
      result3 <- storageDeleteCookies deleteSecureCookies
      logShow "Secure cookies deleted" result3
      pause

-- >>> runDemo storagePartitionKeyDemo
storagePartitionKeyDemo :: BiDiDemo
storagePartitionKeyDemo =
  demo "Storage - Partition Key Management" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Test 1: Context-based partition"
      let contextPartition = BrowsingContextPartition bc
      let getCookiesContext =
            MkGetCookies
              { filter = Nothing,
                partition = Just contextPartition
              }
      result1 <- storageGetCookies getCookiesContext
      logShow "Context partition cookies" result1
      pause

      logTxt "Test 2: Storage key partition (default user context)"
      let storageKeyPartition1 =
            StorageKeyPartition
              { userContext = Just $ MkUserContext "default",
                sourceOrigin = Nothing
              }
      let getCookiesStorageKey1 =
            MkGetCookies
              { filter = Nothing,
                partition = Just storageKeyPartition1
              }
      result2 <- storageGetCookies getCookiesStorageKey1
      logShow "Storage key partition (default)" result2
      pause

      logTxt "Test 3: Storage key partition with custom user context"
      -- First create a custom user context
      customUserContext <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "Custom user context created" customUserContext
      
      let storageKeyPartition2 =
            StorageKeyPartition
              { userContext = Just customUserContext,
                sourceOrigin = Just "https://example.com"
              }
      let getCookiesStorageKey2 =
            MkGetCookies
              { filter = Nothing,
                partition = Just storageKeyPartition2
              }
      result3 <- storageGetCookies getCookiesStorageKey2
      logShow "Storage key partition with custom user context" result3
      pause

-- >>> runDemo storageCompleteWorkflowDemo
storageCompleteWorkflowDemo :: BiDiDemo
storageCompleteWorkflowDemo =
  demo "Storage - Complete Cookie Workflow" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Step 1: Check initial cookies"
      let getAllCookies =
            MkGetCookies
              { filter = Nothing,
                partition = Just $ BrowsingContextPartition bc
              }
      initial <- storageGetCookies getAllCookies
      logShow "Initial cookies" initial
      pause

      logTxt "Step 2: Set test cookies"
      let testCookie1 =
            MkPartialCookie
              { name = "workflow-cookie-1",
                value = TextBytesValue $ MkStringValue "value1",
                domain = "example.com",
                path = Just "/",
                httpOnly = Just False,
                secure = Just False,
                sameSite = Just Lax,
                expiry = Nothing
              }
      let setCookie1 =
            MkSetCookie
              { cookie = testCookie1,
                partition = Just $ BrowsingContextPartition bc
              }
      set1 <- storageSetCookie setCookie1
      logShow "First cookie set" set1

      let testCookie2 =
            MkPartialCookie
              { name = "workflow-cookie-2",
                value = TextBytesValue $ MkStringValue "value2",
                domain = "example.com",
                path = Just "/test",
                httpOnly = Just True,
                secure = Just False,
                sameSite = Just Strict,
                expiry = Nothing
              }
      let setCookie2 =
            MkSetCookie
              { cookie = testCookie2,
                partition = Just $ BrowsingContextPartition bc
              }
      set2 <- storageSetCookie setCookie2
      logShow "Second cookie set" set2
      pause

      logTxt "Step 3: Verify cookies were set"
      afterSet <- storageGetCookies getAllCookies
      logShow "Cookies after setting" afterSet
      pause

      logTxt "Step 4: Filter cookies by path"
      let pathFilter =
            MkCookieFilter
              { name = Nothing,
                value = Nothing,
                domain = Nothing,
                path = Just "/test",
                size = Nothing,
                httpOnly = Nothing,
                secure = Nothing,
                sameSite = Nothing,
                expiry = Nothing
              }
      let getFilteredCookies =
            MkGetCookies
              { filter = Just pathFilter,
                partition = Just $ BrowsingContextPartition bc
              }
      filtered <- storageGetCookies getFilteredCookies
      logShow "Cookies filtered by path" filtered
      pause

      logTxt "Step 5: Delete specific cookie"
      let deleteFilter =
            MkCookieFilter
              { name = Just "workflow-cookie-1",
                value = Nothing,
                domain = Nothing,
                path = Nothing,
                size = Nothing,
                httpOnly = Nothing,
                secure = Nothing,
                sameSite = Nothing,
                expiry = Nothing
              }
      let deleteCookie =
            MkDeleteCookies
              { filter = Just deleteFilter,
                partition = Just $ BrowsingContextPartition bc
              }
      deleted <- storageDeleteCookies deleteCookie
      logShow "Cookie deleted" deleted
      pause

      logTxt "Step 6: Verify deletion"
      final <- storageGetCookies getAllCookies
      logShow "Final cookies" final
      pause
