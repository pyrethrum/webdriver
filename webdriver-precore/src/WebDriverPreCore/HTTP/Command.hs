{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.HTTP.Command
  ( Command (..),
    mkPost,
    mkPost',
    voidCommand,
    loosenCommand,
    coerceCommand,
    extendPost,
    extendPostLoosen,
  )
where

import Data.Aeson as A
  ( Object,
    ToJSON (..), Value,
  )
import Data.Text (Text)
import AesonUtils (objectOrThrow)
import Utils (UrlPath (..))
import Prelude hiding (id, lookup)

-- |
--  The 'Command' type is a specification for a WebDriver Http command.
--  Every endpoint function in this module returns a 'Command' object which defines the HTTP method, URL path, and request body (if applicable) for the command.
--  The phantom type parameter 'r' represents the expected response type for the command. In practice, this 'r' type will always have a 'FromJSON' instance which can be used to parse the result from the response body.
data Command r
  = Get
      { description :: Text,
        path :: UrlPath
      }
  | Post
      { description :: Text,
        path :: UrlPath,
        body :: Object
      }
  | PostEmpty
      { description :: Text,
        path :: UrlPath
      }
  | Delete
      { description :: Text,
        path :: UrlPath
      }
  deriving (Show, Eq)


-- Constructors

-- | Creates a 'Post' command with the given description, path, and body.
--
-- The body parameter must be an instance of 'ToJSON' and encode to a JSON 'Object' type.
-- This function is partially applied in "WebDriverPreCore.HTTP.API" to generate specific named command functions.
--
-- If the body cannot be converted to a JSON 'Object', an error is thrown with the description included in the error message.
mkPost :: forall a r. (ToJSON a) => Text -> UrlPath -> a -> Command r
mkPost description path = mkPost' description path (objectOrThrow ("mkPost - " <> description))

-- | Creates a 'Post' command with the given description, path, and a custom parser function.
--
-- This is a more flexible version of 'mkPost' that allows you to provide a custom function to convert
-- the input parameter to a JSON 'Object'. This is useful when you need more control over the serialization process.
mkPost' :: forall a r. Text -> UrlPath -> (a -> Object) -> a -> Command r
mkPost' description path parser = Post description path . parser

-- Fallback Functions

-- | Changes the expected response type of a 'Command' to a different type.
--
-- This function preserves the HTTP method, path, and request body while only changing the phantom type parameter.
-- It can be used to adapt a command when you need a different response type than what the command originally specified.
coerceCommand  :: forall r r'. Command r -> Command r'
coerceCommand = \case
  Get {description, path} -> Get {description, path}
  Post {description, path, body} -> Post {description, path, body}
  PostEmpty {description, path} -> PostEmpty {description, path}
  Delete {description, path} -> Delete {description, path}

-- | Changes the expected response type of a 'Command' to a generic JSON 'Value'.
--
-- This is a specialization of 'coerceCommand' that loosens the type constraint,
-- allowing you to handle responses in a more flexible way when the exact response structure is not known or not needed.
loosenCommand :: forall r. Command r -> Command Value
loosenCommand = coerceCommand

-- | Changes the expected response type of a 'Command' to @()@, indicating that the response should be ignored.
--
-- This is useful for commands where you only care about the side effects and not the response value.
voidCommand :: Command a -> Command ()
voidCommand = coerceCommand

-- | Extends the request body of a 'Post' or 'PostEmpty' command with additional fields from a JSON 'Object',
-- changing the expected response type to a generic JSON 'Value'.
--
-- For 'Post' commands, the additional fields are merged with the existing body.
-- For 'PostEmpty' commands, the additional fields become the new body.
-- Attempting to use this with 'Get' or 'Delete' commands will result in a runtime error.
extendPostLoosen :: forall r. Command r -> Object -> Command Value
extendPostLoosen = extendCoercePost

-- | Extends the request body of a 'Post' or 'PostEmpty' command with additional fields from a JSON 'Object',
-- preserving the expected response type.
--
-- For 'Post' commands, the additional fields are merged with the existing body.
-- For 'PostEmpty' commands, the additional fields become the new body.
-- Attempting to use this with 'Get' or 'Delete' commands will result in a runtime error.
extendPost :: forall r. Command r -> Object -> Command r
extendPost = extendCoercePost

-- | Extends the request body of a 'Post' or 'PostEmpty' command with additional fields from a JSON 'Object',
-- changing the expected response type to a different type.
--
-- This is the underlying implementation used by both 'extendPost' and 'extendPostLoosen'.
-- For 'Post' commands, the additional fields are merged with the existing body.
-- For 'PostEmpty' commands, the additional fields become the new body.
-- Attempting to use this with 'Get' or 'Delete' commands will result in a runtime error.
extendCoercePost :: forall r r2. Command r -> Object -> Command r2
extendCoercePost cmd extended =
  case cmd of
    Post {description, path, body} -> Post {description, path, body = body <> extended}
    PostEmpty {description, path} -> Post {description = description, path = path, body = extended}
    get@Get {} -> 
        error $ "extendPost called with Get Command (extendPost can only be called with Post or PostEmpty commands): " <> show get
    del@Delete {} -> 
        error $ "extendPost called with Delete Command (extendPost can only be called with Post or PostEmpty commands): " <> show del

