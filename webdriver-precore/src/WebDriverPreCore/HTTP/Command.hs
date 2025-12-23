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

mkPost :: forall a r. (ToJSON a) => Text -> UrlPath -> a -> Command r
mkPost description path = mkPost' description path (objectOrThrow ("mkPost - " <> description))

mkPost' :: forall a r. Text -> UrlPath -> (a -> Object) -> a -> Command r
mkPost' description path parser = Post description path . parser

-- Fallback Functions

coerceCommand  :: forall r r'. Command r -> Command r'
coerceCommand = \case
  Get {description, path} -> Get {description, path}
  Post {description, path, body} -> Post {description, path, body}
  PostEmpty {description, path} -> PostEmpty {description, path}
  Delete {description, path} -> Delete {description, path}


loosenCommand :: forall r. Command r -> Command Value
loosenCommand = coerceCommand

voidCommand :: Command a -> Command ()
voidCommand = coerceCommand

extendPostLoosen :: forall r. Command r -> Object -> Command Value
extendPostLoosen = extendCoercePost

extendPost :: forall r. Command r -> Object -> Command r
extendPost = extendCoercePost

extendCoercePost :: forall r r2. Command r -> Object -> Command r2
extendCoercePost cmd extended =
  case cmd of
    Post {description, path, body} -> Post {description, path, body = body <> extended}
    PostEmpty {description, path} -> Post {description = description, path = path, body = extended}
    get@Get {} -> 
        error $ "extendPost called with Get Command (extendPost can only be called with Post or PostEmpty commands): " <> show get
    del@Delete {} -> 
        error $ "extendPost called with Delete Command (extendPost can only be called with Post or PostEmpty commands): " <> show del

