{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.Command
  ( Command (..),
    mkPost,
    mkPost',
    voidCommand,
    loosenCommand
  )
where

import Data.Aeson as A
  ( ToJSON (..),
    Value, Object,
  )
import Data.Text (Text)
import WebDriverPreCore.Internal.Utils (UrlPath (..))
import Prelude hiding (id, lookup)


voidCommand :: Command a -> Command ()
voidCommand = \case
  Get {..} -> Get {..}
  Post {..} -> PostEmpty {..}
  PostEmpty {..} -> PostEmpty {..}
  Delete {..} -> Delete {..}
  

mkPost :: forall a r. (ToJSON a) => Text -> UrlPath -> a -> Command r
mkPost description path = mkPost' description path toJSON

mkPost'  :: forall a r. Text -> UrlPath -> (a -> Value) -> a -> Command r
mkPost' description path f = Post description path . f

-- |
--  The 'Command' type is a specification for a WebDriver Http command.
--  Every endpoint function in this module returns a 'Command' object.
data Command r
  = Get
      { description :: Text,
        path :: UrlPath
      }
  | Post
      { description :: Text,
        path :: UrlPath,
        body :: Value
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

  -- fallback

-- emptyCommand :: forall r. KnownCommand -> Command r
-- emptyCommand method = MkCommand {method = KnownCommand method, params = Object KM.empty}

loosenCommand :: forall r. Command r -> Command Object
loosenCommand = \case
  Get {..} -> Get {..}
  Post {..} -> Post {..}
  PostEmpty {..} -> PostEmpty {..}
  Delete {..} -> Delete {..}

-- extendPost :: 

-- extendCommandAny :: forall r. Object -> Command r -> Command Object
-- extendCommandAny = extendCommandPriv

-- extendCommand :: forall r. Object -> Command r -> Command r
-- extendCommand = extendCommandPriv

-- mkAnyCommand :: Text -> Value -> Command Value
-- mkAnyCommand method = MkCommand (UnknownCommand $ MkUnknownCommand method)

-- extendCommandPriv :: forall r r2. Object -> Command r -> Command r2
-- extendCommandPriv extended cmd = 
