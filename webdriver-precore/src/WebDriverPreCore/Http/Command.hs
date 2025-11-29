{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.Command
  ( Command (..),
    mkPost,
    mkPost',
    voidCommand
  )
where

import Data.Aeson as A
  ( ToJSON (..),
    Value,
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
