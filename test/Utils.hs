module Utils (
  opt,
  txt
) where

import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    ToJSON ()
  )

import Data.Text (Text, pack)
import Data.Functor ((<$>), Functor)
import GHC.Show (Show (..))
import Data.Function ((.))

{-
16-02-2025
  this module is duplicated across all libs because HLS had issues win a shared directory
  ie. was saying - Module Utils does not export opt when clearly it did

  shared liibrary not yet supported by HLS - when it is this module can go in a shared lib
  (ie. another cabal file entry)
-}

opt :: (Functor f, KeyValue e b, ToJSON a) => Key -> f a -> f b
opt lbl mb = (lbl .=) <$> mb

txt :: Show a => a -> Text
txt = pack . show