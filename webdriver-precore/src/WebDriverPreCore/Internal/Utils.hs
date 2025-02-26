module WebDriverPreCore.Internal.Utils
  ( opt,
    txt,
    enumerate,
    -- Aeson utils
    jsonToText,
    lsbToText,
    prettyPrintJson,
    parseJson
  )
where

import Data.Aeson
  ( Key,
    KeyValue ((.=)),
    ToJSON (),
    Value (..), eitherDecodeStrict,
  )
import Data.Function ((.), ($))
import Data.Functor (Functor, (<$>))
import Data.String (String)
import Data.Text ( Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Show (Show (..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.IO qualified as T
import Control.Exception (try, Exception (displayException), SomeException)
import GHC.Base (IO)
import Data.Either (Either, either)
import System.IO (print)
import GHC.Enum (Enum, Bounded (..))


{-
  this module is used between the library and testing modules
  it will be removed in a later relaease
-}

txt :: (Show a) => a -> Text
txt = pack . show


enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound ..]

-- Aeson stuff
-- TODO move to separte library

opt :: (Functor f, KeyValue e b, ToJSON a) => Key -> f a -> f b
opt lbl mb = (lbl .=) <$> mb

-- https://blog.ssanj.net/posts/2019-09-24-pretty-printing-json-in-haskell.html
lsbToText :: LBS.ByteString -> Text
lsbToText = decodeUtf8 . LBS.toStrict

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty

prettyPrintJson :: Value -> IO ()
prettyPrintJson v = do
  e <- (try @SomeException @_) $ T.putStrLn (jsonToText v)
  either (print . displayException) print e

parseJson :: Text -> Either String Value
parseJson input =
  eitherDecodeStrict (encodeUtf8 input)
