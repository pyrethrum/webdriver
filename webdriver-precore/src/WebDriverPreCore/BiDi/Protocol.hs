module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Object,
    Value (..),
  )
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)
import Prelude 
import WebDriverPreCore.BiDi.ResponseEvent (ResponseObject, ResponseError, MatchedResponse)

data Command c r = MkCommand {
  command :: c -> JSUInt -> Value,
  extendedCommand :: c -> Object -> JSUInt -> Value,
  responseMatcher :: JSUInt -> ResponseObject -> Either ResponseError (Maybe (MatchedResponse r))
}


