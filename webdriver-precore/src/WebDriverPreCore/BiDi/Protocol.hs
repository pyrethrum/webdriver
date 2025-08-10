module WebDriverPreCore.BiDi.Protocol where

import Data.Aeson
  ( Object,
    Value (..),
  )
import WebDriverPreCore.BiDi.CoreTypes (JSUInt)

data Command c r = MkCommand {
  command :: c -> JSUInt -> Value,
  extended :: c -> Object -> JSUInt -> Value --,
  -- responseMatcher :: JSUInt -> ResponseObject -> Either ResponseError (Maybe (MatchedResponse r))
}


