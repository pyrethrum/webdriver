{-# OPTIONS_HADDOCK hide #-}

module WebDriverPreCore.Http.Internal.Capabilities
  ( fullCapsVal,
    firstMatchArray,
    alwaysMatchValue,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    ToJSON (toJSON),
    Value,
    object,
  )
import Data.Aeson.Types
  ( Array,
  )
import Data.Function (($), (.))
import Data.Maybe (catMaybes)
import Data.Vector (fromList)
import WebDriverPreCore.Internal.AesonUtils (opt)
import Prelude (Maybe (..), fmap)

-- specific helpers used in toJSON for initialising both Http and BiDi Session via Http

firstMatchArray :: (ToJSON capabilities) => [capabilities] -> Array
firstMatchArray = fromList . fmap toJSON

alwaysMatchValue :: (ToJSON capabilities) => Maybe capabilities -> Value
alwaysMatchValue caps = object $ catMaybes [opt "alwaysMatch" $ caps]

fullCapsVal :: Value -> Array -> Value
fullCapsVal alwaysMatch firstMatch =
  object
    [ "capabilities" .= alwaysMatch,
      "firstMatch" .= firstMatch
    ]
