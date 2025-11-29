module Http.HttpUtils where

import Data.Text (Text, unpack)
import WebDriverPreCore.Http.Protocol (parseWebDriverError)
import Prelude
import Data.Aeson (FromJSON, Value)
import WebDriverPreCore.Http (HttpResponse(..))
import Data.Aeson (Result(..))
import WebDriverPreCore.Http.Protocol (ErrorClassification(..))
import Data.Aeson.Types (parse)
import Text.Show.Pretty (ppShow)
import Data.Function ((&))
import WebDriverPreCore.Http.HttpResponse (fromBodyValue)

parseResultIO :: forall r. (FromJSON r) => Bool -> Value -> Text -> IO r
parseResultIO expectNull body description =
  case parse (fromBodyValue expectNull) body of
    Error msg ->
      fail $
        parseWebDriverError body & \case
          e@ResponeParseError {} -> unpack description <> "\n" <> "Failed to parse response:\n " <> msg <> "\nin response:" <> ppShow e
          e@UnrecognisedError {} -> "UnrecognisedError:\n " <> "\nin response:" <> ppShow e
          e@WebDriverError {} -> "WebDriver error thrown:\n " <> ppShow e
    Success r -> pure r
