{-# LANGUAGE DeriveAnyClass #-}

module DeepL.Request.Error (getResponseBodyEither, Error(..), ErrorBody(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as UBS
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Simple (JSONException(JSONConversionException, JSONParseException))
import Network.HTTP.Types.Status as Status

data Error = Error { statusCode :: Int
                   , statusMessage :: String
                   , responseBody :: ErrorBody
                   } deriving (Show, Generic, FromJSON, ToJSON)

newtype ErrorBody = ErrorBody { message :: String
                              } deriving (Show, Generic, FromJSON, ToJSON)

getResponseBodyEither :: FromJSON b => HTTP.Response (Either HTTP.JSONException b) -> Either Error b
getResponseBodyEither = parseBodyEither . HTTP.getResponseBody 

parseBodyEither :: FromJSON b => Either HTTP.JSONException b -> Either Error b
parseBodyEither (Left err) = Left $ parseError err
parseBodyEither (Right body) = Right body

parseError :: HTTP.JSONException  -> Error
parseError (JSONConversionException _ res _) = do
        let eBody = fromJSON $ HTTP.getResponseBody res
        case eBody of
          Aeson.Success body -> Error stCode stMessage body
          Aeson.Error msg -> Error stCode stMessage $ ErrorBody $ show msg
    where
        st = HTTP.getResponseStatus res
        stCode = Status.statusCode st
        stMessage = UBS.toString $ Status.statusMessage st
parseError (JSONParseException _ res _) =
    Error { statusCode = stCode
          , statusMessage = stMessage
          , responseBody = body
          }
    where
        st = HTTP.getResponseStatus res
        stCode = Status.statusCode st
        stMessage = UBS.toString $ Status.statusMessage st
        body = ErrorBody { message = "parse error" }
    
