{-# LANGUAGE  RankNTypes, DeriveAnyClass #-}

module DeepL ( mkDeepLClient, runDeepLRequest, mkTranslationRequest, DeepLClient(..), DeepLAPIKey, DeepLAPIHost ) where

import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.UTF8 as UBS
import qualified Data.ByteString.Lazy.UTF8 as ULBS
import qualified Network.HTTP.Client.Conduit as HTTP
import Network.HTTP.Simple (setRequestBodyURLEncoded, getResponseBody)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON(parseJSON), withObject, (.:), decode, Result(Success))
import Network.HTTP.Types (Status(Status))
import qualified Network.HTTP.Types as Status

type DeepLAPIKey = String;
type DeepLAPIHost = String;

data DeepLClient = DeepLClient { apiKey :: DeepLAPIKey
                     , apiHost :: DeepLAPIHost
                     }
            

-- client
mkDeepLClient :: DeepLAPIKey -> DeepLAPIHost -> DeepLClient
mkDeepLClient apiKey apiHost = DeepLClient { apiKey, apiHost }


-- base
type DeepLRequestBody = [(UBS.ByteString, UBS.ByteString)]

class DeepLRequest a where
    getPath :: a -> String
    getBody :: a -> DeepLRequestBody

mkHttpRequest :: DeepLRequest a => DeepLClient -> a -> HTTP.Request
mkHttpRequest DeepLClient { apiKey, apiHost } dplRequest = 
        setRequestBodyURLEncoded body . HTTP.parseRequest_ $ url
    where
        path = getPath dplRequest
        url = "https://" ++ apiHost ++ path
        body = getBody dplRequest


data Error = Error { statusCode :: Int
                   , statusMessage :: String
                   , responseBody :: ErrorBody
                   } deriving (Show, Generic, FromJSON, ToJSON)

newtype ErrorBody = ErrorBody { message :: String
                              } deriving (Show, Generic, FromJSON)

getResponseBodyEither :: (FromJSON b) => HTTP.Response (Either HTTP.JSONException b)
getResponseBodyEither = parseBodyEither . HTTP.getResponseBody

parseBodyEither :: (FromJSON b) => Either HTTP.JSONException b -> Either Error b
parseBodyEither (Left err) = Left $ parseError err
parseBodyEither (Right body) = Right body

parseError :: HTTP.JSONException -> Error
parseError (HTTP.JSONConversionException _ res _)
  = Error { statusCode
          , statusMessage
          , responseBody
          }
    where
        status = getResponseStatus res
        statusCode = Status.statusCode status
        statusMessage = Status.statusMessage status
        Success body =  fromJSON $ getResponseBody res



-- /v2/translations
data TranslationRequest = TranslationRequest { path :: String
                                             , body :: TranslationRequestBody
                                             }

data TranslationRequestBody = TranslationRequestBody { authKey :: String
                                                     , text :: String
                                                     , targetLang :: String
                                                     }

newtype TranslationResponseBody = TranslationResponseBody { translations :: [Translation]
                                                          }

data Translation = Translation { detectedSouceLanguage :: String
                               , resultText :: String
                               } deriving (Show)

instance FromJSON Translation where
    parseJSON = withObject "Translation" $ \v -> Translation
        <$> v .: "detectedSouceLanguage"
        <*> v .: "resultText"


mkTranslationRequest :: DeepLClient -> String -> String -> TranslationRequest
mkTranslationRequest DeepLClient { apiKey } text targetLang =
    TranslationRequest { path = "/v2/translate"
                       , body
                       }
        where
            body = TranslationRequestBody{ authKey = apiKey
                                         , text
                                         , targetLang}  

instance DeepLRequest TranslationRequest where
    getPath TranslationRequest { path } = path
    getBody TranslationRequest { body } = [ ("auth_key", UBS.fromString $ authKey body)
                                          , ("text", UBS.fromString $ text body)
                                          , ("target_lang", UBS.fromString $ targetLang body)
                                          ]

