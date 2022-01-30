{-# LANGUAGE NamedFieldPuns #-}

module DeepL ( mkDeepLClient, runDeepLRequest, mkTranslationRequest, DeepLClient(..), DeepLAPIKey, DeepLAPIHost ) where

import qualified Network.HTTP.Simple as HTTP
import qualified Data.ByteString.UTF8 as UBS
import qualified Data.ByteString.Lazy.UTF8 as ULBS
import qualified Network.HTTP.Client.Conduit as HTTP
import Network.HTTP.Simple (setRequestBodyURLEncoded)

type DeepLAPIKey = String;
type DeepLAPIHost = String;

data DeepLClient = DeepLClient { apiKey :: DeepLAPIKey
                     , apiHost :: DeepLAPIHost
                     }
            
mkDeepLClient :: DeepLAPIKey -> DeepLAPIHost -> DeepLClient
mkDeepLClient apiKey apiHost = DeepLClient { apiKey, apiHost }

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

-- TODO: refactor
-- impl class DeepLResponse with aeson
-- change type of this function into DeepLRequest a => DeepLResponse b => DeepLClient -> a -> IO b
runDeepLRequest :: DeepLRequest a => DeepLClient -> a -> IO (HTTP.Response ULBS.ByteString)
runDeepLRequest client request = do
    HTTP.httpLBS $ mkHttpRequest client request
 --  
data TranslationRequest = TranslationRequest { path :: String
                                             , body :: TranslationRequestBody
                                             }

data TranslationRequestBody = TranslationRequestBody { authKey :: String
                                                     , text :: String
                                                     , targetLang :: String
                                                     }


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



