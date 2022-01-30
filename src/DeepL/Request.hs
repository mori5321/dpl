module DeepL.Request (
    IsRequest(..),
    Request(..),
    ToRequestBodyUrlEncoded(..),
    DeepLConfig(..),
    DeepLAPIKey,
    DeepLAPIHost,
    mkDeepLConfig,
    withBody,
    sendRequest,
    sendRequestEither,
) where

import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Simple ( setRequestBodyURLEncoded, getResponseBody )
import qualified Network.HTTP.Types as HTTPTypes

import qualified Data.ByteString.UTF8 as UBS
import qualified Data.ByteString.Lazy.UTF8 as ULBS

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import Network.HTTP.Simple (getResponseBody)


type DeepLAPIKey = String;
type DeepLAPIHost = String;

data DeepLConfig = DeepLConfig { apiKey :: DeepLAPIKey
                               , apiHost :: DeepLAPIHost
                               }

mkDeepLConfig :: DeepLAPIKey  -> DeepLAPIHost -> DeepLConfig
mkDeepLConfig = DeepLConfig

deepLAPIHost :: DeepLAPIHost
deepLAPIHost = "api-free.deepl.com"


type Path = String
type UrlEncodedRequestBody = [(UBS.ByteString, UBS.ByteString)]

class IsRequest request where
    getPath :: request -> Path
    getMethod :: request -> HTTPTypes.StdMethod  
    mkHttpRequest :: request -> HTTP.Request
    mkHttpRequest = mkHttpRequestDefault

mkHttpRequestDefault :: IsRequest r => r -> HTTP.Request 
mkHttpRequestDefault req =
    HTTP.setRequestPath (UBS.fromString path)
        $ HTTP.setRequestMethod (UBS.fromString method)
        $ setDefaultConfigs HTTP.defaultRequest 
    where
        path = getPath req
        method = show $ getMethod req

setDefaultConfigs :: HTTP.Request -> HTTP.Request
setDefaultConfigs = HTTP.setRequestPort 443 . HTTP.setRequestSecure True . HTTP.setRequestHost host
    where
        -- TODO: read host from args for testing
        host = UBS.fromString deepLAPIHost


class ToRequestBodyUrlEncoded a where
    urlEncode :: a -> UrlEncodedRequestBody

withBody :: ToRequestBodyUrlEncoded b => b -> HTTP.Request -> HTTP.Request
withBody b = HTTP.setRequestBodyURLEncoded body 
    where
        body = urlEncode b

data Request = Request { path :: Path
                       , method :: HTTPTypes.StdMethod
                       } deriving (Show, Generic)

instance IsRequest Request where
    getPath = path
    getMethod = method


sendRequest :: (FromJSON r) => HTTP.Request -> IO (HTTP.Response r)
sendRequest = HTTP.httpJSON

sendRequestEither :: (FromJSON r) => HTTP.Request -> IO (HTTP.Response (Either HTTP.JSONException r))
sendRequestEither = HTTP.httpJSONEither
