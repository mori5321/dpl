{-# LANGUAGE  DeriveAnyClass  #-}

module DeepL.Request.Translate (runRequest) where

import DeepL.Request (Request(..), DeepLConfig(..), ToRequestBodyUrlEncoded(..), withBody, sendRequestEither, IsRequest (mkHttpRequest))
import DeepL.Request.Error (getResponseBodyEither, Error(..), ErrorBody(..))
import Network.HTTP.Simple (getResponseBody)
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types as HTTPTypes
import qualified Data.ByteString.UTF8 as UBS
import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:))
import GHC.Generics (Generic)

type AuthKey = String
type Text = String
type Lang = String

data RequestBody = RequestBody { authKey :: AuthKey
                               , text :: Text
                               , targetLang :: Lang
                               }

instance ToRequestBodyUrlEncoded RequestBody where
    urlEncode RequestBody { authKey, text, targetLang }
        = [ ("auth_key", UBS.fromString authKey)
          , ("text", UBS.fromString text)
          , ("target_lang", UBS.fromString targetLang)
          ]

data Translation = Translation { detectedSourceLanguage :: Lang
                               , text :: Text
                               } deriving (Show, Generic)

instance FromJSON Translation where
    parseJSON = withObject "Translation" $ \v -> Translation
        <$> v .: "detected_source_language"
        <*> v .: "text"


mkRequest :: AuthKey -> Text -> Lang -> HTTP.Request
mkRequest authKey text targetLang = do
    withBody body . mkHttpRequest $ req
        where 
            req = Request { path = mconcat ["/v2", "/translate"]
                          , method = HTTPTypes.POST
                          }
            body = RequestBody { authKey, text, targetLang }

newtype Response = Response { translations :: [Translation] } deriving (Generic, Show, FromJSON)

runRequest :: AuthKey -> Text -> Lang -> IO (Either Error Response)
runRequest authKey text targetLang =
    getResponseBodyEither <$> sendRequestEither httpReq
    where
        httpReq = mkRequest authKey text targetLang

-- runRequest :: AuthKey -> Text -> Lang -> IO (Either Error Response)
-- runRequest authKey text targetLang = do
--     -- getResponseBodyEither <$> sendRequestEither httpReq
--     res <- HTTP.httpLBS httpReq
--     let mResult = (decode $ HTTP.getResponseBody res) :: Maybe Response
--     case mResult of
--       Nothing -> do
--           print res
--           pure $ Left $ Error 400 "Invalid" $ ErrorBody "Invalid" -- Temporary
--       Just r -> pure $ Right r
--     where
--         httpReq = mkRequest authKey text targetLang
