{-# LANGUAGE NamedFieldPuns #-}

module DeepL ( DeepLAPIKey, DeepLAPIHost, mkDeepLClient, DeepLClient(..) ) where

type DeepLAPIKey = String;
type DeepLAPIHost = String;

data DeepLClient = DeepLClient { apiKey :: DeepLAPIKey
                     , apiHost :: DeepLAPIHost
                     }
            
mkDeepLClient :: DeepLAPIKey -> DeepLAPIHost -> DeepLClient
mkDeepLClient apiKey apiHost = DeepLClient { apiKey, apiHost }


