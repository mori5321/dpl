{-# LANGUAGE NamedFieldPuns #-}

module Envs (getEnvs, Envs(..)) where 

import System.Environment (getEnv, lookupEnv)
import DeepL (DeepLAPIKey, DeepLAPIHost)


envApiKey :: DeepLAPIKey
envApiKey = "DPL_API_KEY"

envApiHost :: DeepLAPIHost
envApiHost = "DPL_API_HOST"

data Envs = Envs { apiKey :: DeepLAPIKey
                 , apiHost :: DeepLAPIHost
                 }

getEnvs :: IO (Maybe Envs)
getEnvs = do
    mApiKey <- lookupEnv envApiKey
    mApiHost <- lookupEnv envApiHost
    pure $ mkEnvs <$> mApiKey <*> mApiHost

mkEnvs :: DeepLAPIKey -> DeepLAPIHost -> Envs
mkEnvs apiKey apiHost = Envs { apiKey, apiHost }
