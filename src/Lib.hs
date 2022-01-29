{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Options.Applicative 
import System.Environment (getEnv, lookupEnv)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

import DeepL (DeepLClient(..), mkDeepLClient)
import Envs (Envs(..), getEnvs)


import qualified Data.ByteString.Lazy.UTF8  as ULBS
import qualified Data.ByteString.UTF8 as UBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Simple (setRequestBodyURLEncoded)


newtype Opts = Opts { editor :: Bool }

run :: IO ()
run = do
    mEnvs <- getEnvs
    options <- execParser opts
    
    case mEnvs of
        Nothing -> print "Failed to get some envs"
        Just Envs { apiKey, apiHost } -> do
            let deepL = mkDeepLClient apiKey apiHost
            runCLI options deepL
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "DeepL client"
            <> header "dpl..."
            )
        

runCLI :: Opts -> DeepLClient -> IO ()
runCLI (Opts False) dplCli = runSimpleMode dplCli
runCLI (Opts True) DeepLClient { apiKey, apiHost } = putStrLn $ "Editor Mode: " ++ apiKey ++ apiHost
    
options :: Parser Opts
options = Opts
    <$> switch
        ( long "editor"
        <> short 'e'
        <> help "Use your editor for translation. (NOTE: set env EDITOR)."
        )

demoRequest :: String -> [(UBS.ByteString, UBS.ByteString)]
demoRequest apiKey = [ ("auth_key", UBS.fromString apiKey)
              , ("text", UBS.fromString "それは真実ではありません")
              , ("target_lang", "EN")
              ]

runSimpleMode :: DeepLClient -> IO ()
runSimpleMode DeepLClient { apiKey, apiHost }= do
    baseReq <- HTTP.parseRequest $ "https://" ++ apiHost ++ "/v2/translate"
    let req = setRequestBodyURLEncoded (demoRequest apiKey) baseReq
    response <- HTTP.httpLBS req 
    CLBS.putStrLn $ HTTP.getResponseBody response

setDefaultConfig :: HTTP.Request -> HTTP.Request
setDefaultConfig = HTTP.setRequestMethod  "POST"

