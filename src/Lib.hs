{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Options.Applicative 
import System.Environment (getEnv, lookupEnv)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

import DeepL (mkDeepLClient, mkTranslationRequest, DeepLClient(..), runDeepLRequest)
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
            let deepLClient = mkDeepLClient apiKey apiHost
            runCLI options deepLClient
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

runSimpleMode :: DeepLClient -> IO ()
runSimpleMode client = do
    let req = mkTranslationRequest client "それは真実ではありません" "EN"
    response <- runDeepLRequest client req
    CLBS.putStrLn $ HTTP.getResponseBody response

setDefaultConfig :: HTTP.Request -> HTTP.Request
setDefaultConfig = HTTP.setRequestMethod  "POST"

