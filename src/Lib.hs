{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( run
    ) where

import Options.Applicative 
import System.Environment (getEnv, lookupEnv)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

import Envs (Envs(..), getEnvs)


import qualified Data.ByteString.Lazy.UTF8  as ULBS
import qualified Data.ByteString.UTF8 as UBS
import qualified Data.ByteString.Lazy.Char8 as CLBS
import DeepL.Request (DeepLConfig (..), mkDeepLConfig)
import qualified DeepL.Request.Translate as TranslateAPI

newtype Opts = Opts { editor :: Bool }

run :: IO ()
run = do
    mEnvs <- getEnvs
    options <- execParser opts
    
    case mEnvs of
        Nothing -> print "Failed to get some envs"
        Just Envs { apiKey, apiHost } -> do
            let deepLConfig = mkDeepLConfig apiKey apiHost
            runCLI options deepLConfig
    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "DeepL client"
            <> header "dpl..."
            )
        

runCLI :: Opts -> DeepLConfig -> IO ()
runCLI (Opts False) dplConfig = runSimpleMode dplConfig
runCLI (Opts True) DeepLConfig { apiKey, apiHost } = putStrLn $ "Editor Mode: " ++ apiKey ++ apiHost
    
options :: Parser Opts
options = Opts
    <$> switch
        ( long "editor"
        <> short 'e'
        <> help "Use your editor for translation. (NOTE: set env EDITOR)."
        )

runSimpleMode :: DeepLConfig -> IO ()
runSimpleMode DeepLConfig { apiKey } = do
    eResult <- TranslateAPI.runRequest apiKey "それは真実ではありません" "EN"
    case eResult of
      Left e -> print $ "Failed: " <> show e
      Right result -> print result

