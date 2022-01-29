{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
runCLI (Opts False) DeepLClient { apiKey, apiHost } = putStrLn $ "CLI Mode: " ++ apiKey ++ apiHost
runCLI (Opts True) DeepLClient { apiKey, apiHost } = putStrLn $ "Editor Mode: " ++ apiKey ++ apiHost
    
options :: Parser Opts
options = Opts
    <$> switch
        ( long "editor"
        <> short 'e'
        <> help "Use your editor for translation. (NOTE: set env EDITOR)."
        )

someFunc :: IO ()
someFunc = putStrLn "someFunc"
