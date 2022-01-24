module Main where

import Lib
import Options.Applicative 
import System.Environment (getEnv, lookupEnv)

newtype Opts = Opts { editor :: Bool }
type DeepLAPIKey = String;

main :: IO ()
main = do
    mApiKey <- lookupEnv "DPL_API_KEY"
    options <- execParser opts
    
    case mApiKey of
        Nothing -> print "No env set DPL_API_KEY"
        Just apiKey -> runCLI options apiKey

    where
        opts = info (options <**> helper)
            ( fullDesc
            <> progDesc "DeepL client"
            <> header "dpl..."
            )
        

runCLI :: Opts -> DeepLAPIKey -> IO ()
runCLI (Opts False) apiKey = putStrLn "CLI Mode"
runCLI (Opts True) apiKey = putStrLn "Editor Mode"
    
options :: Parser Opts
options = Opts
    <$> switch
        ( long "editor"
        <> short 'e'
        <> help "Use your editor for translation. (NOTE: set env EDITOR)."
        )
