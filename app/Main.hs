module Main (main) where

import System.Environment
import System.IO

import Lib
import Fetch
import Parse
import Types
import qualified Data.ByteString.Lazy.Char8 as L8

-- App Key Constant Definition
tflAppKey :: String
tflAppKey = "270923a7a73f4dccab574faba91fa8b4"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["loaddata"] -> do -- download data from API and save to the database
            let url = "https://api.tfl.gov.uk/Line/Meta/Modes?app_key=" ++ tflAppKey
            print url
            print "Downloading"
            json <- download url
            print "PRINTING JSON FROM HERE"
            case (parseModes json) of
                Left err -> print err
                Right modes -> do
                    print modes
                    -- Storing the modeName fields into a list of String to use them to the next API Call
                    let modeNames = map modeName modes
                    putStrLn "Mode names:"
                    print modeNames
                    putStrLn "HERE STARTS THE CONCATENATION"
                    let parsedURLs = parseURLforRoutesAPI modeNames tflAppKey
                    print parsedURLs
            --L8.putStrLn json
        _ -> syntaxError
        
syntaxError = putStrLn
    "Usage: stack run -- [args]\n\
    \\n\
    \create                 Create sqlite database and tables\n\
    \loaddata               Download data from API and save to the database\n\
    \dumpdata               Generate data.json file with all data on database\n"

