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
            case (parseModes json) of
                Left err -> print err
                Right modes -> do
                    -- Print the json file for Modes
                    print modes
                    -- Storing the modeName fields into a list of String to use them to the next API Call
                    let modeNames = map modeName modes
                    -- Print the list with the modeNames
                    putStrLn "Mode names:"
                    print modeNames
                    -- Concatenation of the URLs for the Routes API call
                    let parsedURLs = parseURLforRoutesAPI modeNames tflAppKey
                    print parsedURLs -- Prints the list with the URLs
                    -- Second API Call - Routes
                    multipleAPIResults <- downloadMultiple parsedURLs
                    --mapM_ L8.putStrLn multipleAPIResults -- Function to print the json file for Routes
                    -- Parsing the Routes API
                    let parsedFinal = map parseRoutes multipleAPIResults
                    case sequence parsedFinal of
                        Left err -> print err
                        Right allRoutes -> do
                            print allRoutes

                    --["modes"] <- do
                    -- ["route", mode] <- do
                    -- ["lines"] <- do
                    -- ["services"] <- do
                     
                    
        _ -> syntaxError

-- | Information Message to be displayed to the user in case he gives a wrong argument 
syntaxError = putStrLn
    "Usage: stack run -- [args]\n\
    \\n\
    \create                 Create sqlite database and tables\n\
    \loaddata               Download data from API and save to the database\n\
    \dumpdata               Generate data.json file with all data on database\n"


