module Main (main) where

import System.Environment
import Database.SQLite.Simple (Connection, open)


import Database
import Fetch
import Parse
import Types

-- App Key Constant Definition
tflAppKey :: String
tflAppKey = "270923a7a73f4dccab574faba91fa8b4"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"] -> do -- initialize the tables on the database
            initTables

        ["drop"] -> do -- drop the three tables on the database
            dropAllTables

        ["loaddata"] -> do -- download data from API and save to the database
            let url = "https://api.tfl.gov.uk/Line/Meta/Modes?app_key=" ++ tflAppKey
            --print url
            print "Downloading"
            json <- download url
            case parseModes json of
                Left err -> print err
                Right modes -> do
                    -- Print the json file for Modes
                    --print modes
                    -- Insert Modes into the database
                    insertModes modes
                    -- Storing the modeName fields into a list of String to use them to the next API Call
                    let modeNames = map modeName modes
                    -- Print the list with the modeNames
                    --putStrLn "Mode names:"
                    --print modeNames
                    -- Concatenation of the URLs for the Routes API call
                    let parsedURLs = parseURLforRoutesAPI modeNames tflAppKey
                    --print parsedURLs -- Prints the list with the URLs
                    -- Second API Call - Routes
                    multipleAPIResults <- downloadMultiple parsedURLs
                    -- Parsing the Routes API
                    let parsedFinal = map parseRoutes multipleAPIResults
                    case sequence parsedFinal of
                        Left err -> print err
                        Right allRoutes -> do
                            mapM_ insertRoutesByMode allRoutes
        
        ["modes"] -> do -- print all the modes
            connection <- createDatabase
            modeNames <- queryAllMode connection
            printModeName modeNames
        ["routes", modeName] -> do
            connection <- createDatabase
            routes <- queryAllRoutes connection modeName
            mapM_ print routes

        ["search"] -> do
            putStrLn "Please enter your destination:"
            searchDestination <- getLine
            let searchUrl = "https://api.tfl.gov.uk/StopPoint/Search/" ++ searchDestination ++"?maxResults=5&oysterOnly=false&app_key=" ++ tflAppKey
            print searchUrl
            searchJson <- download searchUrl
            print searchJson
        _ -> syntaxError

-- | Information Message to be displayed to the user in case he gives a wrong argument 
syntaxError :: IO ()
syntaxError = putStrLn
    "Usage: stack run -- [args]\n\
    \\n\
    \create                 Create sqlite database and tables\n\
    \loaddata               Download data from API and save to the database\n\
    \dumpdata               Generate data.json file with all data on database\n\
    \search                 The user can search for a specific place\n\
    \modes                  Print all modes\n\
    \routes [modeName]      Print all routes for a specific mode\n"
    


