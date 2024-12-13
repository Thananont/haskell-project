module Main (main) where

import System.Environment
import Database.SQLite.Simple 
import Network.HTTP.Simple
import Database
import Fetch
import Parse
import Types
import Control.Monad
import Data.Char 
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    loadFile defaultConfig

    maybeTflAppKey <- lookupEnv "TFL_APP_KEY"
    tflAppKey <- case maybeTflAppKey of
        Just value -> return value
        Nothing -> do
            putStrLn "Error: TFL_APP_KEY is not set in the .env file"
            error "Missing TFL_APP_KEY"

    connection <- createDatabase
    args <- getArgs
    case args of
        -- | Initialize the three tables, mode, route, and routesection on the database
        ["create"] -> do
            initTables connection
            print "Finished initializing the tables in the database"
            close connection

        -- | Drop the three tables on the database
        ["drop"] -> do
            dropAllTables connection
            print "Finished dropping the tables in the database"
            close connection

        -- Download data from the TFL APIs and save them to the database
        ["loaddata"] -> do
            let url = "https://api.tfl.gov.uk/Line/Meta/Modes?app_key=" ++ tflAppKey
            print "Downloading"
            json <- download url
            case parseModes json of
                Left err -> print err
                Right modes -> do
                    -- Insert Modes into the database
                    insertModes connection modes
                    -- Storing the modeName fields into a list of String to use them to the next API Call
                    let modeNames = map modeName modes
                    -- Concatenation of the URLs for the Routes API call
                    let parsedURLs = parseURLforRoutesAPI modeNames tflAppKey
                    -- Second API Call - Routes
                    multipleAPIResults <- downloadMultiple parsedURLs
                    -- Parsing the Routes API
                    let parsedFinal = map parseRoutes multipleAPIResults
                    case sequence parsedFinal of
                        Left err -> print err
                        Right allRoutes -> do
                            mapM_ (insertRoutesByMode connection) allRoutes
            print "Finished loading and inserting data into the database"
            close connection

        -- Dump the data from the database in a data.json file
        ["dumpdata"] -> do
            dumpDatabase connection
            close connection

        -- | Print all the modes of transportation in the database
        ["modes"] -> do 
            modeNames <- queryAllMode connection
            printModeName modeNames
            close connection
        
        -- | Print all of the routes based on the inputted mode
        ["routes", modeN] -> do 
            let modenameL = map toLower modeN
            routes <- queryAllRoutes connection modenameL
            mapM_ print routes
            close connection

         -- | Print all of the stop points based on the inputted mode
        ["stop-points", modeN] -> do
            -- | handeling input case sensitivity
            let modeNameL = map toLower modeN
            stops <- queryAllStopPoints connection modeNameL
            mapM_ print stops
            close connection

         -- | Search for destinations and print out its details
        ["search"] -> do
            putStrLn "Please enter your destination:"
            searchDestination <- getLine
            result <- fetchStops tflAppKey searchDestination
            case result of
                Left err -> putStrLn ("Error parsing JSON: " ++ err)
                Right searchData -> do
                    let result = searchMatches searchData
                    if null result then
                        putStrLn "Destination not found"
                    else do
                        putStrLn "Found search data"
                        mapM_ printMatch result
            close connection

         -- | Print out real time disruptions occuring on the inputted mode              
        ["disruptions"] -> do
            modeNames <- queryAllMode connection
            mapM_ (\mode -> do
                let urls = parseURLforDisruptionsAPI [mode] tflAppKey
                disruptionsList <- fmap concat $ mapM (\url -> do
                    request <- parseRequest url
                    response <- httpLBS request
                    let jsonResponse = getResponseBody response
                    queryAllDisruptions mode jsonResponse
                    ) urls
                 -- Only print if there are disruptions
                unless (null disruptionsList) $ printDisruptions mode disruptionsList
                ) modeNames    
            close connection

        _ -> syntaxError

-- | Information Message to be displayed to the user in case he gives a wrong argument 
syntaxError :: IO ()
syntaxError = putStrLn
    "Usage: stack run -- [args]\n\
    \\n\
    \create                 Create sqlite database and tables\n\
    \loaddata               Download data from API and save to the database\n\
    \drop                   Drop the three tables on the database\n\
    \dumpdata               Generate data.json file with all data on database\n\
    \search                 The user can search for a specific place\n\
    \modes                  Print all modes\n\
    \routes [modeName]      Print all routes for a specific mode\n\
    \stop-points [modeName] Print all the stop points for a specific mode\n\
    \disruptions            Print all the disruptions\n"