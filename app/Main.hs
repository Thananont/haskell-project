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

-- App Key Constant Definition
tflAppKey :: String
tflAppKey = "270923a7a73f4dccab574faba91fa8b4"

main :: IO ()
main = do
    connection <- createDatabase
    args <- getArgs
    case args of
        ["create"] -> do -- initialize the tables on the database
            initTables

        ["drop"] -> do -- drop the three tables on the database
            dropAllTables

        ["loaddata"] -> do -- download data from API and save to the database
            let url = "https://api.tfl.gov.uk/Line/Meta/Modes?app_key=" ++ tflAppKey
            print "Downloading"
            json <- download url
            case parseModes json of
                Left err -> print err
                Right modes -> do
                    -- Insert Modes into the database
                    insertModes modes
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
                            mapM_ insertRoutesByMode allRoutes

        ["dumpdata"] -> do
            dumpDatabase connection

        -- | Print all modes
        ["modes"] -> do 
            modeNames <- queryAllMode connection
            printModeName modeNames
        
        -- | Print routes based on the Modes
        ["routes", modeN] -> do 
            let modenameL = map toLower modeN
            routes <- queryAllRoutes connection modenameL
            mapM_ print routes

         -- | Print stop points based on the Modes
        ["stop-points", modeN] -> do
            -- | handeling input case sensitivity
            let modeNameL = map toLower modeN
            stops <- queryAllStopPoints connection modeNameL
            mapM_ print stops

         -- | Search for destinations
        ["search"] -> do
            putStrLn "Please enter your destination:"
            searchDestination <- getLine
            result <- fetchStops tflAppKey searchDestination
            case result of
                Left err -> putStrLn ("Error parsing JSON: " ++ err)
                Right searchData -> do
                    putStrLn "Found search data"
                    let matches = searchMatches searchData
                    mapM_ printMatch matches

         -- | Print disruptions for all Kodes                    
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
    \routes [modeName]      Print all routes for a specific mode\n\
    \stop-points [modeName] Print all the stop points for a specific mode\n\
    \disruptions            Print all the disruptions\n"
    


