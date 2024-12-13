
{-# LANGUAGE OverloadedStrings #-}

module Database (
    dropAllTables,
    dumpDatabase,
    initTables,
    insertModes,
    insertRoutesByMode,
    queryAllMode,
    queryAllRoutes,
    createDatabase,
    printModeName,
    fetchStops,
    printMatch,
    queryAllDisruptions,
    printDisruptions,
    queryAllStopPoints
) where

import Data.Char
import Types
import Database.SQLite.Simple
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson
import Parse
import Control.Monad

-- | A function that converts Bool to Int
fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

-- | A function that creates a connection with the database
createDatabase :: IO Connection
createDatabase = open "haskell-project-database.db"

-- | A function that initializes the tables in the database
initTables :: Connection -> IO ()
initTables connection = do
    execute_ connection "CREATE TABLE IF NOT EXISTS mode ( \
        \ modeName TEXT PRIMARY KEY, \
        \ isType TEXT, \
        \ isTflService INTEGER NOT NULL CHECK(isTflService IN (0, 1)), \
        \ isFarePaying INTEGER NOT NULL CHECK(isFarePaying IN (0, 1)), \
        \ isScheduledService INTEGER NOT NULL CHECK(isScheduledService IN (0, 1)) \
        \ )"
    execute_ connection "CREATE TABLE IF NOT EXISTS route ( \
        \ routeId TEXT PRIMARY KEY, \ 
        \ routeIsType TEXT, \ 
        \ routeName TEXT, \
        \ routeModeName TEXT, \ 
        \ routeCreated TEXT, \ 
        \ routeModified TEXT, \
        \ FOREIGN KEY (routeModeName) REFERENCES mode (modeName) \
        \ )"
    execute_ connection "CREATE TABLE IF NOT EXISTS routesection ( \
        \ routeSectionName TEXT, \ 
        \ routeSectionIsType TEXT, \ 
        \ routeModeName TEXT, \
        \ routeId TEXT, \
        \ direction TEXT, \
        \ originationName TEXT, \ 
        \ destinationName TEXT, \ 
        \ originator TEXT, \ 
        \ destination TEXT, \ 
        \ validTo TEXT, \ 
        \ FOREIGN KEY (routeModeName) REFERENCES mode (modeName), \
        \ FOREIGN KEY (routeId) REFERENCES route (routeId) \
        \ )"

-- | A function that drops the mode, route, and routesection table from the database
dropAllTables :: Connection -> IO ()
dropAllTables connection = do
    execute_ connection "DROP TABLE IF EXISTS mode"
    execute_ connection "DROP TABLE IF EXISTS route"
    execute_ connection "DROP TABLE IF EXISTS routesection"

-- | A function that takes the modes list and map them to the executeInsertMode function to insert them into the table, create the table if it doesn't exist
insertModes :: Connection -> [Mode] -> IO ()
insertModes connection modeList = do            
    execute_ connection "CREATE TABLE IF NOT EXISTS mode ( \
        \ modeName TEXT PRIMARY KEY, \
        \ isType TEXT, \
        \ isTflService INTEGER NOT NULL CHECK(isTflService IN (0, 1)), \
        \ isFarePaying INTEGER NOT NULL CHECK(isFarePaying IN (0, 1)), \
        \ isScheduledService INTEGER NOT NULL CHECK(isScheduledService IN (0, 1)) \
        \ )"
    mapM_ (executeInsertMode connection) modeList

-- | A function that inserts the mode to the mode table
executeInsertMode :: Connection -> Mode -> IO ()
executeInsertMode connection mode = execute connection "INSERT INTO mode ( \
    \ modeName, \
    \ isType, \
    \ isTflService, \
    \ isFarePaying, \ 
    \ isScheduledService \ 
    \ ) VALUES (?,?,?,?,?)" (
      modeName mode, 
      isType mode, 
      fromBool (isTflService mode),
      fromBool (isFarePaying mode),
      fromBool (isScheduledService mode)
    )

-- | A function that takes the route list and map them to the executeInsertRoute function to insert them into the tables, create the tables if it doesn't exist
insertRoutesByMode :: Connection -> [Route] -> IO ()
insertRoutesByMode connection routes = do            
    execute_ connection "CREATE TABLE IF NOT EXISTS route ( \
        \ routeId TEXT PRIMARY KEY, \ 
        \ routeIsType TEXT, \ 
        \ routeName TEXT, \
        \ routeModeName TEXT, \ 
        \ routeCreated TEXT, \ 
        \ routeModified TEXT, \
        \ FOREIGN KEY (routeModeName) REFERENCES mode (modeName) \
        \ )"
    execute_ connection "CREATE TABLE IF NOT EXISTS routesection ( \
        \ routeSectionName TEXT, \ 
        \ routeSectionIsType TEXT, \ 
        \ routeModeName TEXT, \
        \ routeId TEXT, \
        \ direction TEXT, \
        \ originationName TEXT, \ 
        \ destinationName TEXT, \ 
        \ originator TEXT, \ 
        \ destination TEXT, \ 
        \ validTo TEXT, \ 
        \ FOREIGN KEY (routeModeName) REFERENCES mode (modeName), \
        \ FOREIGN KEY (routeId) REFERENCES route (routeId) \
        \ )"
    mapM_ (executeInsertRoute connection) routes

-- | A function that inserts the route to the table and map the routesections to the executeInsertRouteSection to insert them into the table
executeInsertRoute :: Connection -> Route -> IO ()
executeInsertRoute connection route  = do
    execute connection "INSERT INTO route ( \ 
        \ routeId, \
        \ routeIsType, \
        \ routeName, \
        \ routeModeName, \
        \ routeCreated, \ 
        \ routeModified \ 
        \ ) VALUES (?,?,?,?,?,?)" (routeId route,
        routeIsType route, 
        routeName route, 
        routeModeName route, 
        routeCreated route, 
        routeModified route
        )
    let routeSections = routeRouteSections route
    mapM_ (executeInsertRouteSection connection (routeModeName route) (routeId route)) routeSections

-- | A function insert the route sections into the table
executeInsertRouteSection :: Connection -> String -> String -> RouteSection -> IO ()
executeInsertRouteSection connection routeModeName routeId routeSection = do
    execute connection "INSERT INTO routesection ( \ 
        \ routeSectionName, \
        \ routeSectionIsType, \
        \ routeModeName, \
        \ routeId, \
        \ direction, \
        \ originationName, \
        \ destinationName, \ 
        \ originator, \ 
        \ destination, \ 
        \ validTo \ 
        \ ) VALUES (?,?,?,?,?,?,?,?,?,?)" (routeSectionName routeSection,
        routeSectionIsType routeSection,
        routeModeName,
        routeId,
        direction routeSection,
        originationName routeSection,
        destinationName routeSection, 
        originator routeSection, 
        destination routeSection, 
        validTo routeSection
        )

-- | A function that dump the data from the database into a json file
dumpDatabase :: Connection -> IO ()
dumpDatabase connection = do
    modes <- fetchModeData connection
    routes <- fetchRouteData connection
    routeSections <- fetchRouteSectionData connection
    let jsonData = encode $ object ["modes" .= modes, "routes" .= routes, "routeSections" .= routeSections]
    _ <- L8.writeFile "data.json" jsonData
    putStrLn "The database data has been written to the JSON file!"

-- | A function that queries for the mode data in the mode table from the database
fetchModeData :: Connection -> IO [ModeDB]
fetchModeData connection = do
    modes <- query_ connection "SELECT isType, isTflService, isFarePaying, isScheduledService, modeName FROM mode" :: IO [ModeDB]
    return modes

-- | A function that queries for the route data in the route table from the database
fetchRouteData :: Connection -> IO [RouteDB]
fetchRouteData connection = do
    routes <- query_ connection "SELECT routeIsType, routeName, routeModeName, routeCreated, routeModified, routeModeName FROM route" :: IO [RouteDB]
    return routes

-- | A function that queries for the routesection data in the routesection table from the database
fetchRouteSectionData :: Connection -> IO [RouteSectionDB]
fetchRouteSectionData connection = do
    routeSections <- query_ connection "SELECT routeSectionName, routeSectionIsType, routeModeName,routeId,direction,originationName, destinationName, originator, destination, validTo FROM routesection" :: IO [RouteSectionDB]
    return routeSections

-- | A function that queries to print all of the name of the modes in the database
queryAllMode :: Connection -> IO [String]
queryAllMode connection =  do
    let selectQuery = "SELECT modeName FROM mode"
    results <- query_ connection selectQuery :: IO [Only String]
    return $ map fromOnly results

-- | A function that turn a string into its upper-case version
toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x:xs) = toUpper x : xs

-- | A function that prints a mode's name with the first letter in uppercase
printModeName :: [String] -> IO ()
printModeName modes =mapM_ (putStrLn . toUpperFirst) modes

-- | A function that queries to print all name of the routes from a mode in the database
queryAllRoutes :: Connection -> String -> IO [String]
queryAllRoutes connection modeName = do
    putStrLn $ "Please wait, looking for all available routes for the " ++ modeName ++ "..."
    putStrLn $ "----------------------------------------------------------"
    let selectQuery = "SELECT routeName FROM route WHERE routeModeName = :routeModeName"
    results <- queryNamed  connection selectQuery [":routeModeName" := modeName] :: IO [Only String]
    if results == [] then
        return ["No routes found for this mode"]
    else
        return $ map fromOnly results

-- | A function that queries for all stop points from a mode in the database
queryAllStopPoints :: Connection -> String -> IO [String]
queryAllStopPoints connection modeName =  do
    putStrLn $ "Looking for stop points for " ++ modeName ++ "...."
    putStrLn $ "----------------------------------------------------------"
    let selectQuery = "SELECT routeSectionName FROM routesection WHERE routeModeName = :routeModeName"
    result <- queryNamed connection selectQuery [":routeModeName" := modeName] :: IO [Only String]
    if result == [] then
        return ["No stop points found"]
    else
        return $ map fromOnly result

-- | A function that queries for the search function
fetchStops :: String -> String -> IO (Either String SearchDestination)
fetchStops tflAppKey searchDestination = do
  let searchUrl = parseSearchDestination tflAppKey searchDestination
  response <- httpLBS (parseRequest_ searchUrl)
  let body = getResponseBody response
  return (eitherDecode body :: Either String SearchDestination)

-- | A function to do a pretty print
printMatch :: Match -> IO ()
printMatch match = do
    putStrLn $ "Name: " ++ searchName match
    putStrLn $ "Available modes: " ++ show (modes match)
    putStrLn ""
           
-- | A function that maps the disruptions to the printDisruption to be printed for each disruption of each mode
printDisruptions :: String -> [DisruptionDetail] -> IO ()
printDisruptions modeName disruptions = do
    putStrLn $ "Disruptions for Mode: " ++ modeName
    mapM_ printDisruption disruptions

-- | A funtion to print the disruption in a readble format 
printDisruption :: DisruptionDetail -> IO ()
printDisruption disruption = do
    let cat = category disruption
    let desc = description disruption
    let routes = affectedRoutes disruption
    let stops = affectedStops disruption
    let update = lastUpdate disruption
    
    when (not $ null cat) $ putStrLn $ "Category: " ++ cat
    when (not $ null desc) $ putStrLn $ "Description: " ++ desc
    when (not $ null routes) $ putStrLn $ "Affected Routes: " ++ show (map affectedRouteName routes)
    when (not $ null stops) $ putStrLn $ "Affected Stops: " ++ show (map affectedStopName stops)
    when (not $ null $ show update) $ putStrLn $ "Last Update: " ++ show update
    putStrLn "-----------------------------------"
-- | Funtion to fetch and print all the disruptions  
queryAllDisruptions :: L8.ByteString -> IO [DisruptionDetail]
queryAllDisruptions json =
    case parseDisruptions json of
        Left err -> do
            putStrLn $ "Error in parsing disruptions: " ++ err
            return []
        Right disruptions -> return disruptions


