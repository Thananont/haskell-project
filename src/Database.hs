
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

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

createDatabase :: IO Connection
createDatabase = open "haskell-project-database.db"

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

dropAllTables :: Connection -> IO ()
dropAllTables connection = do
    execute_ connection (Query $ "DROP TABLE IF EXISTS mode")
    execute_ connection (Query $ "DROP TABLE IF EXISTS route")
    execute_ connection (Query $ "DROP TABLE IF EXISTS routesection")

-- Function that takes the modes and map them to be insert into the table
insertModes :: Connection -> [Mode] -> IO ()
insertModes connection modes = do            
    execute_ connection "CREATE TABLE IF NOT EXISTS mode ( \
        \ modeName TEXT PRIMARY KEY, \
        \ isType TEXT, \
        \ isTflService INTEGER NOT NULL CHECK(isTflService IN (0, 1)), \
        \ isFarePaying INTEGER NOT NULL CHECK(isFarePaying IN (0, 1)), \
        \ isScheduledService INTEGER NOT NULL CHECK(isScheduledService IN (0, 1)) \
        \ )"
    mapM_ (executeInsertMode connection) modes

-- Function insert the modes to the table
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

-- Function that takes the routes and map them to be inserted into the table
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

-- Function insert the routes to the table and map the sections to be inserted into a table
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

-- Function insert the route sections
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

dumpDatabase :: Connection -> IO ()
dumpDatabase connection = do
    modes <- fetchModeData connection
    routes <- fetchRouteData connection
    routeSections <- fetchRouteSectionData connection
    let jsonData = encode $ object ["modes" .= modes, "routes" .= routes, "routeSections" .= routeSections]
    _ <- L8.writeFile "data.json" jsonData
    putStrLn "The database data has been written to the JSON file!"

fetchModeData :: Connection -> IO [ModeDB]
fetchModeData connection = do
    modes <- query_ connection "SELECT isType, isTflService, isFarePaying, isScheduledService, modeName FROM mode" :: IO [ModeDB]
    return modes

fetchRouteData :: Connection -> IO [RouteDB]
fetchRouteData connection = do
    routes <- query_ connection "SELECT routeIsType, routeName, routeModeName, routeCreated, routeModified, routeModeName FROM route" :: IO [RouteDB]
    return routes

fetchRouteSectionData :: Connection -> IO [RouteSectionDB]
fetchRouteSectionData connection = do
    routeSections <- query_ connection "SELECT routeSectionName, routeSectionIsType, routeModeName,routeId,direction,originationName, destinationName, originator, destination, validTo FROM routesection" :: IO [RouteSectionDB]
    return routeSections

-- | Query to print all the modes 
queryAllMode :: Connection -> IO [String]
queryAllMode connection =  do
    let selectQuery = "SELECT modeName FROM mode"
    results <- query_ connection selectQuery :: IO [Only String]
    return $ map fromOnly results

-- | Make the first letter of the string uppercase
toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x:xs) = toUpper x : xs

-- | Function to  print mode name with the first letter in uppercase
printModeName :: [String] -> IO ()
printModeName modes =mapM_ (putStrLn . toUpperFirst) modes

-- | EXTRA FEARURES IMPLEMENTATION
-- | Query to print all the routes
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

-- | Query for all stop points
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

-- | Query for the search function
fetchStops :: String -> String -> IO (Either String SearchDestination)
fetchStops tflAppKey searchDestination = do
  let searchUrl = "https://api.tfl.gov.uk/StopPoint/Search/" 
                  ++ searchDestination 
                  ++ "?maxResults=10&oysterOnly=false&app_key=" 
                  ++ tflAppKey
  response <- httpLBS (parseRequest_ searchUrl)
  let body = getResponseBody response
  return (eitherDecode body :: Either String SearchDestination)

-- | Funtion to do a pretty print
printMatch :: Match -> IO ()
printMatch match = do
    putStrLn $ "Name: " ++ searchName match
    putStrLn $ "Available modes: " ++ show (modes match)
    putStrLn ""
           
-- | Query to print all the disruptions
-- | Funtion to print the disruption in a readble format 
printDisruptions :: String -> [DisruptionDetail] -> IO ()
printDisruptions modeName disruptions = do
    putStrLn $ "Disruptions for Mode: " ++ modeName
    mapM_ printDisruption disruptions

-- | Funtion to print non empty detials
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

queryAllDisruptions :: String -> L8.ByteString -> IO [DisruptionDetail]
queryAllDisruptions modeName json =
    case parseDisruptions json of
        Left err -> do
            putStrLn $ "Error in parsing disruptions: " ++ err
            return []
        Right disruptions -> return disruptions


