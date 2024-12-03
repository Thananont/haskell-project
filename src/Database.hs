{-# LANGUAGE OverloadedStrings #-}

module Database (
    insertModes,
    insertRoutesByMode,
) where

import Types
import Database.SQLite.Simple

instance ToRow Mode where
  toRow (Mode isType isTflService isFarePaying isScheduledService modeName) = toRow (isType, fromBool isTflService, fromBool isFarePaying, fromBool isScheduledService, modeName)

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

-- Function that takes the modes and map them to be insert into the table
insertModes :: [Mode] -> IO ()
insertModes modes = do            
    connection <- open "mode.db"
    execute_ connection "CREATE TABLE IF NOT EXISTS mode (modeName TEXT PRIMARY KEY, isType TEXT, isTflService INTEGER NOT NULL CHECK(isTflService IN (0, 1)), isFarePaying INTEGER NOT NULL CHECK(isFarePaying IN (0, 1)), isScheduledService INTEGER NOT NULL CHECK(isScheduledService IN (0, 1)))"
    mapM_ (executeInsertMode connection) modes
    close connection

-- Function insert the modes to the table
executeInsertMode :: Connection -> Mode -> IO ()
executeInsertMode conn x = execute conn "INSERT INTO test1 (modeName, isType, isTflService, isFarePaying, isScheduledService) VALUES (?,?,?,?,?)" (toRow x)

-- Function that takes the routes and map them to be inserted into the table
insertRoutesByMode :: [Route] -> IO ()
insertRoutesByMode x = do            
    connection <- open "mode.db"
    execute_ connection "CREATE TABLE IF NOT EXISTS route ( \
        \ routeId TEXT PRIMARY KEY, \ 
        \ routeIsType TEXT, \ 
        \ routeName TEXT, \
        \ routeModeName TEXT, \ 
        \ routeCreated TEXT, \ 
        \ routeModified TEXT \
        \ )"
    execute_ connection "CREATE TABLE IF NOT EXISTS routesection ( \
        \ routeSectionName TEXT, \ 
        \ routeSectionIsType TEXT, \ 
        \ routeModeName TEXT, \
        \ direction TEXT, \
        \ originationName TEXT, \ 
        \ destinationName TEXT, \ 
        \ originator TEXT, \ 
        \ destination TEXT, \ 
        \ validTo TEXT, \ 
        \ validFrom TEXT \
        \ )"
    mapM_ (executeInsertRoute connection) x
    close connection

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
    mapM_ (executeInsertRouteSection connection (routeModeName route)) routeSections

-- Function insert the route sections
executeInsertRouteSection :: Connection -> String -> RouteSection -> IO ()
executeInsertRouteSection connection routeModeName routeSection = do
    execute connection "INSERT INTO routesection ( \ 
        \ routeSectionName, \
        \ routeSectionIsType, \
        \ routeModeName, \
        \ direction, \
        \ originationName, \
        \ destinationName, \ 
        \ originator, \ 
        \ destination, \ 
        \ validTo, \ 
        \ validFrom \ 
        \ ) VALUES (?,?,?,?,?,?,?,?,?,?)" (routeSectionName routeSection,
        routeSectionIsType routeSection,
        routeModeName,
        direction routeSection,
        originationName routeSection,
        destinationName routeSection, 
        originator routeSection, 
        destination routeSection, 
        validTo routeSection,
        validFrom routeSection 
        )