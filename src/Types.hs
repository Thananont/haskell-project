{-# LANGUAGE DeriveGeneric #-}

module Types (
    Mode(..),
    Modes,
    Route(..),
    RouteSection(..),
    ServiceType(..),
    LineStatus(..),
    Disruption(..),
    Crowding(..)
) where

import GHC.Generics

-- | Data Structures for the Modes
data Mode = Mode {
    isType :: String, 
    isTflService :: Bool,
    isFarePaying :: Bool,
    isScheduledService :: Bool,
    modeName :: String
} deriving (Show, Generic)

type Modes = [Mode]

-- | Data Structures for the Routes
data Route = Route {
    routeIsType :: String,
    routeId :: String,
    routeName :: String,
    routeModeName :: String,
    routeDisruptions :: [Disruption],
    routeCreated :: String,
    routeModified :: String,
    routeLineStatuses :: [LineStatus],
    routeRouteSections :: [RouteSection]
} deriving (Show, Generic)


data RouteSection = RouteSection {
    routeSectionIsType :: String,    
    routeSectionName :: String,
    direction :: String,
    originationName :: String,
    destinationName :: String,
    originator :: String,
    destination :: String,
    serviceType :: String,
    validTo :: String,
    validFrom :: String
} deriving (Show, Generic)

data ServiceType = ServiceType {
    serviceTypeIsType :: String,   
    serviceTypeName :: String,
    serviceTypeUri :: String
} deriving (Show, Generic)

data LineStatus = LineStatus {
    lineStatusIsType :: String
} deriving (Show, Generic)

data Disruption = Disruption {
    disruptionIsType :: String 
} deriving (Show, Generic)

data Crowding = Crowding {
    crowdingIsType :: String
} deriving (Show, Generic)
