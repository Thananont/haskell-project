{-# LANGUAGE DeriveGeneric #-}

module Types (
    URL,
    MultipleURL,
    Mode(..),
    ModeDB(..),
    Modes,
    Route(..),
    RouteDB(..),
    RouteSection(..),
    RouteSectionDB(..),
    ServiceType(..),
    LineStatus(..),
    Disruption(..),
    Crowding(..),
    DisruptionDetail(..),
    AffectedRoute(..),
    AffectedStop(..),
    DisruptionsResponse,
    SearchDestination(..),
    Match(..),
) where

import GHC.Generics
import Data.Aeson
import Database.SQLite.Simple

-- | Type Aliases for the URLs
type URL = String
type MultipleURL = [String]

-- | Data Structure for Mode
data Mode = Mode {
    isType :: String, 
    isTflService :: Bool,
    isFarePaying :: Bool,
    isScheduledService :: Bool,
    modeName :: String
} deriving (Show, Generic)
type Modes = [Mode]

-- | Data Structure for ModeDB obtained querying modes from the database
data ModeDB = ModeDB {
    isTypeDB :: String, 
    isTflServiceDB :: Bool,
    isFarePayingDB :: Bool,
    isScheduledServiceDB :: Bool,
    modeNameDB :: String
} deriving (Show, Generic)
instance ToJSON ModeDB
instance FromRow ModeDB where
    fromRow = ModeDB <$> field <*> field <*> field <*> field <*> field

-- | Data Structure for the Route
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

-- | Data Structure for RouteDB obtained querying routes from the database
data RouteDB = RouteDB {
    routeIdDB :: String,
    routeIsTypeDB :: String, 
    routeNameDB :: String, 
    routeModeNameDB :: String, 
    routeCreatedDB :: String, 
    routeModifiedDB :: String
} deriving (Show, Generic)
instance ToJSON RouteDB
instance FromRow RouteDB where
    fromRow = RouteDB <$> field <*> field <*> field <*> field <*> field <*> field

-- | Data Structure for RouteSection part of the Routes API
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

-- | Data Structure for RouteSectionDB obtained querying routesections from the database
data RouteSectionDB = RouteSectionDB {
    routeSectionNameDB :: String, 
    routeSectionIsTypeDB :: String, 
    routeModeNameKeyDB :: String, 
    routeIdKeyDB :: String, 
    directionDB :: String, 
    originationNameDB :: String, 
    destinationNameDB :: String, 
    originatorDB :: String, 
    destinationDB :: String, 
    validToDB :: String
} deriving (Show, Generic)

instance ToJSON RouteSectionDB
instance FromRow RouteSectionDB where
    fromRow = RouteSectionDB <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Data Structure that represents the service type of a route
data ServiceType = ServiceType {
    serviceTypeIsType :: String,   
    serviceTypeName :: String,
    serviceTypeUri :: String
} deriving (Show, Generic)

-- | Data Structure that represents the line status of a route
data LineStatus = LineStatus {
    lineStatusIsType :: String
} deriving (Show, Generic)

-- | Data Structure that represents crowding information
data Crowding = Crowding {
    crowdingIsType :: String
} deriving (Show, Generic)

-- | Data Structures for the Search Feature (Extra Implementation)
data SearchDestination = SearchDestination {
    searchDestinationTypeIsType :: String,
    query :: String,
    total :: Int,
    searchMatches :: [Match]
} deriving (Show, Generic)

-- | Data Structure that represents the search result matches
data Match = Match {
    matchIsType :: String,
    icsId :: String,
    modes :: [String],
    zone :: Maybe String,
    id :: String,
    searchName :: String,
    lat :: Double,
    lon :: Double
} deriving (Show, Generic)

-- | Data Structures for the Disruptions (Extra Implementation)

-- | Data Structure that represents disruption information for a route
data Disruption = Disruption {
    disruptionIsType :: String 
} deriving (Show, Generic)

-- | Data Structure that represents disruption details
data DisruptionDetail = DisruptionDetail
    { category :: String
    , disruptionDetailIsType :: String
    , description :: String
    , created :: Maybe String
    , lastUpdate :: Maybe String
    , affectedRoutes :: [AffectedRoute]
    , affectedStops :: [AffectedStop]
    , closureText :: Maybe String
    } deriving (Show, Generic)

-- | Data Structure that represents routes affected by a disruption
data AffectedRoute = AffectedRoute
    { affectedRouteId :: String
    , affectedRouteName :: String
    } deriving (Show, Generic)

-- | Data Structure that represents stops affected by a disruption
data AffectedStop = AffectedStop
    { affectedStopId :: String
    , affectedStopName :: String
    } deriving (Show, Generic)

type DisruptionsResponse = [DisruptionDetail]
