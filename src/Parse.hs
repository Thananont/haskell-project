{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseModes,
    parseURLforRoutesAPI,
    parseRoutes,
    parseURLforDisruptionsAPI,
    parseDisruptions,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Function to rename fields in the JSON to match Haskell data type field names.
renameFields :: String -> String

-- | Rename Fields for the Modes
renameFields "isType" = "$type"

-- | Rename Fields for the Routes
renameFields "routeIsType" = "$type"
renameFields "routeId" = "id"
renameFields "routeName" = "name"
renameFields "routeModeName" = "modeName"
renameFields "routeDisruptions" = "disruptions"
renameFields "routeCreated" = "created"
renameFields "routeModified" = "modified"
renameFields "routeLineStatuses" = "lineStatuses"
renameFields "routeRouteSections" = "routeSections"

-- | Rename Fields for the Routes Section
renameFields "routeSectionIsType" = "$type"
renameFields "routeSectionName" = "name"

<<<<<<< HEAD

-- Rename Fields for Search Destination 
renameFields "searchDestinationTypeIsType" = "$type"
renameFields "query" = "query"
renameFields "total" = "total"
renameFields "searchMatches" = "matches"

-- Rename Fields for Match
renameFields "matchIsType" = "$type"
renameFields "icsId" = "icsId"
renameFields "modes" = "modes"
renameFields "zone" = "zone"
renameFields "id" = "id"
renameFields "searchName" = "name"
renameFields "lat" = "lat"
renameFields "lon" = "lon"


-- General Rename
=======
-- | Rename Fields for the Disruptions
renameFields "disruptionDetailIsType" = "$type"

-- | General Rename
>>>>>>> origin/Disruptions-Implementation
renameFields other = other

-- | Custom options for JSON parsing.
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

-- | Instances declarations for the Data Structures
instance FromJSON Mode where
    parseJSON = genericParseJSON customOptions

instance FromJSON Route where
    parseJSON = genericParseJSON customOptions

instance FromJSON RouteSection where
    parseJSON = genericParseJSON customOptions

instance FromJSON Disruption where
    parseJSON = genericParseJSON customOptions

instance FromJSON LineStatus where
    parseJSON = genericParseJSON customOptions

instance FromJSON SearchDestination where
    parseJSON = genericParseJSON customOptions
    
instance FromJSON Match where
instance FromJSON DisruptionDetail where
    parseJSON = genericParseJSON customOptions

instance FromJSON AffectedRoute where
    parseJSON = genericParseJSON customOptions

instance FromJSON AffectedStop where
    parseJSON = genericParseJSON customOptions

-- | Parses a ByteString containing JSON data into a list of Mode objects (Modes).
parseModes :: L8.ByteString -> Either String Modes
parseModes json = eitherDecode json :: Either String [Mode]

-- | Helper function to concatenate the URLs for the Routes by Mode API
parseURLforRoutesAPI :: [String]-> String -> [String]
parseURLforRoutesAPI modeName app_key = map (\mode -> firstPartUrl ++ mode ++ "/Route?app_key=" ++ app_key) modeName
    where 
        firstPartUrl = "https://api.tfl.gov.uk/Line/Mode/"

-- | Parses a ByteString containing JSON data into a list of Routes objects.
parseRoutes :: L8.ByteString -> Either String [Route]
parseRoutes json = eitherDecode json :: Either String [Route]

-- | Helper function to concatenate the URLs for the Disruptions by Mode API
parseURLforDisruptionsAPI :: [String]-> String -> [String]
parseURLforDisruptionsAPI modeName app_key = map (\mode -> firstPartUrl ++ mode ++ "/Disruption?app_key=" ++ app_key) modeName
    where 
        firstPartUrl = "https://api.tfl.gov.uk/Line/Mode/"

-- | Parses a ByteString containing JSON data into a list of Disruption objects.
parseDisruptions :: L8.ByteString -> Either String DisruptionsResponse
parseDisruptions json = eitherDecode json :: Either String DisruptionsResponse

