{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseModes,
    parseURLforRoutesAPI,
    parseRoutes,
    parseSearchDestinations,
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

-- | Rename Fields for the Search Section (Extra Implementation Feature)
renameFields "searchDestinationTypeIsType" = "$type"
renameFields "matchIsType" = "$type"
renameFields "searchMatches" = "matches"
renameFields "searchName" = "name"
renameFields "parentId" = "parentId"
renameFields "topMostParentId" = "topMostParentId"

-- General Rename
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
    parseJSON = genericParseJSON customOptions


-- | Parses a ByteString containing JSON data into a list of Mode objects (Modes)
parseModes :: L8.ByteString -> Either String Modes
parseModes json = eitherDecode json :: Either String [Mode]

-- | Helper function to concatenate the URLs for the Routes by Mode API
parseURLforRoutesAPI :: [String]-> String -> [String]
parseURLforRoutesAPI modeName app_key = map (\mode -> firstPartUrl ++ mode ++ "/Route?app_key=" ++ app_key) modeName
    where 
        firstPartUrl = "https://api.tfl.gov.uk/Line/Mode/"

-- | Parses a ByteString containing JSON data into a list of Routes objects (Modes)
parseRoutes :: L8.ByteString -> Either String [Route]
parseRoutes json = eitherDecode json :: Either String [Route]

-- | Parses a ByteString containing JSON data into a list of SearchDestination objects (SearchDestination)
parseSearchDestinations :: L8.ByteString -> Either String [SearchDestination]
parseSearchDestinations json = eitherDecode json :: Either String [SearchDestination]
