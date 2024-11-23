{-# LANGUAGE DeriveGeneric #-}

module Parse (
    parseModes,
    parseURLforRoutesAPI,
) where

import Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Function to rename fields in the JSON to match Haskell data type field names.
renameFields :: String -> String
renameFields "isType" = "$type"
renameFields other = other

-- | Custom options for JSON parsing.
customOptions = defaultOptions {
    fieldLabelModifier = renameFields
}

-- | Instance declaration to parse the Mode data type from JSON.
-- It uses generic parsing with the custom options defined above.
instance FromJSON Mode where
    parseJSON = genericParseJSON customOptions

-- | Parses a ByteString containing JSON data into a list of Mode objects (Modes).
parseModes :: L8.ByteString -> Either String Modes
parseModes json = eitherDecode json :: Either String [Mode]

-- | Helper function to concatenate the URLs for the Routes by Mode API
parseURLforRoutesAPI :: [String]-> String -> [String]
parseURLforRoutesAPI modeName app_key = map (\mode -> firstPartUrl ++ mode ++ "/Route?app_key=" ++ app_key) modeName
    where 
        firstPartUrl = "https://api.tfl.gov.uk/Line/Mode/"