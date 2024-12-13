module Fetch (
    download,
    downloadMultiple,
    fetchDisruptions,
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Types
import Parse
import Database

-- | General Download Function to Fetch Data from an Endpoint
download :: URL -> IO L8.ByteString
download url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response

-- | Function to make multiple API Calls
downloadMultiple :: MultipleURL -> IO [L8.ByteString]
downloadMultiple urlList = do
    mapM download urlList

-- | Function to fetch Disruptions from the TFL API
fetchDisruptions :: String -> String -> IO [DisruptionDetail]
fetchDisruptions mode tflAppKey = do
    let urls = parseURLforDisruptionsAPI [mode] tflAppKey
    fmap concat $ mapM (\url -> do
        request <- parseRequest url
        response <- httpLBS request
        let jsonResponse = getResponseBody response
        queryAllDisruptions jsonResponse
        ) urls

