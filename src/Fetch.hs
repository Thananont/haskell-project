module Fetch (
    download,
    downloadMultiple
) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Types

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