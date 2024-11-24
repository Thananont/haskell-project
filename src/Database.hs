-- | Database 


-- | Queries 


-- | Query to print all the modes 
queryAllMode :: Connection -> IO [String]
queryAllMode conn =  do
    let selectQuery = "SELECT mode FROM modes"
    results <- queryNamed conn selectQuery [":modes":= mode]
    if length results > 0 then
        return $ map fromOnly results

-- | Query to print availble route
queryRoutesByMode :: Connection -> String -> IO [String]
queryRoutesByMode conn mode = do
    let query = "SELECT route FROM routes WHERE mode = :mode"
    results <- queryNamed conn query [":mode":= mode]
    return $ map fromOnly results

-- | Query to print availble Lines
queryAllLines :: Connection -> IO [String]
queryAllLines conn = do
    let query = "SELECT line FROM lines"
    results <- queryNamed conn query  [":lines":= line]
    return $ map fromOnly results

-- | Query to print availble services
queryServiceTypes :: Connection -> IO [String]
queryServiceTypes conn = do
    let query = "SELECT serv_name FROM service_types"
    results <- queryNamed conn query  [":service_types":= service_types]
    return $ map fromOnly results




