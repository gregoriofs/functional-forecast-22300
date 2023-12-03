module ApiRequests where

import Web.Data.Yahoo.API
import Web.Data.Yahoo.Response (PriceResponse)
import Data.Time(Day)

requestInformation :: String -> Day -> Day -> IO (Either String [PriceResponse])
requestInformation ticker start end = 
    let firstRequest = request ticker; daily = withDaily firstRequest; finalRequest = between (start,end) daily in
        fetch finalRequest