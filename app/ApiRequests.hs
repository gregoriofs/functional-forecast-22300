module ApiRequests where

import Web.Data.Yahoo.API
import Web.Data.Yahoo.Response (PriceResponse)
import Data.Time(Day)

-- Pull from yahoo api given some ticker, start date and end date to pull with
requestInformation :: String -> Day -> Day -> IO (Either String [PriceResponse])
requestInformation ticker start end = 
    let firstRequest = request ticker; daily = withDaily firstRequest; finalRequest = between (start,end) daily in
        fetch finalRequest