{-# LANGUAGE OverloadedStrings #-}

module Plotting where

import Options.Applicative
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import Graphics.Vega.VegaLite
import ProcessCSVData (readStockData, StockPerformance(..))

import Data.Aeson (Value, object, (.=))

import qualified Data.ByteString.Lazy as BL
import Graphics.Vega.VegaLite (fromVL)
import Data.Aeson (encode)

saveChartAsHtml :: FilePath -> VegaLite -> IO ()
saveChartAsHtml filePath chart = do
    let spec = fromVL chart
        html = "<html><head><script src='https://cdn.jsdelivr.net/npm/vega@5'></script><script src='https://cdn.jsdelivr.net/npm/vega-lite@5'></script><script src='https://cdn.jsdelivr.net/npm/vega-embed@6'></script></head><body><div id='vis'></div><script type='text/javascript'>var spec = " <> encode spec <> "; vegaEmbed('#vis', spec);</script></body></html>"
    BL.writeFile filePath html


-- Function to convert StockPerformance to hvega data
toVegaData :: StockPerformance -> Value
toVegaData sp = object ["Date" .= date sp, "Close" .= close sp]

-- Function to create a Vega-Lite chart for Close Prices
createClosePriceChart :: [StockPerformance] -> VegaLite
createClosePriceChart stockData =
    let dataValues = dataFromRows [] $ map toVegaData stockData
        enc = encoding . position X [PName "Date", PmType Temporal]
                       . position Y [PName "Close", PmType Quantitative]

    in toVegaLite [dataValues, mark Line [], enc []]