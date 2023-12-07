{-# LANGUAGE OverloadedStrings #-}

module Plotting where
import Debug.Trace
import Options.Applicative
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Graphics.Vega.VegaLite as VL
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Web.Data.Yahoo.Response
import qualified Data.Text as T
import TimeSeriesRegression


saveChartAsHtml :: FilePath -> VL.VegaLite -> IO ()
saveChartAsHtml filePath chart = do
    let spec = VL.fromVL chart
        html = "<html><head><script src='https://cdn.jsdelivr.net/npm/vega@5'></script><script src='https://cdn.jsdelivr.net/npm/vega-lite@5'></script><script src='https://cdn.jsdelivr.net/npm/vega-embed@6'></script></head><body><div id='vis'></div><script type='text/javascript'>var spec = " <> encode spec <> "; vegaEmbed('#vis', spec);</script></body></html>"
    BL.writeFile filePath html

-- Convert to hvega data for createClosePriceChart
toVegaData :: PriceResponse -> Value
toVegaData sp = object ["Date" .= date sp, "Close" .= close sp]

-- Plot close prices
createClosePriceChart :: [PriceResponse] -> VL.VegaLite
createClosePriceChart stockData =
    let dataValues = VL.dataFromRows [] $ map toVegaData stockData
        enc = VL.encoding . VL.position VL.X [VL.PName "Date", VL.PmType VL.Temporal]
                       . VL.position VL.Y [VL.PName "Close", VL.PmType VL.Quantitative]

    in VL.toVegaLite [dataValues, VL.height 600, VL.width 900, VL.mark VL.Line [], enc []]

-- convert  to hvega data for generatePlot
toVegaData2 :: [PriceResponse] -> [Double] -> [Value]
toVegaData2 stockData predictions = [object ["Date" .= date (fst item), "Close" .= close (fst item), "Predicted" .= snd item] | item <- zip stockData predictions]

-- Plots close values, predictions on those values using our model and predictions for a month in advance
generatePlot :: [PriceResponse] -> RegressionModel -> String -> Day -> Int -> VL.VegaLite
generatePlot stockData model ticker currDay lag =
  let predictions = map (predict model . generateNewFeatures stockData lag . date) stockData
      lVs = laggedValues lag stockData
      (newPreds, newSDs) = predictMonthInAdvance model stockData lVs lag currDay currDay
      -- Combine data for plotting
      plotData = VL.dataFromRows [] $ toVegaData2 (stockData ++ newSDs) (predictions ++ newPreds)
      enc1 = VL.encoding . 
             VL.position VL.X [VL.PName "Date", VL.PmType VL.Temporal]
      actuals = VL.position VL.Y [VL.PName "Close", VL.PmType VL.Quantitative, VL.PAxis [VL.AxTitle "Closing Price"]] .
                VL.color [VL.MString "red"]
      preds = VL.position VL.Y [VL.PName "Predicted", VL.PmType VL.Quantitative, VL.PAxis [VL.AxTitle "Closing Price"]] .
              VL.color [VL.MString "blue"]
      enc2 = [VL.mark VL.Line [], VL.encoding (actuals [])]

  in VL.toVegaLite [plotData, VL.title (T.pack $ ticker ++ " Stock Price (Red) vs Predicted Stock Price (Blue)") [], VL.height 600, VL.width 900, VL.layer (map VL.asSpec [enc2,[VL.mark VL.Point [VL.MOpacity 0.5], VL.encoding (preds [])]]) , enc1 []] 