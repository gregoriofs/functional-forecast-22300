module TimeSeriesRegression where

import Debug.Trace
import Data.Time
import qualified Numeric.LinearAlgebra as LA
import qualified Web.Data.Yahoo.Response as PR
import Data.Maybe


data RegressionModel = PolynomialRegressionModel Int [Double] | LinearRegressionModel [Double]
   deriving (Show)

-- Lagged values function
laggedValues :: Int -> [PR.PriceResponse] -> [[Double]]
laggedValues lag stockData =
  let t1 = map (\i -> 1 : take lag (map PR.close (drop i stockData))) [0..length stockData - lag - 1] in
    t1

-- Generate Model
linearRegression :: [[Double]] -> [Double] -> Int -> Either String RegressionModel
linearRegression xs ys lag =
  let xMatrix = LA.fromLists xs
      yVector = LA.asColumn $ LA.fromList (take (length ys - lag) ys)
      result = concat $ LA.toLists $ LA.linearSolveSVD xMatrix yVector
  in
    if null result
        then Left "Error running regression"
    else
        Right $ LinearRegressionModel result


performLinearRegression :: [PR.PriceResponse] -> Int -> Either String RegressionModel
performLinearRegression responses lag = let y = map PR.close responses; x = laggedValues lag responses in
    linearRegression x y lag

-- Prediction function
predict :: RegressionModel -> [Double] -> Double
predict (LinearRegressionModel coeffs) features = LA.dot fs cs where
  fs = LA.fromList features
  cs = LA.fromList coeffs
predict (PolynomialRegressionModel deg coeffs) features = LA.dot (LA.fromList features) (LA.fromList coeffs)

-- Calculate lagged values for a new day
calculateLaggedValuesForNewDay :: [PR.PriceResponse] -> Int -> Day -> [Double]
calculateLaggedValuesForNewDay responses lag newDayTimestamp =
  let precedingDays = takeWhile (\resp -> PR.date resp < newDayTimestamp) responses
      laggedValues = take lag $ map PR.close $ reverse precedingDays
  in 
    if length laggedValues < lag
      then 
        laggedValues ++ replicate (lag - length laggedValues) 0.0 
      else 
        laggedValues

-- Generate new features for a given date
generateNewFeatures :: [PR.PriceResponse] -> Int -> Day -> [Double]
generateNewFeatures stockData lag predictionDay = 1.0 : calculateLaggedValuesForNewDay stockData lag predictionDay

-- Generate a month's worth of predictions
predictMonthInAdvance :: RegressionModel -> [PR.PriceResponse] -> [[Double]] -> Int -> Day -> Day -> ([Double],[PR.PriceResponse])
predictMonthInAdvance model stockData laggedVals lag currentDay today |
  currentDay > addDays 30 today = ([],[]) |
  otherwise = let newFeatures = generateNewFeatures stockData lag currentDay; newPrediction = predict model newFeatures; newPR = PR.PriceResponse {PR.date = currentDay, PR.open = 1.0, PR.high = 1.0, PR.low = 1.0, PR.close = newPrediction, PR.adjClose = 1.0, PR.volume = 1.0}; newStockData = stockData ++ [newPR]; newLVs = laggedVals ++ [newFeatures]; (recursePreds, recurseSD) = predictMonthInAdvance model newStockData newLVs lag (addDays 1 currentDay) today in
    (recursePreds ++ [newPrediction], recurseSD ++ [newPR])