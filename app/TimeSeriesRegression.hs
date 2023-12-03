module TimeSeriesRegression where

import Debug.Trace
import Data.Time
import qualified Numeric.LinearAlgebra as LA
import Web.Data.Yahoo.Response

data RegressionModel = PolynomialRegressionModel Int [Double] | LinearRegressionModel [Double]
   deriving (Show)

-- Lagged values function with rolling mean as an additional feature
laggedValuesWithRollingMean :: Int -> Int -> [PriceResponse] -> [[Double]]
laggedValuesWithRollingMean lag window responses =
  let t1 = map (\i -> take lag $ map close (drop i responses)) [0..length responses - lag - 1]
      t2 =  map (\i -> [rollingMean window (take i responses)]) [0..length responses - lag - 1] in
        zipWith (++) t1 t2

-- Rolling mean function
rollingMean :: Int -> [PriceResponse] -> Double
rollingMean window responses = sum (map close (take window responses)) / fromIntegral window

linearRegression :: [[Double]] -> [Double] -> Either String RegressionModel
linearRegression xs ys =
  let xMatrix = LA.fromLists xs
      yVector = LA.asColumn $ LA.fromList (take (length ys - 2) ys)
      result = concat $ LA.toLists $ LA.linearSolveSVD xMatrix yVector
  in 
    trace ("Length of xMatrix: " ++ show (LA.rows xMatrix, LA.cols xMatrix)) $
    trace ("Length of yVector: " ++ show (LA.size yVector)) $
    if null result
        then Left "Error running regression"
    else
        Right $ LinearRegressionModel result

performLinearRegression :: [PriceResponse] -> Either String RegressionModel
performLinearRegression responses = let y = map close responses; x = laggedValuesWithRollingMean 2 2 responses in
    linearRegression x y

-- Prediction function
predict :: RegressionModel -> [Double] -> Double
predict (LinearRegressionModel coeffs) features = LA.dot (LA.fromList features) (LA.fromList coeffs)
predict (PolynomialRegressionModel deg coeffs) features = LA.dot (LA.fromList features) (LA.fromList coeffs)

-- Calculate lagged values for a new day
calculateLaggedValuesForNewDay :: [PriceResponse] -> Int -> Day -> [Double]
calculateLaggedValuesForNewDay responses lag newDayTimestamp =
  let precedingDays = takeWhile (\resp -> date resp < newDayTimestamp) responses
      laggedValuesForNewDay = take lag $ map close $ reverse precedingDays
  in laggedValuesForNewDay