{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}

module StockRegression where

import ProcessCSVData
import Debug.Trace (trace, traceM, traceShow)
import Numeric.LinearAlgebra ((<>), vector)
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.HMatrix as HM

data RegressionModel = PolynomialRegressionModel Int [Double] | LinearRegressionModel [Double]
   deriving (Show)

-- Perform linear regression on stock data
performLinearRegression :: [StockPerformance] -> Either String RegressionModel
performLinearRegression stockData = do
  let x = HM.fromLists $ [[1,open s, high s, low s, fromIntegral (volume s)] | s <- stockData]
      y = HM.fromLists [[close s] | s <- stockData]
      -- trace show (HM.size xTrans, HM.size y)
      coefficients = LA.linearSolveSVD x y
  (if null (HM.toLists coefficients) then
     Left "Linear regression failed: Singular matrix"
  else
     Right $ LinearRegressionModel $ concat $ HM.toLists coefficients)

-- TODO: Func to predict using linear regression model

-- Convert StockPerformance to a feature vector
stockToFeatureVector :: StockPerformance -> LA.Vector Double
stockToFeatureVector stock = LA.vector [open stock, high stock, low stock, fromIntegral $ volume stock]

-- Polynomial feature transformation for a matrix of stock performances
polyFeaturesMatrix :: Int -> HM.Matrix Double -> HM.Matrix Double
polyFeaturesMatrix deg x =
  let lst = HM.toLists x in
    HM.fromLists [[xval ^ exp | xval <- row, exp <- [2..deg]] | row <- lst]

-- Perform polynomial regression
polynomialRegression :: Int -> [StockPerformance] -> Either String RegressionModel
polynomialRegression deg stockData = do
  let xValues = map stockToFeatureVector stockData
      yValues = HM.fromLists [[close stock] | stock <- stockData]
      xPoly = polyFeaturesMatrix deg (HM.fromRows xValues)
      xTrans = LA.tr xPoly
      -- _ = traceShow (HM.size xPoly, HM.size yValues)
      coefficientsMatrix = LA.linearSolveSVD xPoly yValues
  (if null (HM.toLists coefficientsMatrix) then
     Left "Polynomial regression failed: Singular matrix"
  else
         Right
           $ PolynomialRegressionModel deg $ concat $ HM.toLists coefficientsMatrix)

-- Predict using the polynomial regression model
-- TODO: Check if this actually works
predictPoly :: RegressionModel -> StockPerformance -> Double
predictPoly (PolynomialRegressionModel deg coeffs) stock =
  sum $ zipWith (*) coeffs [xval ^ expn | expn <- [0 .. deg]]
  where
    xval = open stock  -- Use 'open' as an example; you can adjust it based on your features

-- TODO: implement one more model perhaps