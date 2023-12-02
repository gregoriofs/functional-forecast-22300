{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE FlexibleContexts #-}

module StockRegression where

import ProcessCSVData
import Debug.Trace (trace)
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
      
     Right $ LinearRegressionModel [coefficients `HM.atIndex` (0, 0), coefficients `HM.atIndex`  (1, 0)])

-- Polynomial feature transformation
polyFeatures :: Int -> LA.Vector Double -> LA.Matrix Double
polyFeatures deg x =
  HM.fromColumns [HM.fromList [xval ^ expn | expn <- [0 .. deg]] | xval <- LA.toList x]

-- Perform polynomial regression
polynomialRegression :: Int -> LA.Vector Double -> [Double] -> Either String RegressionModel
polynomialRegression deg x y = do
  let xPoly = polyFeatures deg x
      xTrans = LA.tr xPoly
      coefficientsMatrix = LA.linearSolveSVD xTrans (HM.fromLists [[item] | item <- y])
  (if null (HM.toLists coefficientsMatrix) then
     Left "Polynomial regression failed: Singular matrix"
   else
   trace "sdafs"
     Right
       $ PolynomialRegressionModel deg (LA.toList coefficientsMatrix))

-- Predict using the polynomial regression model
predict :: RegressionModel -> LA.Vector Double -> Double
predict (PolynomialRegressionModel degree coefficients) x =
  sum $ zipWith (*) coefficients [x `HM.atIndex` expn | expn <- [0 .. degree]]