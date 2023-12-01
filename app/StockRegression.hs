
module StockRegression where

import ProcessCSVData
import Numeric.LinearAlgebra ((<>), (<.>), vector, fromList)
import qualified Numeric.LinearAlgebra.HMatrix as HM

-- Perform linear regression on stock data
performLinearRegression :: [StockPerformance] -> Either String (Double, Double)
performLinearRegression stockData = do
  let onesColumn = HM.vector $ LA.constant 1 (length stockData)
      x = HM.fromColumns [onesColumn, LA.fromList [stockOpen s | s <- stockData]]
      y = HM.fromList [stockClose s | s <- stockData]
  case HM.linearRegression x y of
    HM.Failed msg -> Left ("Linear regression failed: " ++ msg)
    HM.Converged coefficients -> Right (coefficients HM.! 0, coefficients HM.! 1)