{-# LANGUAGE NoImplicitPrelude #-}

module PolynomialRegression
  ( performPolynomialRegression
  , main
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Numeric.LinearAlgebra ((<>), (<.>), vector)
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.HMatrix as HM

data StockPerformance = StockPerformance
  { stockDate :: String
  , stockOpen :: Double
  , stockHigh :: Double
  , stockLow :: Double
  , stockClose :: Double
  , stockVolume :: Int
  , stockAdjClose :: Double
  }
  deriving (Show)

performPolynomialRegression :: [StockPerformance] -> Int -> Either String [Double]
performPolynomialRegression stockData degree = do
  let x = HM.fromColumns [ LA.constant 1 (length stockData) | _ <- [1 .. degree]
                         , LA.fromList [stockOpen s ^ n | s <- stockData] | n <- [1 .. degree]
                         ]
      y = HM.fromList [stockClose s | s <- stockData]
  case HM.linearRegression x y of
    HM.Failed msg -> Left ("Polynomial regression failed: " ++ msg)
    HM.Converged coefficients -> Right (LA.toList coefficients)

main :: IO ()
main = do
  let filePath = "path/to/your/spreadsheet.csv"
  result <- readStockData filePath >>= either (pure . Left) (pure . validateStockData) >>= either (pure . Left) (\data' -> pure (performPolynomialRegression data' 2)) -- Change the degree as needed
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right coefficients -> putStrLn $ "Polyn
