module Main where
import ProcessCSVData
import StockRegression

main :: IO ()
main = do
  let filePath = "/Users/gregorioflorentino/Downloads/AAPL.csv"
  result <- readStockData filePath
  case result of
    Left err -> putStrLn $ "Error reading stock data: " ++ err
    Right stockData -> do
      putStrLn "Stock Performance Data:"
      -- print stockData
      
      case performLinearRegression stockData of
         Right model -> case model of
          (LinearRegressionModel coeffs) -> putStrLn $ "Linear Regression Model: Close = " ++ show (head coeffs)  ++ " + " ++ show (coeffs !! 1) ++ " * Open"
          (PolynomialRegressionModel deg coeffs) -> putStrLn $ "Polynomial Regression Model: Deg = " ++ show deg  ++ ", " ++ show coeffs
         Left err -> putStrLn err