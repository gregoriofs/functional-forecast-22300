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
      -- TODO: get ticker from terminal and model,  use api to request using yahoo-prices 
      case performLinearRegression stockData of
         Right model -> case model of
          (LinearRegressionModel coeffs) -> putStrLn $ "Linear Regression Model: coeffs = " ++ show coeffs
          (PolynomialRegressionModel deg coeffs) -> putStrLn $ "Polynomial Regression Model: Deg = " ++ show coeffs
         Left err -> putStrLn err