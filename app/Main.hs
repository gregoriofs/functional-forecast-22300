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
      print stockData
      
      case performLinearRegression stockData of
         Right (LinearRegressionModel coeff) -> putStrLn $ "Linear Regression Model: Close = " ++ show head coeff  ++ " + " ++ show coeff !! 1 ++ " * Open"
         Left err -> putStrLn err