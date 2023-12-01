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
      
      let (intercept, slope) = performLinearRegression stockData
      putStrLn $ "Linear Regression Model: Close = " ++ show intercept ++ " + " ++ show slope ++ " * Open"