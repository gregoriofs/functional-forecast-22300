{-# LANGUAGE OverloadedStrings #-}
module Main where
import ProcessCSVData
import StockRegression
import ApiRequests
import Plotting

import Options.Applicative
import Data.Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)

data CommandLineOptions = CommandLineOptions
  { tickerSymbol :: String
  , startDate :: Day
  , endDate :: Day
  }

-- Parser for command line options
commandLineParser :: Parser CommandLineOptions
commandLineParser = CommandLineOptions
  <$> argument str (metavar "TICKER_SYMBOL" <> help "Stock ticker symbol")
  <*> option parseDate (long "start-date" <> short 's' <> metavar "START_DATE" <> help "Start date (MM-DD-YYYY)")
  <*> option parseDate (long "end-date" <> short 'e' <> metavar "END_DATE" <> help "End date (MM-DD-YYYY)")

-- Parse a date in the format MM-DD-YYYY
parseDate :: ReadM Day
parseDate = eitherReader $ \s -> case parseTimeM True defaultTimeLocale "%m-%d-%Y" s of
  Just day -> Right day
  Nothing  -> Left "Invalid date format. Please use MM-DD-YYYY."

-- Run the parser and handle errors
parseCommandLine :: IO CommandLineOptions
parseCommandLine = execParser opts
  where
    opts = info (commandLineParser <**> helper) fullDesc
    
main :: IO ()
main = do
  let filePath = "/Users/prathamgandhi/Downloads/AAPL.csv"
  result <- readStockData filePath
  -- Read ticker, dates
  --params <- parseCommandLine
  --result <- requestInformation (tickerSymbol params) (startDate params) (endDate params)

  case result of
    Left err -> putStrLn $ "Error reading stock data: " ++ err
    Right stockData -> do
      putStrLn "Stock Performance Data:"
      print stockData
      let chart = createClosePriceChart stockData
      saveChartAsHtml "stockPerformance.html" chart
      putStrLn "Visualization saved to stockPerformance.html"
      case performLinearRegression stockData of
         Right model -> case model of
          (LinearRegressionModel coeffs) -> putStrLn $ "Linear Regression Model: coeffs = " ++ show coeffs
          (PolynomialRegressionModel deg coeffs) -> putStrLn $ "Polynomial Regression Model: Deg = " ++ show coeffs
         Left err -> putStrLn err