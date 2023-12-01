{-# LANGUAGE DeriveGeneric #-}
module ProcessCSVData where
    
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import GHC.Generics
import qualified Data.Vector as V

data StockPerformance = StockPerformance
  { date :: String
  , open :: Double
  , high :: Double
  , low :: Double
  , close :: Double
  , volume :: Int
  , adjClose :: Double
  }
  deriving (Show, Generic)

customFieldNameModifier :: String -> String
customFieldNameModifier "date" = "Date"
customFieldNameModifier "open" = "Open"
customFieldNameModifier "high" = "High"
customFieldNameModifier "low" = "Low"
customFieldNameModifier "close" = "Close"
customFieldNameModifier "volume" = "Volume"
customFieldNameModifier "adjClose" = "Adj Close"
customFieldNameModifier other = other

instance FromNamedRecord StockPerformance where
  parseNamedRecord = genericParseNamedRecord
    defaultOptions { fieldLabelModifier = customFieldNameModifier }

instance ToNamedRecord StockPerformance where
  toNamedRecord = genericToNamedRecord
    defaultOptions { fieldLabelModifier = customFieldNameModifier }
instance DefaultOrdered StockPerformance

readStockData :: FilePath -> IO (Either String [StockPerformance])
readStockData filePath = do
  csvData <- BL.readFile filePath
  -- Use V.toList to convert the Vector to a list
  case decodeByName csvData of
    Left err -> return (Left err)
    Right (_, records) -> return (Right (V.toList records))