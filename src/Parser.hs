{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Parser
  ( parseSalesFile
  ) where

import Types
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

-- CSV intermediate row representation
data Row = Row
  { rDate      :: !Text
  , rOrderId   :: !(Maybe Text)
  , rProduct   :: !Text
  , rQuantity  :: !Int
  , rUnitPrice :: !Double
  , rCustomer  :: !(Maybe Text)
  } deriving Show

instance FromNamedRecord Row where
  parseNamedRecord m = Row
    <$> m .: "Date"
    <*> m .:? "OrderID"
    <*> m .: "Product"
    <*> m .: "Quantity"
    <*> m .: "UnitPrice"
    <*> m .:? "Customer"

-- parse date text "YYYY-MM-DD" or other common formats
parseDay :: Text -> Maybe Day
parseDay t =
  let s = T.unpack t
      try fm = parseTimeM True defaultTimeLocale fm s :: Maybe Day
  in  try "%Y-%m-%d"
      <|> try "%d/%m/%Y"
      <|> try "%Y/%m/%d"

-- Convert Row -> Maybe Sale (fails when date parse fails)
toSale :: Row -> Maybe Sale
toSale Row{..} = do
  day <- parseDay rDate
  return $ Sale
    { sDate = day
    , sOrderId = rOrderId
    , sProduct = rProduct
    , sQuantity = rQuantity
    , sUnitPrice = rUnitPrice
    , sCustomer = rCustomer
    }

-- Parse CSV file path into [Sale]
parseSalesFile :: FilePath -> IO (Either String [Sale])
parseSalesFile fp = do
  csv <- BL.readFile fp
  case decodeByName csv of
    Left err -> return $ Left ("CSV parse error: " ++ err)
    Right (_, v) -> do
      let maybeSales = V.toList $ V.map toSale v
          successful = [s | Just s <- maybeSales]
          failed = length maybeSales - length successful
      if null successful
        then return $ Left "No valid rows parsed (check Date format and headers)"
        else return $ Right successful