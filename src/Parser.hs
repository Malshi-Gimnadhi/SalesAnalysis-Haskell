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
import Control.Applicative ((<|>), optional)
import qualified Data.HashMap.Strict as HM
import Control.Exception (try, SomeException)

-- CSV intermediate row representation
data Row = Row
  { rDate      :: !Text
  , rOrderId   :: !(Maybe Text)
  , rProduct   :: !Text
  , rQuantity  :: !Int
  , rUnitPrice :: !Double
  , rCustomer  :: !(Maybe Text)
  } deriving Show

-- Optional field lookup for cassava
(.:?) :: FromField a => NamedRecord -> Name -> Parser (Maybe a)
m .:? name = case HM.lookup name m of
  Nothing -> pure Nothing
  Just bs -> optional (parseField bs)

instance FromNamedRecord Row where
  parseNamedRecord m = Row
    <$> m .: "Date"
    <*> m .:? "OrderID"
    <*> m .: "Item"
    <*> m .: "Quantity"
    <*> m .: "Price"
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

-- Parse CSV file path into ([Sale], failedCount)
-- Returns Left err for file/CSV-level errors or when no valid rows
-- Returns Right (sales, failedCount) on success
parseSalesFile :: FilePath -> IO (Either String ([Sale], Int))
parseSalesFile fp = do
  result <- try $ BL.readFile fp
  case result of
    Left e -> return $ Left (show (e :: SomeException))
    Right csv -> case decodeByName csv of
      Left err -> return $ Left err
      Right (_, v) -> do
        let maybeSales = V.toList $ V.map toSale v
            successful = [s | Just s <- maybeSales]
            failedCount = length maybeSales - length successful
        if null successful
          then return $ Left ("No valid rows parsed. " ++ show failedCount ++ " row(s) failed conversion (check Date format and headers).")
          else return $ Right (successful, failedCount)