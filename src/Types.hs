{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import Data.Time (Day)

-- A simple Sale record used across the project
data SaleRecord = SaleRecord
  { orderId  :: Maybe Int
  , customer :: Text
  , item     :: Text
  , quantity :: Int
  , price    :: Double
  , date     :: Day
  } deriving (Show, Eq)
