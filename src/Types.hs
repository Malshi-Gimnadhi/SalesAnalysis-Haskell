{-# LANGUAGE OverloadedStrings #-}

module Types
  ( SaleRecord(..)
  , Sale(..)
  ) where

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

-- Sale type used by Parser, Analysis, and GUI
data Sale = Sale
  { sDate      :: !Day
  , sOrderId   :: !(Maybe Text)
  , sProduct   :: !Text
  , sQuantity  :: !Int
  , sUnitPrice :: !Double
  , sCustomer  :: !(Maybe Text)
  } deriving (Show, Eq)