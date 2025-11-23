{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Sale(..)
  , Summary(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (Day)

-- Sale record
data Sale = Sale
  { sDate     :: Day
  , sOrderId  :: Maybe Text
  , sProduct  :: Text
  , sQuantity :: Int
  , sUnitPrice:: Double
  , sCustomer :: Maybe Text
  } deriving (Show, Eq, Generic)

-- Summary to display / output
data Summary = Summary
  { totalRevenue   :: Double
  , totalQuantity  :: Int
  , recordCount    :: Int
  } deriving (Show, Eq)
