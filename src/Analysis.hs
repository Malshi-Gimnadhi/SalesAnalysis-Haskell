{-# LANGUAGE OverloadedStrings #-}
module Analysis
  ( analyzeSales
  , salesByProduct
  , salesByMonth
  ) where

import Types
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (toGregorian, Day)
import Data.Time.Calendar (toGregorian)
import Data.List (foldl')

-- compute total revenue, total quantity and record count
analyzeSales :: [Sale] -> (Double, Int, Int)
analyzeSales sales =
  let totals = foldl' accum (0.0, 0) sales
      accum (rev, qty) s = (rev + fromIntegral (sQuantity s) * sUnitPrice s, qty + sQuantity s)
      (rev, qty) = totals
  in (rev, qty, length sales)

-- total amount per product
salesByProduct :: [Sale] -> Map String Double
salesByProduct =
  foldl' (\m s ->
            let prod = show (sProduct s)
                amt  = fromIntegral (sQuantity s) * sUnitPrice s
            in Map.insertWith (+) prod amt m) Map.empty

-- total amount per month (format: "YYYY-MM")
monthKey :: Day -> String
monthKey d =
  let (y, m, _) = toGregorian d
  in show y ++ "-" ++ (if m < 10 then "0" ++ show m else show m)

salesByMonth :: [Sale] -> Map String Double
salesByMonth =
  foldl' (\m s ->
            let k = monthKey (sDate s)
                amt = fromIntegral (sQuantity s) * sUnitPrice s
            in Map.insertWith (+) k amt m) Map.empty
