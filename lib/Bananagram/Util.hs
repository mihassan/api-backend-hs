module Bananagram.Util (histogram, isSubMapOf, startsWith) where

import Data.Map (Map)
import Data.Map qualified as M

histogram :: (Ord a) => [a] -> Map a Int
histogram = M.fromListWith (+) . map (,1)

isSubMapOf :: (Ord a) => Map a Int -> Map a Int -> Bool
isSubMapOf = M.isSubmapOfBy (<=)

startsWith :: (Eq a) => a -> [a] -> Bool
startsWith _ [] = False
startsWith x (y : _) = x == y
