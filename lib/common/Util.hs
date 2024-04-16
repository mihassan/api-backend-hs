module Common.Util ((...), annotateBy, histogram, distribution) where

import Data.List (group, sort)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

type Histogram a = Map a Int

type Distribution a = Map a Double

infixr 9 ...

(...) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(...) = (.) . (.)

annotateBy :: (a -> b) -> a -> (b, a)
annotateBy f x = (f x, x)

histogram :: (Ord a) => [a] -> Histogram a
histogram = Map.fromList . map count . group . sort
  where
    count xs = (head xs, length xs)

distribution :: (Ord a) => [a] -> Distribution a
distribution xs = toProbability <$> histogram xs
  where
    toProbability count = fromIntegral count / fromIntegral (length xs)
