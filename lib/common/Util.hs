module Common.Util ((...), (.>), (<.), (|>), Distribution, Histogram, annotateBy, histogram, histMin, histMax, histAverage, distribution, entropy) where

import Data.List (group, sort)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

type Histogram a = Map a Int

type Distribution a = Map a Double

infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(f .> g) x = g (f x)

infixr 9 <.

(<.) :: (b -> c) -> (a -> b) -> (a -> c)
(f <. g) x = f (g x)

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 9 ...

(...) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(...) = (.) . (.)

annotateBy :: (a -> b) -> a -> (b, a)
annotateBy f x = (f x, x)

histogram :: (Ord a) => [a] -> Histogram a
histogram = Map.fromList . map count . group . sort
  where
    count xs = (head xs, length xs)

histMin :: (Ord a) => Histogram a -> a
histMin = Map.findMin .> fst

histMax :: (Ord a) => Histogram a -> a
histMax = Map.findMax .> fst

histAverage :: (Integral a) => Histogram a -> Double
histAverage h = fromIntegral total / fromIntegral count
  where
    total = Map.assocs h |> map (\(k, v) -> v * fromIntegral k) |> sum
    count = Map.elems h |> sum

distribution :: (Ord a) => [a] -> Distribution a
distribution xs = toProbability <$> histogram xs
  where
    toProbability count = fromIntegral count / fromIntegral (length xs)

entropy :: (Ord a) => [a] -> Distribution a
entropy xs = Map.map toEntropy $ distribution xs
  where
    toEntropy p = -p * logBase 2 p
