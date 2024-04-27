module Common.Util
  ( (...),
    (.>),
    (<.),
    (|>),
    Distribution,
    Histogram,
    annotateBy,
    histogram,
    histMin,
    histMax,
    histAverage,
    distributionFromHistogram,
    distribution,
    entropyFromHistogram,
    entropy,
  )
where

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

distributionFromHistogram :: Histogram a -> Distribution a
distributionFromHistogram hs = hs |> Map.map ((/ total) . fromIntegral)
  where
    total = Map.elems hs |> sum |> fromIntegral

distribution :: (Ord a) => [a] -> Distribution a
distribution = histogram .> distributionFromHistogram

entropyFromHistogram :: Histogram a -> Distribution a
entropyFromHistogram = distributionFromHistogram .> Map.map toEntropy
  where
    toEntropy p = -p * logBase 2 p

entropy :: (Ord a) => [a] -> Distribution a
entropy = histogram .> entropyFromHistogram
