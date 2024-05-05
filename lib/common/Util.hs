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

-- | A histogram is a map from a value to its frequency.
type Histogram a = Map a Int

-- | A distribution is a map from a value to its probability.
-- The sum of all probabilities should be 1.
type Distribution a = Map a Double

-- | Left associative function composition with flipped arguments.
infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(f .> g) x = g (f x)

-- | Right associative function composition an alias for '(.)'.
-- This is defined for symmetry with '.>'.
infixr 9 <.

(<.) :: (b -> c) -> (a -> b) -> (a -> c)
(<.) = (.)

-- | Left associative apply operator.
infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | Compose 2 functions where the second function takes 2 arguments.
infixr 9 ...

(...) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(...) = (.) . (.)

-- | Annotate a value with a function. This is useful in decorate-sort-undecorate pattern.
annotateBy :: (a -> b) -> a -> (b, a)
annotateBy f x = (f x, x)

-- | Compute the histogram of a list, takes /O(n log n)/ time.
histogram :: (Ord a) => [a] -> Histogram a
histogram = Map.fromList . map toKeyLength . group . sort
  where
    toKeyLength xs = (head xs, length xs)

-- | Get the mnimum key in a histogram.
histMin :: (Ord a) => Histogram a -> a
histMin = Map.findMin .> fst

-- | Get the maximum key in a histogram.
histMax :: (Ord a) => Histogram a -> a
histMax = Map.findMax .> fst

-- | Compute the average of the keys weighted by their frequencies.
histAverage :: (Integral a) => Histogram a -> Double
histAverage h = fromIntegral total / fromIntegral count
  where
    total = Map.assocs h |> map (\(k, v) -> v * fromIntegral k) |> sum
    count = Map.elems h |> sum

-- | Convert a histogram to a distribution.
distributionFromHistogram :: Histogram a -> Distribution a
distributionFromHistogram hs = hs |> Map.map ((/ total) . fromIntegral)
  where
    total = Map.elems hs |> sum |> fromIntegral

-- | Compute the distribution of a list, takes /O(n log n)/ time.
distribution :: (Ord a) => [a] -> Distribution a
distribution = histogram .> distributionFromHistogram

-- | Compute the entropy from a histogram.
entropyFromHistogram :: Histogram a -> Distribution a
entropyFromHistogram = distributionFromHistogram .> Map.map toEntropy
  where
    toEntropy p = -p * logBase 2 p

-- | Compute the entropy of a list.
entropy :: (Ord a) => [a] -> Distribution a
entropy = histogram .> entropyFromHistogram
