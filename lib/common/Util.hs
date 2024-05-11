module Common.Util
  ( -- * Useful operators
    (...),
    (.>),
    (<.),
    (|>),
    annotateBy,

    -- * Hisogram related functions
    Histogram,
    histogram,
    histMin,
    histMax,
    histAverage,

    -- * Distribution related functions
    Distribution,
    distributionFromHistogram,
    distribution,

    -- * Entropy related functions
    entropyFromHistogram,
    entropy,
  )
where

import Data.List (group, sort)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | Left associative function composition with flipped arguments.
--
-- >>> length .> show .> reverse $ [1..10]
-- "01"
--
-- >>> length .> odd $ "1"
-- True
infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(f .> g) x = g (f x)

-- | Right associative function composition an alias for '(.)'.
-- This is defined for symmetry with '.>'.
--
-- >>> reverse <. show <. length $ [1..10]
-- "01"
--
-- >>> odd <. length $ "1"
-- True
infixr 9 <.

(<.) :: (b -> c) -> (a -> b) -> (a -> c)
(<.) = (.)

-- | Left associative apply operator.
--
-- >>> 1 |> (+1) |> (*2)
-- 4
--
-- >>> "Fizz" |> (++ "Buzz") |> (++ "!")
-- "FizzBuzz!"
infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | Compose 2 functions where the second function takes 2 arguments.
--
-- >>> let count = length ... filter in count odd [1..10]
-- 5
infixr 9 ...

(...) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(...) = (.) . (.)

-- | Annotate a value with a function. This is useful in decorate-sort-undecorate pattern.
--
-- >>> annotateBy odd <$> [1..5]
-- [(True,1),(False,2),(True,3),(False,4),(True,5)]
--
-- >>> annotateBy length <$> ["a", "ab", "abc"]
-- [(1,"a"),(2,"ab"),(3,"abc")]
annotateBy :: (a -> b) -> a -> (b, a)
annotateBy f x = (f x, x)

-- | A histogram is a map from a value to its frequency.
type Histogram a = Map a Int

-- | Compute the histogram of a list, takes /O(n log n)/ time.
--
-- >>> histogram [1, 2, 1, 3, 1, 2, 4]
-- fromList [(1,3),(2,2),(3,1),(4,1)]
histogram :: (Ord a) => [a] -> Histogram a
histogram = Map.fromList . map toKeyLength . group . sort
  where
    toKeyLength xs = (head xs, length xs)

-- | Get the mnimum key in a histogram.
--
-- >>> histMin $ histogram [1, 2, 1, 3, 1, 2, 4]
-- 1
histMin :: (Ord a) => Histogram a -> a
histMin = Map.findMin .> fst

-- | Get the maximum key in a histogram.
--
-- >>> histMax $ histogram [1, 2, 1, 3, 1, 2, 4]
-- 4
histMax :: (Ord a) => Histogram a -> a
histMax = Map.findMax .> fst

-- | Compute the average of the keys weighted by their frequencies.
--
-- >>> histAverage $ histogram [1, 2, 1, 3, 1, 2, 4]
-- 2.0
histAverage :: (Integral a) => Histogram a -> Double
histAverage h = fromIntegral total / fromIntegral count
  where
    total = Map.assocs h |> map (\(k, v) -> v * fromIntegral k) |> sum
    count = Map.elems h |> sum

-- | A distribution is a map from a value to its probability.
-- The sum of all probabilities should be 1 if it is actually a probability distribution.
-- However, we do not enforce this constraint when used to hold entropy values.
type Distribution a = Map a Double

-- | Convert a histogram to a distribution.
--
-- >>> distributionFromHistogram $ histogram [1, 2, 1, 3, 1 ]
-- fromList [(1,0.6),(2,0.2),(3,0.2)]
distributionFromHistogram :: Histogram a -> Distribution a
distributionFromHistogram hs = hs |> Map.map ((/ total) . fromIntegral)
  where
    total = Map.elems hs |> sum |> fromIntegral

-- | Compute the distribution of a list, takes /O(n log n)/ time.
--
-- >>> distribution [1, 2, 1, 3, 1]
-- fromList [(1,0.6),(2,0.2),(3,0.2)]
distribution :: (Ord a) => [a] -> Distribution a
distribution = histogram .> distributionFromHistogram

-- | Compute the entropy from a histogram.
--
-- >>> entropyFromHistogram $ histogram [1, 2]
-- fromList [(1,0.5),(2,0.5)]
--
-- >>> entropyFromHistogram $ histogram [1, 2, 3, 4]
-- fromList [(1,0.5),(2,0.5),(3,0.5),(4,0.5)]
--
-- >>> entropyFromHistogram $ histogram [1, 2, 1, 3, 1]
-- fromList [(1,0.44217935649972373),(2,0.46438561897747244),(3,0.46438561897747244)]
entropyFromHistogram :: Histogram a -> Distribution a
entropyFromHistogram = distributionFromHistogram .> Map.map toEntropy
  where
    toEntropy p = -p * logBase 2 p

-- | Compute the entropy of a list.
--
-- >>> entropy [1, 2]
-- fromList [(1,0.5),(2,0.5)]
--
-- >>> entropy [1, 2, 3, 4]
-- fromList [(1,0.5),(2,0.5),(3,0.5),(4,0.5)]
--
-- >>> entropy [1, 2, 1, 3, 1]
-- fromList [(1,0.44217935649972373),(2,0.46438561897747244),(3,0.46438561897747244)]
entropy :: (Ord a) => [a] -> Distribution a
entropy = histogram .> entropyFromHistogram
