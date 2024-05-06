module Wordle.Types
  ( Letter,
    Word,
    WordBank,
    Feedback (..),
    WordFeedback,
    Attempt (..),
    Attempts,
    Solver (..),
    RankingStrategy,
    AnalysisReport (..),
  )
where

import Common.Util
import Data.Aeson
import GHC.Generics
import Prelude hiding (Word)

type Letter = Char

type Word = [Letter]

-- | A list of possible words.
type WordBank = [Word]

-- | Feedback for a letter in a word guess.
-- 'Correct' means the letter is in the correct position.
-- 'Misplaced' means the letter is in the word but not in the correct position.
-- 'Absent' means the letter is not in the word.
data Feedback
  = Correct
  | Misplaced
  | Absent
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Feedback

-- | Feedback for a word guess for each letter in the same order.
type WordFeedback = [Feedback]

-- | An attempt to guess the hidden word.
-- Contains the word guess and the feedback for each letter.
data Attempt = Attempt
  { word :: Word,
    feedback :: WordFeedback
  }
  deriving (Show, Eq, Generic)

instance FromJSON Attempt

-- | A list of attempts to guess the hidden word in order.
type Attempts = [Attempt]

-- | A list of available solvers.
data Solver
  = RandomSolver
  | NaiveSolver
  | FastSolver1
  | FastSolver2
  | MixedSolver1
  | MixedSolver2
  | SlowSolver1
  | SlowSolver2
  deriving (Show, Eq, Bounded, Enum, Generic)

instance FromJSON Solver

-- | A strategy to rank words in the word bank.
-- Higher value means the word is more likely to be the hidden word.
-- The range of the value is not specified.
type RankingStrategy = WordBank -> Word -> Double

-- | An analysis report for a solver to evaluate its performance.
data AnalysisReport = AnalysisReport
  { histogramOfAttempts :: Histogram Int,
    minAttempts :: Int,
    maxAttempts :: Int,
    avgAttempts :: Double
  }
  deriving (Show)