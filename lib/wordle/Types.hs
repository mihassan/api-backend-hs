module Wordle.Types (Letter, Word, WordBank, Feedback (..), WordFeedback, Attempt, Attempts, Solver (..), RankingStrategy, AnalysisReport (..)) where

import Common.Util
import Prelude hiding (Word)

type Letter = Char

type Word = [Letter]

type WordBank = [Word]

data Feedback = Correct | Misplaced | Absent deriving (Show, Eq, Ord)

type WordFeedback = [Feedback]

type Attempt = (Word, WordFeedback)

type Attempts = [Attempt]

data Solver
  = RandomSolver
  | NaiveSolver
  | FastestSolver
  | FastSolver
  | ModerateSolver
  | SlowerSolver
  | SlowestSolver
  deriving (Show, Eq, Ord, Bounded, Enum)

type RankingStrategy = WordBank -> Word -> Double

data AnalysisReport = AnalysisReport
  { histogramOfAttempts :: Histogram Int,
    minAttempts :: Int,
    maxAttempts :: Int,
    avgAttempts :: Double
  }
  deriving (Show)
