module Wordle.Type
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

type WordBank = [Word]

data Feedback
  = Correct
  | Misplaced
  | Absent
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Feedback

type WordFeedback = [Feedback]

data Attempt = Attempt
  { word :: Word,
    feedback :: WordFeedback
  }
  deriving (Show, Eq, Generic)

instance FromJSON Attempt

type Attempts = [Attempt]

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

type RankingStrategy = WordBank -> Word -> Double

data AnalysisReport = AnalysisReport
  { histogramOfAttempts :: Histogram Int,
    minAttempts :: Int,
    maxAttempts :: Int,
    avgAttempts :: Double
  }
  deriving (Show)
