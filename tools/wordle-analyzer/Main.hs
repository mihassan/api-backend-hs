module Main where

import Control.Monad
import System.Environment
import System.TimeIt
import Wordle.Analyzer
import Wordle.Solver
import Wordle.Types
import Wordle.WordBank
import Prelude hiding (Word)

runAnalysis :: Solver -> Word -> WordBank -> IO ()
runAnalysis s g wb = do
  let r = analyze s g wb
  putStrLn $ "\nAnalysis: " ++ show r
  when (maxAttempts r > 5) $ do
    putStrLn $ "\nDifficult words: " ++ show (difficultWords 5 s g wb)

main :: IO ()
main = do
  [g] <- getArgs
  forM_ [minBound .. maxBound] $ \s -> do
    putStrLn $ "\n\nRunning analysis for solver: " ++ show s ++ " starting with word: " ++ g
    timeIt $ runAnalysis s g wordBank
