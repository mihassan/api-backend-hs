module Main where

import Control.Monad
import System.TimeIt
import Wordle.Analyzer
import Wordle.Solver
import Wordle.Types
import Wordle.WordBank

runAnalysis :: Solver -> WordBank -> IO ()
runAnalysis s wb = do
  let r = analyze s wb
  putStrLn $ "Analysis: " ++ show r

main :: IO ()
main = do
  forM_ [minBound .. maxBound] $ \s -> do
    putStrLn $ "Running analysis for solver: " ++ show s
    timeIt $ runAnalysis s wordBank
    putStrLn "\n\n"
