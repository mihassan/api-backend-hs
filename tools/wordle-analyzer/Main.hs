module Main where

import Control.Monad
import System.Random
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
  gen <- newStdGen
  let idxs = randomRs (0, length wordBank - 1) gen
  let wb = (wordBank !!) <$> take 100 idxs
  forM_ [minBound .. maxBound] $ \s -> do
    putStrLn $ "Running analysis for solver: " ++ show s
    timeIt $ runAnalysis s wb
    putStrLn "\n\n"
