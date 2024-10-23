module Scrabble.Grid (Cell, Grid, RenderedGrid, render, chainToGrid) where

import Data.Map (Map)
import Data.Map qualified as M

data Direction = Horizontal | Vertical deriving (Eq, Show)

type Cell = (Int, Int)

type Grid = Map Cell Char

type RenderedGrid = [String]

bounds :: Grid -> (Cell, Cell)
bounds grid | M.null grid = ((0, 0), (0, 0))
bounds grid =
  let keys = M.keys grid
      xs = map fst keys
      ys = map snd keys
   in ((minimum xs, minimum ys), (maximum xs, maximum ys))

size :: Grid -> (Int, Int)
size grid =
  let ((minX, minY), (maxX, maxY)) = bounds grid
   in (maxX - minX + 1, maxY - minY + 1)

normalize :: Grid -> Grid
normalize grid =
  let ((minX, minY), _) = bounds grid
      adjust (x, y) = (x - minX, y - minY)
   in M.mapKeys adjust grid

render :: Grid -> RenderedGrid
render grid =
  let grid' = normalize grid
      (sizeX, sizeY) = size grid'
      renderCell (x, y) = M.findWithDefault '.' (x, y) grid'
   in [[renderCell (x, y) | x <- [0 .. sizeX - 1]] | y <- [0 .. sizeY - 1]]

placeWord :: Grid -> String -> Direction -> Cell -> Grid
placeWord grid word dir (x, y) = M.union grid m
  where
    cells = case dir of
      Horizontal -> (\d -> (x + d, y)) <$> [0 ..]
      Vertical -> (\d -> (x, y + d)) <$> [0 ..]
    m = M.fromList $ zip cells word

chainToGrid :: [String] -> Grid
chainToGrid chain = go chain Horizontal M.empty
  where
    go :: [String] -> Direction -> Grid -> Grid
    go [] _ grid = grid
    go (w : ws) dir grid = go ws dir' grid'
      where
        (_, (maxX, maxY)) = bounds grid
        grid' = placeWord grid w dir (maxX, maxY)
        dir' = if dir == Horizontal then Vertical else Horizontal
