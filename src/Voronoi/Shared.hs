module Voronoi.Shared
  where

filterVoronoi :: ([a] -> Bool) -> [([Double], [a])] -> [([Double], [a])]
filterVoronoi cellTester = filter (\(_, cell) -> cellTester cell)

removeDegenerateCells :: [([Double], [a])] -> [([Double], [a])]
removeDegenerateCells = filterVoronoi (not . null)
