module ConvexHull.BiTruncatedTesseract
  where
import Math.Combinat.Permutations as P
import Data.List

signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll = concatMap signs
  where
  signs :: (Eq a, Num a) => [a] -> [[a]]
  signs = mapM (\x -> nub [x,-x])
  

vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  map (map (/ sqrt 18)) $ signsAll $
  nub $ zipWith permuteList perms (replicate 24 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (P.permutations 4)


biTruncatedTesseract :: [[Double]]
biTruncatedTesseract = concatMap vertices 
  [ ([0, sqrt 2, 2 * sqrt 2, 2 * sqrt 2], True) ]

