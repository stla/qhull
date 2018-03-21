module ConvexHull.CantiTrunc600Cell.Data
  where
import Math.Combinat.Permutations as P
import Data.List
-- http://eusebeia.dyndns.org/4d/cantitrunc600cell

signs :: (Eq a, Num a) => [a] -> [[a]]
signs = mapM (\x -> nub [x,-x])

signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll = concatMap signs

vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  map (map (/ sqrt 270.1640786)) $ signsAll $
  nub $ zipWith permuteList perms (replicate 24 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (P.permutations 4)


allVertices :: [[Double]]
allVertices = concatMap vertices
  [ ([1, 1, 1+6*phi, 5+6*phi], True)
  , ([1, 3, 3+6*phi, 3+6*phi], True)
  , ([2, 2, 2+6*phi, 2*phi4], True)
  , ([0, 1, 3*phi, 3+9*phi], False)
  , ([0, 1, 5*phi, 5+7*phi], False)
  , ([0, 2, 4*phi, 4*phi3], False)
  , ([0, 4+3*phi, phi5, 5+4*phi], False)
  , ([0, 3+4*phi, 4+5*phi, 5+3*phi], False)
  , ([1, phi, 6*phi2, 1+5*phi], False)
  , ([1, 2*phi, 3+9*phi, 2+phi], False)
  , ([1, 3+phi, 3*phi, 4*phi3], False)
  , ([1, 3*phi, 6*phi2, 3*phi2], False)
  , ([1, 3+2*phi, 3*phi3, 5+4*phi], False)
  , ([1, phi4, 2*phi4, 5+3*phi], False)
  , ([phi, 2, phi3, 3+9*phi], False)
  , ([phi, 3, 1+3*phi, 4*phi3], False)
  , ([phi, 3*phi2, 2+6*phi, 5+4*phi], False)
  , ([phi, 2*phi3, 3*phi3, 5+3*phi], False)
  , ([2, 2*phi, 6*phi2, 2*phi3], False)
  , ([2, 3+phi, 3*phi3, 4+5*phi], False)
  , ([2, 3*phi, 5+7*phi, 3+2*phi], False)
  , ([2, 1+3*phi, 5+6*phi, 4+3*phi], False)
  , ([3, 2*phi, 5+7*phi, phi4], False)
  , ([3, 2+phi, 2*phi4, phi5], False)
  , ([3, phi3, 5+6*phi, 3+4*phi], False)
  , ([2*phi, 3*phi2, 1+6*phi, 4+5*phi], False)
  , ([2*phi, 4+3*phi, 1+5*phi, 3*phi3], False)
  , ([2+phi, 3+phi, 2+6*phi, 3*phi3], False)
  , ([2+phi, 1+3*phi, 6*phi2, 3+2*phi], False)
  , ([2+phi, 4*phi, 5+6*phi, 3*phi2], False)
  , ([phi3, 3+phi, phi4, 6*phi2], False)
  , ([phi3, 3+2*phi, 1+6*phi, 3*phi3], False)
  , ([phi3, 3*phi2, 5*phi, 2*phi4], False)
  , ([3+phi, 3*phi, 5+6*phi, 2*phi3], False)
  , ([3*phi, 3+2*phi, 1+5*phi, 2*phi4], False)
  , ([3*phi, 2*phi3, 1+6*phi, phi5], False)
  , ([3*phi, 1+5*phi, 2+6*phi, 3+4*phi], False)
  , ([1+3*phi, phi4, 1+6*phi, 2+6*phi], False)
  , ([1+3*phi, 5*phi, 3*phi3, 2*phi3], False)
  , ([4*phi, phi4, 1+5*phi, 3*phi3], False)]
  where
    phi2 = phi*phi
    phi3 = phi2*phi
    phi4 = phi3*phi
    phi5 = phi4*phi
    phi = (1 + sqrt 5) / 2
