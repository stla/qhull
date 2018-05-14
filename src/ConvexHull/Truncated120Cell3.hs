module ConvexHull.Truncated120Cell3
  where
import           Data.List
import           Math.Combinat.Permutations as P
-- http://eusebeia.dyndns.org/4d/trunc120cell
-- http://mathworld.wolfram.com/120-Cell.html

signs :: (Eq a, Num a) => [a] -> [[a]]
signs = mapM (\x -> nub [x,-x])

signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll = concatMap signs

vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  map (map (/ sqrt 270.1640786)) $ signsAll $
  nub $ zipWith permuteList perms (replicate 24 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (P.permutations 4)

vs120trunc = concatMap vertices [([1, 3+4*phi, 3+4*phi, 3+4*phi], True)
         , ([phi3, phi3, phi3, 5+6*phi], True)
         , ([2*phi2, 2*phi2, 2*phi2, 2*phi4], True)
         , ([0, 1, 4+5*phi, phi5] ,False)
         , ([0, 1, 4+7*phi, 1+3*phi], False)
         , ([0, phi2, 3*phi3, 2+5*phi], False)
         , ([0, phi2, 5+6*phi, phi4], False)
         , ([0, 2*phi, 2*phi4, 2*phi3], False)
         , ([1, phi2, 4+7*phi, 2*phi2], False)
         , ([1, phi3, 3*phi2, 3+4*phi], False)
         , ([phi2, 2*phi, 4+7*phi, phi3], False)
         , ([phi2, 2*phi2, 4+5*phi, 3+4*phi], False)
         , ([phi2, 2*phi3, 2+5*phi, 3+4*phi], False)
         , ([2*phi, phi4, phi5, 3+4*phi], False)
         , ([phi3, 2*phi2, phi5, 2+5*phi], False)
         , ([phi3, 1+3*phi, 4+5*phi, 2*phi3], False)
         , ([2*phi2, 1+3*phi, 3*phi3, phi4], False)
         ]
  where
     phi = (1+sqrt 5) / 2
     phi3 = phi*phi*phi
     phi2 = phi*phi
     phi4 = phi*phi*phi*phi
     phi5 = phi*phi*phi*phi*phi
