module ConvexHull.OmniTruncated120Cell (vs120omnitrunc)
  where
import           Data.List
import           Math.Combinat.Permutations as P
-- http://eusebeia.dyndns.org/4d/trunc120cell
-- http://mathworld.wolfram.com/120-Cell.html

vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  map (map (/ sqrt 6376.10896088)) $ signsAll $
  nub $ zipWith permuteList perms (replicate 24 coords)
  where
  perms = filter (if allperms then const True else isEvenPermutation) (P.permutations 4)
  signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
  signsAll = concatMap signs
    where
    signs :: (Eq a, Num a) => [a] -> [[a]]
    signs = mapM (\x -> nub [x,-x])

vs120omnitrunc :: [[Double]]
vs120omnitrunc = concatMap vertices [
    ([1, 1, 1+6*phi, 7+10*phi], True),
    ([1, 1, 3+8*phi, 7+8*phi], True),
    ([1, 1, 1+4*phi, 5+12*phi], True),
    ([1, 3, phi6, phi6], True),
    ([2, 2, 4*phi3, 6+8*phi], True),
    ([2*phi2, 4+2*phi, 4*phi3, 4*phi3], True),
    ([3+2*phi, 3+2*phi, 3+8*phi, phi6], True),
    ([1+4*phi, 3+4*phi, 3+8*phi, 3+8*phi], True),
    ([2*phi3, 2*phi3, 2+8*phi, 4*phi3], True),
    ([1, 5*phi2, 4+7*phi, 6*phi2], False),
    ([1, 2*phi4, 5+7*phi, 6+5*phi], False),
    ([1, 2, 1+5*phi, 6+11*phi], False),
    ([1, phi2, 6+9*phi, 2+8*phi], False),
    ([1, phi2, 8+9*phi, 2+6*phi], False),
    ([1, 2*phi, 7+9*phi, 2+7*phi], False),
    ([1, phi3, 5+12*phi, 3+2*phi], False),
    ([1, 3+phi, 4+9*phi, 4*phi3], False),
    ([1, 1+3*phi, 8+9*phi, 4*phi2], False),
    ([1, 1+3*phi, 6+11*phi, 4+2*phi], False),
    ([1, 4*phi, 7+9*phi, 4+5*phi], False),
    ([1, 3*phi2, 4+9*phi, 6*phi2], False),
    ([1, 2*phi3, 5+9*phi, 6+5*phi], False),
    ([2, phi2, 5+12*phi, phi4], False),
    ([2, 2+phi, 5+9*phi, 3+8*phi], False),
    ([2, phi3, 8+9*phi, phi5], False),
    ([2, 3*phi, 7+9*phi, 3*phi3], False),
    ([2, 1+3*phi, 7+10*phi, 4+3*phi], False),
    ([2, 3+2*phi, 4+9*phi, 5+7*phi], False),
    ([2, 1+4*phi, 6+9*phi, 5*phi2], False),
    ([phi2, 4+5*phi, 3+8*phi, 6*phi2], False),
    ([phi2, 3*phi3, 4*phi3, 6+5*phi], False),
    ([phi2, 3, 2*phi3, 6+11*phi], False),
    ([phi2, 3*phi, 5+12*phi, 2*phi2], False),
    ([phi2, 3+2*phi, 4*phi, 6+11*phi], False),
    ([phi2, 4+3*phi, phi6, 6*phi2], False),
    ([phi2, 3+4*phi, 6+8*phi, 6+5*phi], False),
    ([3, phi3, 7+10*phi, 3+4*phi], False),
    ([3, 2*phi2, 5+9*phi, 4+7*phi], False),
    ([3, 1+3*phi, 6+9*phi, 2*phi4], False),
    ([2*phi, 4*phi2, 4*phi3, 6*phi2], False),
    ([2*phi, phi5, phi6, 6+5*phi], False),
    ([2*phi, 2+phi, 1+3*phi, 5+12*phi], False),
    ([2*phi, 3+phi, 1+4*phi, 6+11*phi], False),
    ([2+phi, 4*phi, 7+10*phi, 3*phi2], False),
    ([2+phi, 4+2*phi, phi6, 5+7*phi], False),
    ([2+phi, 2*phi3, 7+8*phi, 5*phi2], False),
    ([phi3, 4+5*phi, 2+8*phi, 5+7*phi], False),
    ([phi3, 5*phi2, 2+7*phi, 4*phi3], False),
    ([3+phi, 3*phi, 7+10*phi, 2*phi3], False),
    ([3+phi, 3+2*phi, 6+8*phi, 4+7*phi], False),
    ([3+phi, phi4, 7+8*phi, 2*phi4], False),
    ([3*phi, 4*phi2, 3+8*phi, 5+7*phi], False),
    ([3*phi, 2+6*phi, phi6, 5*phi2], False),
    ([2*phi2, 1+4*phi, 8+9*phi, 3*phi2], False),
    ([2*phi2, 1+5*phi, 7+8*phi, 4+5*phi], False),
    ([1+3*phi, 1+6*phi, 6+8*phi, 4+5*phi], False),
    ([1+3*phi, 3*phi3, 2+8*phi, 4+7*phi], False),
    ([1+3*phi, 2+7*phi, 3+8*phi, 2*phi4], False),
    ([1+3*phi, 3+2*phi, 2*phi3, 8+9*phi], False),
    ([1+3*phi, 4+3*phi, 3+8*phi, 4*phi3], False),
    ([3+2*phi, 1+4*phi, 7+8*phi, 3*phi3], False),
    ([3+2*phi, 2*phi3, 7+9*phi, 4+3*phi], False),
    ([3+2*phi, 1+5*phi, 6+9*phi, 4*phi2], False),
    ([4*phi, phi5, 3+8*phi, 4+7*phi], False),
    ([4*phi, 2+6*phi, 4*phi3, 2*phi4], False),
    ([phi4, 4*phi2, 1+6*phi, 5+9*phi], False),
    ([phi4, 4+2*phi, 3+4*phi, 7+9*phi], False),
    ([phi4, 3*phi2, 2+8*phi, phi6], False),
    ([4+2*phi, 1+4*phi, 6+9*phi, phi5], False),
    ([1+4*phi, 1+6*phi, phi6, 3*phi3], False),
    ([1+4*phi, 3*phi2, 2+7*phi, 6+8*phi], False),
    ([1+4*phi, 4+3*phi, 2+6*phi, 5+9*phi], False),
    ([2*phi3, 1+6*phi, 4+9*phi, phi5], False),
    ([2*phi3, 1+5*phi, phi6, 2+7*phi], False),
    ([1+5*phi, 3+4*phi, 2+6*phi, 4+9*phi], False)
    ]
    where
        phi = (1+sqrt 5) / 2
        phi2 = phi*phi
        phi3 = phi2*phi
        phi4 = phi3*phi
        phi5 = phi4*phi
        phi6 = phi5*phi




