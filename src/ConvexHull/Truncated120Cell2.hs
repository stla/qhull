module ConvexHull.Truncated120Cell2
  where
import Math.Combinat.Permutations as P
import Data.List
-- http://eusebeia.dyndns.org/4d/trunc120cell
-- http://mathworld.wolfram.com/120-Cell.html

signs :: (Eq a, Num a) => [a] -> [[a]]
--signs [] = [[]]
signs = mapM (\x -> nub [x,-x])

signsAll :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll = concatMap signs

vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  signsAll $
  nub $ zipWith permuteList perms (replicate 24 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (P.permutations 4)


vs = map vertices [([0,0,2,2], True)
         , ([1,1,1,sqrt 5], True)
         , ([1/phi/phi, phi, phi, phi], True)
         , ([1/phi, 1/phi, 1/phi, phi*phi] ,True)
         , ([0, 1/phi/phi, 1, phi*phi], False)
         , ([0, 1/phi, phi, sqrt 5], False)
         , ([1/phi, 1, phi, 2], False)
         ]

phi :: Double
phi = (1+ sqrt 5) / 2

-- vs6 = vertices6 [0, phi*phi, 3*phi*phi*phi, 2+5*phi]
-- vs7 = vertices7 [0, phi*phi, 5+6*phi, phi*phi*phi*phi]
-- vs8 = vertices8 [0, 2*phi, 2*phi*phi*phi*phi, 2*phi*phi*phi]
-- vs9 = vertices9 [1, phi*phi, 4+7*phi, 2*phi*phi]
-- vs10 = vertices10 [1, phi*phi*phi, 3*phi*phi*phi, 3+4*phi]
-- vs11 = vertices11 [phi*phi, 2*phi, 4+7*phi, phi*phi*phi]
-- vs12 = vertices12 [phi*phi, 2*phi*phi, 4+5*phi, 3+4*phi]
-- vs13 = vertices13 [phi*phi, 2*phi*phi*phi, 2+5*phi, 3+4*phi]
-- vs14 = vertices14 [2*phi, phi*phi*phi*phi, phi*phi*phi*phi*phi, 3+4*phi]
-- vs15 = vertices15 [phi*phi*phi, 2*phi*phi, phi*phi*phi*phi*phi, 2+5*phi]
-- vs16 = vertices16 [phi*phi*phi, 1+3*phi, 4+5*phi, 2*phi*phi*phi]
-- vs17 = vertices17 [2*phi*phi, 1+3*phi, 3*phi*phi*phi, phi*phi*phi*phi]
--
-- allVertices = vs1 ++ vs2 ++ vs3 ++ vs4 ++ vs5 ++ vs6 ++ vs7 ++ vs8 ++ vs9 ++ vs10 ++ vs11 ++ vs12 ++ vs13 ++ vs14 ++ vs15 ++ vs16 ++ vs17
