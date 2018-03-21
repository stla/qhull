module ConvexHull.SnubDodecahedron.SnubDodecahedron where
import           Data.List                  hiding (permutations)
import           Math.Combinat.Permutations

signsAll2 :: (Eq a, Num a) => [[a]] -> [[a]]
signsAll2 = concatMap signs
  where
    signs :: (Eq a, Num a) => [a] -> [[a]]
    signs [x,y,z] = nub [[x,y,-z], [x,-y,z], [-x,y,z], [-x,-y,-z]]


vertices :: ([Double], Bool) -> [[Double]]
vertices (coords, allperms) =
  -- map (map (/ sqrt 128.9619)) $
  signsAll2 $
  nub $ zipWith permuteList perms (replicate 12 coords)
  where perms = filter (if allperms then const True else isEvenPermutation) (permutations 3)

snubDodecahedron :: [[Double]]
snubDodecahedron = concatMap vertices
  [ ([2*alpha, 2, 2*beta], False)
   , ([alpha + beta/phi + phi, -alpha*phi + beta + 1/phi, alpha/phi + beta*phi-1], False)
   , ([alpha + beta/phi - phi, alpha*phi - beta + 1/phi, alpha/phi + beta*phi+1], False)
   , ([-alpha/phi + beta*phi+1, -alpha + beta/phi - phi, alpha*phi + beta - 1/phi], False)
   , ([-alpha/phi + beta*phi-1, alpha - beta/phi - phi, alpha*phi + beta + 1/phi], False) ]
  where
    alpha = xi - 1/xi
    beta = xi*phi + phi*phi + phi/xi
    xi = 1.7155615
    phi = (1 + sqrt 5)/2

