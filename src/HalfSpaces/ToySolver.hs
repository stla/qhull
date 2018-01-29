module HalfSpaces.ToySolver
  (interiorPoint)
  where
import           Control.Monad                (replicateM_)
import           Data.Default.Class
import           Data.IntMap.Strict           (IntMap, mapKeys, mergeWithKey)
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (nub)
import           Data.Ratio                   (Rational)
import           Data.VectorSpace
import           HalfSpaces.Constraint        (Constraint (..), Sense (..))
import           HalfSpaces.Internal          (varsOfConstraint)
import           HalfSpaces.LinearCombination (LinearCombination (..))
import           ToySolver.Arith.Simplex      hiding (Lt)
import qualified ToySolver.Data.LA            as LA

constraintToCoeffMap :: Int -> Constraint -> IntMap Rational
constraintToCoeffMap
  newvar (Constraint (LinearCombination lhs) sense (LinearCombination rhs)) =
  let terms = mapKeys (subtract 1)
              (mergeWithKey (\_ x y -> Just (x-y)) id (IM.map negate) lhs rhs)
  in
  if sense == Lt
    then IM.union terms (IM.singleton newvar 1)
    else IM.union (IM.map negate terms) (IM.singleton newvar 1)

constraintToAtom :: Int -> Constraint -> Atom Rational
constraintToAtom newvar constraint =
  let coeffmap = constraintToCoeffMap newvar constraint
  in
  LA.fromCoeffMap coeffmap .<=. LA.constant 0

interiorPoint :: [Constraint] -> IO [Rational]
interiorPoint constraints = do
  let vars = nub (concatMap varsOfConstraint constraints)
      dim = length (filter (/= 0) vars)
      atoms = map (constraintToAtom dim) constraints
  solver <- newSolver
  replicateM_ (dim+1) (newVar solver)
  mapM_ (assertAtom solver) atoms
  setObj solver (negateV $ LA.var dim)
  o <- optimize solver def
  print o
  mapM (getValue solver) [0 .. dim-1]
