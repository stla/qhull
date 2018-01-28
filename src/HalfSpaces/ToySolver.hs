module HalfSpaces.ToySolver
  where
import           Control.Monad                (replicateM_)
import           Data.Default.Class
import           Data.IntMap.Strict           (IntMap, mapKeys, mergeWithKey)
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (nub)
import           Data.Ratio                   (Rational)
import           Data.VectorSpace
import           HalfSpaces.Constraint
import           HalfSpaces.LinearCombination
import           ToySolver.Arith.Simplex      as Simplex hiding (Lt)
import qualified ToySolver.Data.LA            as LA


constraintToCoeffMap :: Int -> Constraint -> IntMap Rational
constraintToCoeffMap
  newvar (Constraint (LinearCombination lhs) sense (LinearCombination rhs)) =
  let terms = mapKeys (subtract 1)
              (mergeWithKey (\i x y -> Just (x-y)) id (IM.map negate) lhs rhs)
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
  replicateM_ (dim+1) (Simplex.newVar solver)
  mapM_ (assertAtom solver) atoms
  setObj solver (negateV $ LA.var dim)
  o <- optimize solver def
  mapM (getValue solver) [0 .. dim-1]

cubeConstraints :: [Constraint]
cubeConstraints =
  [ x .<= constant 1
  , x .>= constant (-1)
  , y .<= constant 1
  , y .>= constant (-1)
  , z .<= constant 1
  , z .>= constant (-1) ]
  where
    x = asLinearCombination 1
    y = asLinearCombination 2
    z = asLinearCombination 3
