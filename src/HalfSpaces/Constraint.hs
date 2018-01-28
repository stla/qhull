module HalfSpaces.Constraint
  where
import           Data.IntMap.Strict           (mergeWithKey)
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (nub, union)
import           Data.Ratio                   (Rational)
import           HalfSpaces.LinearCombination (LinearCombination, VarIndex,
                                               constant,
                                               normalizeLinearCombination,
                                               varsOfLinearCombo)

data Sense = Gt | Lt
  deriving Eq

instance Show Sense where
  show Gt = ">="
  show Lt = "<="

data Constraint = Constraint LinearCombination Sense LinearCombination
  deriving (Eq, Show)

(.>=.) :: LinearCombination -> LinearCombination -> Constraint
(.>=.) lhs rhs = Constraint lhs Gt rhs

(.<=.) :: LinearCombination -> LinearCombination -> Constraint
(.<=.) lhs rhs = Constraint lhs Lt rhs

(.>=) :: LinearCombination -> Rational -> Constraint
(.>=) lhs x = (.>=.) lhs (constant x)

(.<=) :: LinearCombination -> Rational -> Constraint
(.<=) lhs x = (.<=.) lhs (constant x)

varsOfConstraint :: Constraint -> [VarIndex]
varsOfConstraint (Constraint lhs _ rhs) =
  varsOfLinearCombo lhs `union` varsOfLinearCombo rhs

normalizeConstraint :: [VarIndex] -> Constraint -> [Double]
normalizeConstraint vars (Constraint lhs sense rhs) =
  let (x:xs) = map realToFrac $
               IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
  in
  if sense == Lt
    then xs ++ [x]
    else map negate xs ++ [-x]
  where lhs' = normalizeLinearCombination vars lhs
        rhs' = normalizeLinearCombination vars rhs

normalizeConstraints :: [Constraint] -> [[Double]] -- for qhalf
normalizeConstraints constraints = map (normalizeConstraint vars) constraints
  where
    vars = nub $ concatMap varsOfConstraint constraints

-- x = newVar 1
-- y = newVar 2
-- z = newVar 3
-- xx = asLinearCombination x
-- yy = asLinearCombination y
-- c1 = yy .>= constant 1
-- c2 = xx .<= yy
