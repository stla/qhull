module HalfSpaces.Internal
  (normalizeConstraints, varsOfConstraint)
  where
import           Data.IntMap.Strict           (IntMap, mergeWithKey)
import qualified Data.IntMap.Strict           as IM
import           Data.List                    (nub, union)
import           Data.Ratio
import           HalfSpaces.Constraint        (Constraint (..), Sense (..))
import           HalfSpaces.LinearCombination (LinearCombination (..), VarIndex)

normalizeLinearCombination :: [VarIndex] -> LinearCombination -> IntMap Rational
normalizeLinearCombination vars (LinearCombination lc) =
  IM.union lc (IM.fromList [(i,0) | i <- vars `union` [0]])

varsOfLinearCombo :: LinearCombination -> [VarIndex]
varsOfLinearCombo (LinearCombination imap) = IM.keys imap

varsOfConstraint :: Constraint -> [VarIndex]
varsOfConstraint (Constraint lhs _ rhs) =
  varsOfLinearCombo lhs `union` varsOfLinearCombo rhs

normalizeConstraint :: [VarIndex] -> Constraint -> [Double]
normalizeConstraint vars (Constraint lhs sense rhs) =
  if sense == Lt
    then xs ++ [x]
    else map negate xs ++ [-x]
  where
    lhs' = normalizeLinearCombination vars lhs
    rhs' = normalizeLinearCombination vars rhs
    coefs = IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
    denominators = map denominator coefs
    ppcm = foldr lcm 1 denominators % 1
    x:xs = map (realToFrac . numerator . (*ppcm)) coefs
  -- let (x:xs) = map realToFrac $
  --              IM.elems $ mergeWithKey (\_ a b -> Just (a-b)) id id lhs' rhs'
  -- in
  -- if sense == Lt
  --   then xs ++ [x]
  --   else map negate xs ++ [-x]
  -- where lhs' = normalizeLinearCombination vars lhs
  --       rhs' = normalizeLinearCombination vars rhs

normalizeConstraints :: [Constraint] -> [[Double]] -- for qhalf
normalizeConstraints constraints = map (normalizeConstraint vars) constraints
  where
    vars = nub $ concatMap varsOfConstraint constraints
