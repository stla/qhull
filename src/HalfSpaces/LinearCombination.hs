{-# LANGUAGE TypeFamilies #-}
module HalfSpaces.LinearCombination
  where
import           Data.AdditiveGroup
import           Data.IntMap.Strict (IntMap, mergeWithKey)
import qualified Data.IntMap.Strict as IM
import           Data.List
import           Data.Ratio
import           Data.Tuple         (swap)
import           Data.VectorSpace

newtype LinearCombination = LinearCombination (IntMap Rational)
  deriving Eq

instance Show LinearCombination where
  show (LinearCombination x) =
    intercalate " + " $
      map (\(i,r) -> if i==0
                      then showRational r
                      else if r == 1
                            then "x" ++ show i
                            else showRational r ++ "*x" ++ show i)
          (IM.toAscList x)
    where
      showRational :: Rational -> String
      showRational r = if q==1 then show p else show p ++ "/" ++ show q
                       where
                        p = numerator r
                        q = denominator r

instance AdditiveGroup LinearCombination where
  zeroV = LinearCombination (IM.singleton 0 0)
  (^+^) (LinearCombination imap1) (LinearCombination imap2) =
    LinearCombination
    (mergeWithKey (\_ x y -> Just (x+y)) id id imap1 imap2)
  negateV (LinearCombination imap) = LinearCombination (IM.map negate imap)

instance VectorSpace LinearCombination where
  type Scalar LinearCombination = Rational
  (*^) lambda (LinearCombination imap) =
    LinearCombination (IM.map (*lambda) imap)

type Var = LinearCombination
type VarIndex = Int

newVar :: VarIndex -> Var
newVar i = if i >= 0
            then LinearCombination (IM.singleton i 1)
            else error "negative index"

linearCombination :: [(Rational,Var)] -> LinearCombination
linearCombination terms = linearCombo (map swap terms)
--  LinearCombination (IM.fromListWith (+) (map swap terms))

-- asLinearCombination :: Var -> LinearCombination
-- asLinearCombination var = LinearCombination (IM.singleton var 1)

constant :: Rational -> LinearCombination
constant x = LinearCombination (IM.singleton 0 x)

normalizeLinearCombination :: [VarIndex] -> LinearCombination -> IntMap Rational
normalizeLinearCombination vars (LinearCombination lc) =
  IM.union lc (IM.fromList [(i,0) | i <- vars `union` [0]])

varsOfLinearCombo :: LinearCombination -> [VarIndex]
varsOfLinearCombo (LinearCombination imap) = IM.keys imap
