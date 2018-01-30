module HalfSpaces.Examples
  where
import           Data.Ratio                   ((%))
import           Data.VectorSpace
import           HalfSpaces.Constraint        (Constraint (..), (.<=), (.<=.),
                                               (.>=), (.>=.))
import           HalfSpaces.LinearCombination (constant, cst, linearCombination,
                                               newVar)

testSmall :: [Constraint]
testSmall = [ x .<= 1, x .>= 0, y .<= 1]
  where
    x = newVar 1
    y = newVar 2

rggConstraints :: [Constraint]
rggConstraints =
  [ x .>= (-5)
  , x .<=  4
  , y .>= (-5)
  , y .<=. cst 3 ^-^ x
  , z .>= (-10)
  , z .<=. cst 6 ^-^ x ^-^ y ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3

region3D :: [Constraint]
region3D =
  [ x .>=  0 -- shortcut for x .>=. cst 0
  , x .<=  3
  , y .>=  0
  , y .<=. cst 2 ^-^ (2%3)*^x
  , z .>=  0
  , z .<=. cst 6 ^-^ 2*^x ^-^ 3*^y ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3

cubeConstraints :: [Constraint]
cubeConstraints =
  [ x .<= 1
  , x .>= (-1)
  , y .<= 1
  , y .>= (-1)
  , z .<= 1
  , z .>= (-1) ]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3
