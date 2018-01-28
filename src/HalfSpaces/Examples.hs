module HalfSpaces.Examples
  where
import           Data.VectorSpace
import           HalfSpaces.Constraint        (Constraint (..), (.<=), (.<=.),
                                               (.>=), (.>=.))
import           HalfSpaces.LinearCombination (constant, linearCombination,
                                               newVar)

testSmall :: [Constraint]
testSmall = [ x .<= 1, x .>= 0, y .<= 1]
  where
    x = newVar 1
    y = newVar 2

rggConstraints :: [Constraint]
rggConstraints =
  [ x .>=. ((-5)*^one)
  , x .<=. (4*^one)
  , y .>=. ((-5)*^one)
  , y .<=. (3*^one ^-^ x)
  , z .>=. ((-10)*^one)
  , z .<=. (6*^one ^-^ x ^-^ y)]
  where
    x = newVar 1
    y = newVar 2
    z = newVar 3
    one = constant 1

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
