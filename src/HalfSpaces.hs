module HalfSpaces
  (module X)
  where
import           HalfSpaces.Constraint        as X (Constraint (..), (.<=),
                                                    (.<=.), (.>=), (.>=.))
import           HalfSpaces.Examples          as X
import           HalfSpaces.HalfSpaces        as X
import           HalfSpaces.LinearCombination as X (LinearCombination (..),
                                                    constant, linearCombination,
                                                    newVar)
