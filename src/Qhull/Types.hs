module Qhull.Types
  where
import           Data.IntMap.Strict  (IntMap)
import           Data.IntSet         (IntSet)

type Index = Int
type IndexMap = IntMap
type IndexSet = IntSet

data Family = Family Int | None
     deriving (Show, Read, Eq)

sameFamily :: Family -> Family -> Bool
sameFamily (Family i) (Family j) = i == j
sameFamily _ _ = False

class HasFamily m where
  family :: m -> Family
