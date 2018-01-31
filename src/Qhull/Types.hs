module Qhull.Types
  where
import           Data.Hashable
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import           Data.IntMap.Strict         (IntMap)
import           Data.IntSet                (IntSet)

type Index = Int
type IndexMap = IntMap
type IndexSet = IntSet

data IndexPair = Pair Index Index
  deriving (Show, Read)
instance Eq IndexPair where
    Pair i j == Pair i' j' = (i == i' && j == j') || (i == j' && j == i')

instance Hashable IndexPair where
  hashWithSalt _ (Pair i j) = (i+j)*(i+j+1) + 2 * min i j

type EdgeMap = InsOrdHashMap IndexPair ([Double],[Double])

data Family = Family Int | None
     deriving (Show, Read, Eq)

class HasFamily m where
  _family :: m -> Family

class HasNormal m where
  _normal :: m -> [Double]
  _offset :: m -> Double

class HasVertices m where
  _vertices :: m -> IndexMap [Double]

class HasEdges m where
  _edges :: m -> EdgeMap

class HasVolume m where
  _volume :: m -> Double

class HasCenter m where
  _center :: m -> [Double]
