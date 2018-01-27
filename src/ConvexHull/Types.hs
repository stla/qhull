module ConvexHull.Types
  where
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import           Data.IntMap.Strict  (IntMap)
import           Data.IntSet         (IntSet)
import           Qhull.Types

data IndexPair = Pair Index Index
  deriving (Show, Read)
instance Eq IndexPair where
    Pair i j == Pair i' j' = (i == i' && j == j') || (i == j' && j == i')

instance Hashable IndexPair where
  hashWithSalt _ (Pair i j) = (i+j)*(i+j+1) + 2 * min i j

type EdgeMap = HashMap IndexPair ([Double],[Double])

data Vertex = Vertex {
    _point         :: [Double]
  , _neighfacets   :: IntSet
  , _neighvertices :: IndexSet
  , _neighridges   :: IntSet
} deriving Show

data Ridge = Ridge {
    _rvertices :: IndexMap [Double]
  , _ridgeOf   :: IntSet
} deriving Show

data Facet = Facet {
    _fvertices :: IndexMap [Double]
  , _ridges    :: IntMap Ridge
  , _centroid  :: [Double]
  , _normal    :: [Double]
  , _offset    :: Double
  , _area      :: Double
  , _neighbors :: IntSet
  , _family    :: Family
  , _edges     :: EdgeMap
} deriving Show

instance HasFamily Facet where
  family = _family

data ConvexHull = ConvexHull {
    _allvertices :: IndexMap Vertex -- ou hvertices, etc ?
  , _facets      :: IntMap Facet
  , _allridges   :: IntMap Ridge
  , _alledges    :: EdgeMap
} deriving Show
