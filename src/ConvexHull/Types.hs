module ConvexHull.Types
  where
import           Data.IntMap.Strict         (IntMap)
import           Data.IntSet                (IntSet)
import           Qhull.Types

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

instance HasVertices Ridge where
  _vertices = _rvertices

data Facet = Facet {
    _fvertices :: IndexMap [Double]
  , _fridges   :: IntMap Ridge
  , _centroid  :: [Double]
  , _normal'   :: [Double]
  , _offset'   :: Double
  , _area      :: Double
  , _neighbors :: IntSet
  , _family'   :: Family
  , _fedges    :: EdgeMap
} deriving Show

instance HasEdges Facet where
  _edges = _fedges

instance HasVertices Facet where
  _vertices = _fvertices

instance HasNormal Facet where
  _normal = _normal'
  _offset = _offset'

instance HasFamily Facet where
  _family = _family'

data ConvexHull = ConvexHull {
    _hvertices :: IndexMap Vertex
  , _hfacets   :: IntMap Facet
  , _hridges   :: IntMap Ridge
  , _hedges    :: EdgeMap
} deriving Show

instance HasEdges ConvexHull where
  _edges = _hedges
