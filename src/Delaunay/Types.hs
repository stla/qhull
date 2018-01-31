module Delaunay.Types
  where
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.IntSet        (IntSet)
import           Qhull.Types

data Site = Site {
    _point          :: [Double]
  , _neighsitesIds  :: IndexSet
  , _neighfacetsIds :: IntSet
  , _neightilesIds  :: IntSet
} deriving Show

data Simplex = Simplex {
    _vertices'    :: IndexMap [Double]
  , _circumcenter :: [Double]
  , _circumradius :: Double
  , _volume'      :: Double
} deriving Show

instance HasCenter Simplex where
  _center = _circumcenter

instance HasVertices Simplex where
  _vertices = _vertices'

instance HasVolume Simplex where
  _volume = _volume'

data TileFacet = TileFacet {
    _subsimplex :: Simplex
  , _facetOf    :: IntSet
  , _normal'    :: [Double]
  , _offset'    :: Double
} deriving Show

instance HasNormal TileFacet where
  _normal = _normal'
  _offset = _offset'

instance HasVertices TileFacet where
  _vertices = _vertices' . _subsimplex

instance HasVolume TileFacet where
  _volume = _volume' . _subsimplex

instance HasCenter TileFacet where
  _center = _circumcenter . _subsimplex

data Tile = Tile {
    _simplex      :: Simplex
  , _neighborsIds :: IntSet
  , _facetsIds    :: IntSet
  , _family'      :: Family
  , _toporiented  :: Bool
} deriving Show

instance HasFamily Tile where
  _family = _family'

instance HasVertices Tile where
  _vertices = _vertices' . _simplex

instance HasVolume Tile where
  _volume = _volume' . _simplex

instance HasCenter Tile where
  _center = _circumcenter . _simplex

data Tesselation = Tesselation {
    _sites      :: IndexMap Site
  , _tiles      :: IntMap Tile
  , _tilefacets :: IntMap TileFacet
  , _edges'     :: EdgeMap
} deriving Show

instance HasEdges Tesselation where
  _edges = _edges'

instance HasVertices Tesselation where
  _vertices tess = IM.map _point (_sites tess)

instance HasVolume Tesselation where
  _volume tess = sum (IM.elems $ IM.map (_volume' . _simplex) (_tiles tess))
