module Delaunay.Types
  where
import           Data.IntMap.Strict (IntMap)
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
  , _volume       :: Double
} deriving Show

instance HasVertices Simplex where
  _vertices = _vertices'

data TileFacet = TileFacet {
    _subsimplex :: Simplex
  , _facetOf    :: IntSet
  , _normal'    :: [Double]
  , _offset'    :: Double
} deriving Show

instance HasNormal TileFacet where
  _normal = _normal'
  _offset = _offset'

data Tile = Tile {
    _simplex      :: Simplex
  , _neighborsIds :: IntSet
  , _facetsIds    :: IntSet
  , _family'      :: Family
  , _toporiented  :: Bool
} deriving Show

instance HasFamily Tile where
  _family = _family'

data Tesselation = Tesselation {
    _sites      :: IndexMap Site
  , _tiles      :: IntMap Tile
  , _tilefacets :: IntMap TileFacet
  , _edges'     :: EdgeMap
} deriving Show

instance HasEdges Tesselation where
  _edges = _edges'
