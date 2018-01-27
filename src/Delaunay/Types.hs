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
    _points       :: IndexMap [Double]
  , _circumcenter :: [Double]
  , _circumradius :: Double
  -- , _normal       :: [Double]
  -- , _offset       :: Double
  , _volume       :: Double
} deriving Show

data TileFacet = TileFacet {
    _subsimplex :: Simplex
  , _facetOf    :: IntSet
  , _normal     :: [Double]
  , _offset     :: Double
} deriving Show

data Tile = Tile {
    _simplex      :: Simplex
  , _neighborsIds :: IntSet
  , _facetsIds    :: IntSet
  , _family       :: Family
  , _toporiented  :: Bool
} deriving Show

instance HasFamily Tile where
  family = _family

data Tesselation = Tesselation {
    _sites      :: IndexMap Site
  , _tiles      :: IntMap Tile
  , _tilefacets :: IntMap TileFacet
} deriving Show
