module Delaunay.Delaunay
  where
import           Control.Monad         (unless, when)
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS
import           Data.List.Unique      (allUnique)
import           Delaunay.CDelaunay
import           Delaunay.Types
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           Qhull.Types

delaunay :: [[Double]] -- sites (vertices)
         -> Bool       -- add a point at infinity
         -> Bool       -- include degenerate tiles
         -> IO Tesselation
delaunay sites atinfinity degenerate = do
  let n     = length sites
      dim   = length (head sites)
      check = all (== dim) (map length (tail sites))
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim+1) $
    error "insufficient number of points"
  unless check $
    error "the points must have the same dimension"
  unless (allUnique sites) $
    error "some points are duplicated"
  sitesPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray sitesPtr (concatMap (map realToFrac) sites)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_tesselation sitesPtr
               (fromIntegral dim) (fromIntegral n)
               (fromIntegral $ fromEnum atinfinity)
               (fromIntegral $ fromEnum degenerate)
               exitcodePtr
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free sitesPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      result <- peek resultPtr
      out <- cTesselationToTesselation sites result
      free resultPtr
      return out

-- | vertices ids of a tile facet
facetVertices :: TileFacet -> IndexSet
facetVertices = IS.fromAscList . IM.keys . _points . _subsimplex

-- | tile facets a vertex belongs to, vertex given by its index;
-- the output is the empty map if the index is not valid
vertexNeighborFacets :: Tesselation -> Index -> IntMap TileFacet
vertexNeighborFacets tess i = IM.restrictKeys (_tilefacets tess) ids
  where
    ids = maybe IS.empty _neighfacetsIds (IM.lookup i (_sites tess))

-- | whether a tile facet is sandwiched between two tiles
sandwichedFacet :: TileFacet -> Bool
sandwichedFacet tilefacet = IS.size (_facetOf tilefacet) == 2

-- | the tiles a facet belongs to
facetOf :: Tesselation -> TileFacet -> IntMap Tile
facetOf tess tilefacet = IM.restrictKeys (_tiles tess) (_facetOf tilefacet)

-- | the families of the tiles a facet belongs to
facetFamilies :: Tesselation -> TileFacet -> IntMap Family
facetFamilies tess tilefacet = IM.map _family (facetOf tess tilefacet)

-- | the circumcenters of the tiles a facet belongs to
facetCenters :: Tesselation -> TileFacet -> IntMap [Double]
facetCenters tess tilefacet =
  IM.map (_circumcenter . _simplex) (facetOf tess tilefacet)

funofFacetToFunofInt :: (Tesselation -> TileFacet -> IntMap a)
                     -> (Tesselation -> Int -> IntMap a)
funofFacetToFunofInt f tess i =
  maybe IM.empty (f tess) (IM.lookup i (_tilefacets tess))

-- | the tiles a facet belongs to, facet given by its id
facetOf' :: Tesselation -> Int -> IntMap Tile
facetOf' = funofFacetToFunofInt facetOf

-- | the families of the tiles a facet belongs to, facet given by its id
facetFamilies' :: Tesselation -> Int -> IntMap Family
facetFamilies' = funofFacetToFunofInt facetFamilies

-- | the circumcenters of the tiles a facet belongs to, facet given by its id
facetCenters' :: Tesselation -> Int -> IntMap [Double]
facetCenters' = funofFacetToFunofInt facetCenters
