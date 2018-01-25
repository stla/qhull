module Delaunay.Delaunay
  where
import           Control.Monad         (unless, when)
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS
import           Data.List.Unique      (allUnique)
import           Data.Maybe
import           Delaunay.CDelaunay
import           Delaunay.Types
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)

delaunay :: [[Double]] -> Bool -> IO Tesselation
delaunay sites deg = do
  let n     = length sites
      dim   = length (head sites)
      check = all (== dim) (map length (tail sites))
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim+1) $
    error "insufficient number of points"
  unless (allUnique sites) $
    error "some points are duplicated"
  sitesPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray sitesPtr (concatMap (map realToFrac) sites)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_tesselation sitesPtr
               (fromIntegral dim) (fromIntegral n) (fromIntegral $ fromEnum deg)
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

-- | the tile facets a vertex belongs to
vertexNeighborFacets :: Tesselation -> Index -> [TileFacet]
vertexNeighborFacets tess i =
  IM.elems (IM.restrictKeys tilefacets ids)
  where
    ids = _neighfacetsIds (_sites tess IM.! i)
    tilefacets = _tilefacets tess

-- | whether a tile facet is sandwiched between two tiles
sandwichedFacet :: TileFacet -> Bool
sandwichedFacet tilefacet = IS.size (_facetOf tilefacet) == 2
