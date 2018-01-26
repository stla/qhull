module ConvexHull.ConvexHull
  where
import           Control.Monad          (unless, when)
import           ConvexHull.CConvexHull
import           ConvexHull.Types
import           Data.Function          (on)
import qualified Data.HashMap.Strict    as H
import qualified Data.IntMap.Strict     as IM
import           Data.List
import           Data.List.Unique       (allUnique)
import           Data.Maybe
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Marshal.Array  (pokeArray)
import           Foreign.Storable       (peek, sizeOf)
import           Qhull.Types

convexHull :: [[Double]]     -- vertices
           -> Bool           -- triangulate
           -> Bool           -- print output to stdout
           -> Maybe FilePath -- write summary to a file
           -> IO ConvexHull
convexHull points triangulate stdout file = do
  let n     = length points
      dim   = length (head points)
      check = all (== dim) (map length (tail points))
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim) $
    error "insufficient number of points"
  unless (allUnique points) $
    error "some points are duplicated"
  pointsPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray pointsPtr (concatMap (map realToFrac) points)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  summaryFile <- maybe (newCString []) newCString file
  resultPtr <- c_convexhull pointsPtr (fromIntegral dim) (fromIntegral n)
               (fromIntegral $ fromEnum triangulate)
               (fromIntegral $ fromEnum stdout) summaryFile exitcodePtr
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free pointsPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      result <- (>>=) (peek resultPtr) cConvexHullToConvexHull
      free resultPtr
      return result


xxx :: ConvexHull -> [[Int]]
xxx chull = map (IM.keys . _rvertices) (IM.elems (_allridges chull))

-- | whether a pair of vertices form an edge of the hull
isEdge :: ConvexHull -> (Index, Index) -> Bool
isEdge hull (i,j) = Pair i j `H.member` _alledges hull

-- | edge as pair of points
toPoints :: ConvexHull -> (Index, Index) -> Maybe ([Double], [Double])
toPoints hull (i,j) = H.lookup (Pair i j) (_alledges hull)

-- | edge as pair of points, without checking the edge exists
toPoints' :: ConvexHull -> (Index, Index) -> ([Double], [Double])
toPoints' hull (i,j) = (H.!) (_alledges hull) (Pair i j)

-- | vertices of a convex hull
hullVertices :: ConvexHull -> [[Double]]
hullVertices hull = map _point (IM.elems (_allvertices hull))

-- | vertices of a facet
facetVertices :: Facet -> [[Double]]
facetVertices = IM.elems . _fvertices

-- | facets ids an edge belongs to
edgeOf :: ConvexHull -> (Index, Index) -> [Int]
edgeOf hull (v1,v2) = IM.keys (IM.filter (elem (Pair v1 v2)) facesEdges)
  where
    facesEdges = IM.map (H.keys . _edges) (_facets hull)
    -- v1v2' = if v1<v2 then Pair v1 v2 else Pair v2 v1 -- useless

-- | group facets of the same family
groupedFacets :: ConvexHull -> [(Family, [IndexMap [Double]], [EdgeMap])]
groupedFacets hull =
  zip3 (map head families) verticesGroups edgesGroups
  where
    facets         = IM.elems (_facets hull)
    facesGroups    = groupBy (\f1 f2 -> sameFamily
                                        (_family f1) (_family f2)) facets
    edgesGroups    = map (map _edges) facesGroups
    verticesGroups = map (map _fvertices) facesGroups
    families       = map (map _family) facesGroups

-- | group facets of the same family and merge vertices and edges
groupedFacets' :: ConvexHull -> [(Family, IndexMap [Double], EdgeMap)]
groupedFacets' hull =
  zip3 (map head families) (map (foldr IM.union IM.empty) verticesGroups)
       (map (foldr delta H.empty) edgesGroups)
  where
    facets         = IM.elems (_facets hull)
    facesGroups    = groupBy (sameFamily `on` _family) facets
    edgesGroups    = map (map _edges) facesGroups
    verticesGroups = map (map _fvertices) facesGroups
    families       = map (map _family) facesGroups
    delta :: EdgeMap -> EdgeMap -> EdgeMap
    delta e1 e2 = H.difference (H.union e1 e2) (H.intersection e1 e2)
