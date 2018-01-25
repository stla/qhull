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

-- | R code to plot a 2D Delaunay tesselation
delaunay2ForR :: Tesselation -> Bool -> String
delaunay2ForR tess colors =
  let tiles = IM.elems (_tiles tess) in
  (if colors
    then "colors <- heat.colors(" ++ show (length tiles + 1) ++ ", alpha=0.5)\n"
    else "\n") ++
  concatMap triangle (zip [1 .. length tiles] tiles)
  where
    triangle :: (Int, Tile) -> String
    triangle (i, tile) =
      let pts = map (\p -> [p!!0,p!!1,p!!2])
                (IM.elems $ _points $ _simplex tile)
      in
      "polygon(c(" ++ show (pts!!0!!0) ++ ", " ++ show (pts!!1!!0) ++
                      ", " ++ show (pts!!2!!0) ++ "), "
                   ++ "c(" ++ show (pts!!0!!1) ++ ", " ++ show (pts!!1!!1) ++
                      ", " ++ show (pts!!2!!1) ++
                   "), border=\"black\", " ++
                   (if colors
                     then "col=colors[" ++ show i ++ "])\n"
                     else "col=\"lightblue\")\n")

-- | R code to plot a 3D Delaunay tesselation
delaunay3rgl :: Tesselation -> Bool -> Bool -> Bool -> Maybe Double -> String
delaunay3rgl tess onlyinterior segments colors alpha =
  let ridges = IM.elems (_tilefacets tess) in
  (if colors
    then "colors <- topo.colors(" ++ show (length ridges +1) ++ ", alpha=0.5)\n"
    else "\n") ++
  concatMap rglRidge (if onlyinterior
                        then ridges
                        else filter (not . sandwichedFacet) ridges)
  where
    rglRidge :: TileFacet -> String
    rglRidge ridge =
      let i = 1 + head (IS.elems $ _facetOf ridge) in
      "triangles3d(rbind(c" ++ show (pts!!0) ++
      ", c" ++ show (pts!!1) ++
      ", c" ++ show (pts!!2) ++
      (if colors
        then
          "), color=colors[" ++ show i ++ "]"
        else
          "), color=\"blue\"") ++
      (if isJust alpha
        then ", alpha=" ++ show (fromJust alpha) ++ ")\n"
        else ")\n")
      ++
        -- else "") ++
      if segments
        then
          "segments3d(rbind(c" ++ show (pts!!0) ++
          ", c" ++ show (pts!!1) ++
          "), color=\"black\")\n" ++
          "segments3d(rbind(c" ++ show (pts!!1) ++
          ", c" ++ show (pts!!2) ++
          "), color=\"black\")\n" ++
          "segments3d(rbind(c" ++ show (pts!!2) ++
          ", c" ++ show (pts!!0) ++
          "), color=\"black\")\n"
        else "\n"
      where
        pts = map (\p -> (p!!0,p!!1,p!!2))
                  (IM.elems $ _points $ _subsimplex ridge)
