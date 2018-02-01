module Delaunay.R
  where
-- import qualified Data.HashMap.Strict.InsOrd as H
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import           Data.List
import           Data.List.Index            (iconcatMap)
-- import           Data.Maybe
import           Delaunay
import           Text.Printf

-- | R code to plot a 2D Delaunay tesselation
delaunay2ForR :: Tesselation -> Bool -> String
delaunay2ForR tess colors =
  let tiles = IM.elems (_tiles tess) in
  "plot(0, 0, type=\"n\", xlim=c(0,5), ylim=c(0,5)) # please set the limits\n" ++
  (if colors
    then printf "colors <- heat.colors(%d, alpha=0.5)\n" (length tiles + 1)
    else "\n") ++
  concatMap triangle (zip [1 .. length tiles] tiles)
  where
    triangle :: (Int, Tile) -> String
    triangle (i, tile) =
      let pts = map (\p -> [p!!0,p!!1,p!!2]) (verticesCoordinates tile)
      in
      printf "polygon(c(%f,%f,%f), c(%f,%f,%f), border=\"black\", "
             (pts!!0!!0) (pts!!1!!0) (pts!!2!!0)
             (pts!!0!!1) (pts!!1!!1) (pts!!2!!1) ++
      -- "polygon(c(" ++ show (pts!!0!!0) ++ ", " ++ show (pts!!1!!0) ++
      --                 ", " ++ show (pts!!2!!0) ++ "), "
      --              ++ "c(" ++ show (pts!!0!!1) ++ ", " ++ show (pts!!1!!1) ++
      --                 ", " ++ show (pts!!2!!1) ++
      --              "), border=\"black\", " ++
      (if colors
        then printf "col=colors[%d])\n" i
        else "col=\"lightblue\")\n")

-- | R code to plot a 3D Delaunay tesselation
-- TODO: exterior interior
delaunay3rgl :: Tesselation -> Bool -> Bool -> Bool -> Maybe Double -> String
delaunay3rgl tess onlyexterior segments colors alpha =
  let allridges = IM.elems (_tilefacets tess) in
  let ridges = (if onlyexterior
                  then filter (not . sandwichedFacet) allridges
                  else allridges) in
  "library(rgl)\n" ++
  (if colors
    then printf "colors <- topo.colors(%d, alpha=0.5)\n" (length ridges + 1)
    else "\n") ++
  concatMap rglRidge ridges ++
  (if segments
    then if onlyexterior
      then concatMap rglSegment (tilefacetsEdges ridges)
      else concatMap rglSegment (edgesCoordinates tess)
    else "")
  where
    rglRidge :: TileFacet -> String
    rglRidge ridge =
      let i = 1 + head (IS.elems $ _facetOf ridge) in
      printf "triangles3d(rbind(c%s,c%s,c%s"
             (show (pts!!0)) (show (pts!!1)) (show (pts!!2)) ++
      -- "triangles3d(rbind(c" ++ show (pts!!0) ++
      -- ", c" ++ show (pts!!1) ++
      -- ", c" ++ show (pts!!2) ++
      (if colors
        then
          printf "), color=colors[%d]" i
        else
          "), color=\"blue\"") ++
      maybe ")\n" (printf "alpha=%f)\n") alpha
      -- (if isJust alpha
      --   then printf "alpha=%f)\n" (fromJust alpha)
      --   else ")\n")
      where
        pts = map (\p -> (p!!0,p!!1,p!!2)) (verticesCoordinates ridge)
    rglSegment :: ([Double], [Double]) -> String
    rglSegment (p1, p2) =
      printf "segments3d(rbind(c%s,c%s), color=\"black\")\n"
             (show p1') (show p2')
      -- "segments3d(rbind(c" ++ show p1' ++ ", c" ++ show p2' ++
      -- "), color=\"black\")\n"
      where
        p1' = (p1!!0, p1!!1, p1!!2)
        p2' = (p2!!0, p2!!1, p2!!2)
    tilefacetsEdges :: [TileFacet] -> [([Double], [Double])]
    tilefacetsEdges tilefacets = foldl' union [] (map tilefacetEdges tilefacets)
      where
        tilefacetEdges :: TileFacet -> [([Double], [Double])]
        tilefacetEdges tilefacet = [(v!!0,v!!1),(v!!1,v!!2),(v!!2,v!!0)]
          where
            v = verticesCoordinates tilefacet

delaunaySpheres :: Tesselation -> String
delaunaySpheres tess =
  let tiles = IM.elems (_tiles tess) in
  "library(rgl)\n" ++
  printf "colors <- rainbow(%d)\n" (length tiles) ++
  iconcatMap rglSphere tiles
  where
    rglSphere :: Int -> Tile -> String
    rglSphere i tile =
      printf "spheres3d(%f, %f, %f, radius=%f, color=colors[%d], alpha=0.75)\n"
             (c!!0) (c!!1) (c!!2) r (i+1)
      where
        c = _center tile
        r = _circumradius (_simplex tile)
