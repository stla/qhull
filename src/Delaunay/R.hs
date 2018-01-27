module Delaunay.R
  where
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.Maybe
import           Delaunay

-- | R code to plot a 2D Delaunay tesselation
delaunay2ForR :: Tesselation -> Bool -> String
delaunay2ForR tess colors =
  let tiles = IM.elems (_tiles tess) in
  "plot(0, 0, type=\"n\", xlim=c(0,5), ylim=c(0,5)) # please set the limits\n" ++ 
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
delaunay3rgl tess onlyexterior segments colors alpha =
  let ridges = IM.elems (_tilefacets tess) in
  "library(rgl)\n" ++
  (if colors
    then "colors <- topo.colors(" ++ show (length ridges +1) ++ ", alpha=0.5)\n"
    else "\n") ++
  concatMap rglRidge (if onlyexterior
                        then filter (not . sandwichedFacet) ridges
                        else ridges)
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
