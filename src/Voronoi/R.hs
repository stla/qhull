module Voronoi.R
  where
import Data.List (intercalate)
import           Data.Maybe
import           Delaunay.R
import           Delaunay.Types (Tesselation)
import           Voronoi2D      (Cell2, Voronoi2, Edge2(..))
import           Voronoi3D      (Cell3, Voronoi3, Edge3(..))

voronoi2ForR :: Voronoi2 -> Maybe Tesselation -> String
voronoi2ForR v d =
  (if isJust d then dcode else "") ++ unlines (map cellForRgl v)
  where
    dcode = delaunay2ForR (fromJust d) True
    cellForRgl :: ([Double], Cell2) -> String
    cellForRgl (site, edges) =
      point ++ "\n" ++ unlines (map f edges)
      where
        point =
          "points(" ++ show (site!!0) ++ ", " ++ show (site!!1) ++
                    ", pch=19, col=\"blue\")"
        f :: Edge2 -> String
        f edge = case edge of
          Edge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++
                        ", col=\"green\", lty=2, lwd=2)"
          IEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x0+x1,y0+y1]) ++
                        ", col=\"red\", lty=2, lwd=2)"
          TIEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++
                        ", col=\"red\", lty=2, lwd=2)"


voronoi3ForRgl :: Voronoi3 -> Maybe Tesselation -> String
voronoi3ForRgl v d =
  let code = unlines $ map cellForRgl v in
  if isJust d
    then code ++ "\n" ++ "# Delaunay:\n" ++
         delaunay3rgl (fromJust d) True True True (Just 0.9)
    else code
  where
    cellForRgl :: ([Double], Cell3) -> String
    cellForRgl (site, cell) = plotpoint ++ unlines (map f cell)
      where
        plotpoint = "spheres3d(" ++ intercalate "," (map show site) ++ ", radius=0.1, color=\"red\")\n"
        f :: Edge3 -> String
        f edge = case edge of
          Edge3 (x,y) ->
            "segments3d(rbind(c" ++ show x ++ ", c" ++ show y ++ "))"
          TIEdge3 (x,y) ->
            "segments3d(rbind(c" ++ show x ++ ", c" ++ show y ++ "), col=c(\"red\",\"red\"))"
          IEdge3 (x,y) ->
            "segments3d(rbind(c" ++ show x ++ ", c" ++ show (sumTriplet x y) ++ "), col=c(\"red\",\"red\"))"
        sumTriplet (a,b,c) (a',b',c') = (a+a',b+b',c+c')
