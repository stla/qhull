module Voronoi.R
  where
import           ConvexHull
import qualified Data.IntMap.Strict as IM
import           Data.List          (intercalate, transpose)
import           Data.List.Index    (imap, iconcatMap)
import           Data.Maybe
import           Delaunay.R
import           Delaunay.Types     (Tesselation)
import           Voronoi2D
import           Voronoi3D

voronoi2ForR :: Voronoi2 -> Maybe Tesselation -> String
voronoi2ForR v d =
  (if isJust d then dcode else "")
  ++ "colors <- rainbow(" ++ show (length boundedCells +1) ++ ")\n"
  ++ unlines (imap boundedCellForR boundedCells)
  ++ unlines (map cellForR v)
  where
    dcode = delaunay2ForR (fromJust d) True
    cellForR :: ([Double], Cell2) -> String
    cellForR (site, edges) =
      point ++ "\n" ++ unlines (map f edges)
      where
        point =
          "points(" ++ show (site!!0) ++ ", " ++ show (site!!1) ++
                    ", pch=19, col=\"blue\")"
        f :: Edge2 -> String
        f edge = case edge of
          Edge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++
                        ", col=\"black\", lty=1, lwd=1)"
          IEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x0+x1,y0+y1]) ++
                        ", col=\"red\", lty=1, lwd=1)"
          TIEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++
                        ", col=\"red\", lty=1, lwd=1)"
    boundedCells = map snd (restrictVoronoi2 v)
    boundedCellForR :: Int -> Cell2 -> String
    boundedCellForR i cell =
      "polygon(c(" ++ intercalate "," (map show x) ++ "), c(" ++
      intercalate "," (map show y) ++ "), col=colors[" ++ show (i+1) ++ "])\n"
      where
        [x, y] = transpose (cell2Vertices' cell)


voronoi3ForRgl :: Voronoi3 -> Maybe Tesselation -> String
voronoi3ForRgl v d =
  let code = unlines $ map cellForRgl v in
  "library(rgl)\n" ++
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

-- | plot with facets
voronoi3ForRgl' :: Voronoi3 -> Maybe Tesselation -> IO String
voronoi3ForRgl' v d = do
  let code1 = voronoi3ForRgl v d
      boundedCells = map (cell3Vertices . snd) (restrictVoronoi3 v)
  hulls <- mapM (\cell -> convexHull cell True False Nothing) boundedCells
  let triangles = map (map facetVertices . IM.elems . _hfacets) hulls
      code_colors = "colors <- rainbow(" ++ show (length triangles +1) ++ ")\n"
      code2 = iconcatMap (\i x -> concatMap (rglTriangle i) x ++ "\n") triangles
  return $ code_colors ++ code1 ++ code2
  where
    asTriplet p = (p!!0, p!!1, p!!2)
    rglTriangle :: Int -> [[Double]] -> String
    rglTriangle i threepoints =
      "triangles3d(rbind(c" ++ show p1 ++ ", c" ++ show p2 ++
      ", c" ++ show p3 ++ "), color=colors[" ++ show (i+1) ++ "]" ++
      ", alpha=0.75)\n"
      where
        p1 = asTriplet $ threepoints!!0
        p2 = asTriplet $ threepoints!!1
        p3 = asTriplet $ threepoints!!2
