module Main
  where
import           ConvexHull
import           Delaunay.Examples
import           ConvexHull.R
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet        as IS
import Data.List
import           Delaunay
import           Delaunay.R
import           System.IO
import           Text.Show.Pretty
import           Voronoi.R
import           Voronoi2D
import           Voronoi3D
-- import Data.Graph
-- import Data.List.Index
-- import Data.List

-- connectedEdges :: Edge2 -> Edge2 -> Bool
-- connectedEdges (Edge2 (x1,x2)) (Edge2 (y1,y2)) = length ([x1,x2] `intersect` [y1,y2]) == 1
-- connectedEdges _ _ = False
--
-- edgeVertices :: Edge2 -> [[Double]]
-- edgeVertices (Edge2 ((x1,x2),(y1,y2))) = [[x1,x2],[y1,y2]]
--
-- connectedVertices :: Cell2 -> [Double] -> [Double] -> Bool
-- connectedVertices cell [x1,x2] [y1,y2] =
--   (Edge2 ((x1,x2),(y1,y2)) `elem` cell) || (Edge2 ((y1,y2),(x1,x2)) `elem` cell)

main :: IO ()
main = do

  let x1 = map (map (*2)) cube3
  let x2 = map (map (*(1/2))) cube3
  tess <- delaunay (x1 ++ x2) False True
  pPrint $ _tiles tess
  let ridgeof = filter (\fo -> IS.size fo == 1) (map _facetOf (IM.elems (_tilefacets tess)))
  print $ length ridgeof
  print $ IM.size (_tilefacets tess)
  let ridgevertices = map (IM.keys . _points . _subsimplex) (IM.elems (_tilefacets tess))
  print $ length $ nub ridgevertices
  let tilevertices = map (IM.keys . _points . _simplex) (IM.elems (_tiles tess))
  pPrint $ length $ filter (== [3]) $ map (\verts -> filter (==3) $ map (\tv -> length (intersect tv verts)) tilevertices) ridgevertices
  let v = voronoi3 tess
  code <- voronoi3ForRgl' v Nothing
--  let code = delaunay3rgl tess False True True (Just 0.5)
  writeFile "rgl/voronoi_projhcube4.R" code

  -- x1 <- randomOnSphere 500 1
  -- x2 <- randomOnSphere 500 0.5
  -- let points = x1 ++ x2 ++ [[0,0,0]]
  -- tess <- delaunay points False False
  -- let v = voronoi3 tess
  --     vv = [last v]
  -- code <- voronoi3ForRgl' vv Nothing
  -- writeFile "rgl/voronoi_sphere.R" code

  -- let x = duplicate3 (duplicate3 (duplicate3 (cube3 ++ [[0,0,0]]) [2,0,0]) [0,2,0]) [0,0,2]
  -- tess <- delaunay x False False
  -- let v = voronoi3 tess
  -- code <- voronoi3ForRgl' v Nothing
  -- writeFile "rgl/voronoi_multicube.R" code

  -- let x = dodecahedron ++ [[0,0,0]]
  -- tess <- delaunay x False
  -- let v = voronoi3 tess
  -- code <- voronoi3ForRgl' v Nothing
  -- writeFile "rgl/voronoi_centricDodecahedron.R" code
  -- prettyShowVoronoi3 v (Just 3)

  -- let c = 4
  --     a = 1
  -- let curve3D = map (\x -> [ cos (2*pi*x) * (c + a * cos (2*pi*x))
  --                         ,  sin (2*pi*x) * (c + a * cos (2*pi*x))
  --                         ,  a * sin (2*pi*x)]) [i/50 | i <- [0 .. 50]]
  -- tess <- delaunay curve3D False
  -- let v = voronoi3 tess
  -- writeFile "rgl/voronoi_curve3Dtorus.R" (voronoi3ForRgl v (Just tess))

  -- let curve3D = map (\x -> [ sin (pi*x) * cos (2*pi*x)
  --                         ,  sin (pi*x) * sin (2*pi*x)
  --                         ,  cos (pi*x)]) [i/50 | i <- [0 .. 50]]
  -- tess <- delaunay curve3D False
  -- let v = voronoi3 tess
  -- writeFile "rgl/voronoi_curve3D.R" (voronoi3ForRgl v (Just tess))

  -- x <- randomOnTorus 50 4 1
  -- tess <- delaunay (x ++ [[4,0,0],[-4,0,0],[0,4,0],[0,-4,0]]) False
  -- writeFile "rgl/delaunay_torus00.R" (delaunay3rgl tess True True True Nothing)
  -- -- pPrint (_tiles tess)
  -- -- pPrint (_tilefacets tess)
  -- let v = voronoi3 tess
  -- -- pPrint v
  -- -- pPrint $ zip [0 .. 50] (map (map (facetCenters tess) . vertexNeighborFacets tess) [0 .. 50])
  -- code <- voronoi3ForRgl' v Nothing
  -- writeFile "rgl/voronoi_torus00.R" code

  -- x <- randomInSquare 100
  -- tess <- delaunay (x ++ [[-1,-1],[-1,2],[2,-1],[2,2]]) False
  -- let v = voronoi2 tess
  -- writeFile "Rplots/voronoi_randomInSquare00.R" (voronoi2ForR v Nothing)

  -- let cube = [[i,j,k] | i <- [-1,2], j <- [-1,2], k <- [-1,2]]
  -- x <- randomInCube 30
  -- tess <- delaunay (cube ++ x) False
  -- let v = voronoi3 tess
  -- prettyShowVoronoi3 v (Just 3)
  -- pPrint (map (_circumcenter . _simplex) (IM.elems (_tiles tess)))
  -- code <- voronoi3ForRgl' v Nothing
  -- writeFile "rgl/voronoi_randomInCube00.R" code

  -- let cube = [[i,j,k] | i <- [0,1], j <- [0,1], k <- [0,1]]
  -- x <- randomInCube 30
  -- tess <- delaunay (cube ++ x) False
  -- let v = voronoi3 tess
  --     v' = restrictVoronoi3box (-5,5) (-5,5) (-5,5) v
  -- prettyShowVoronoi3 v' (Just 3)
  -- code <- voronoi3ForRgl' v' Nothing
  -- writeFile "rgl/voronoi_randomInCube01.R" code

  -- x <- randomInCircle 50
  -- tess <- delaunay (x ++ [[0,0]]) False
  -- let v = voronoi2 tess
  -- writeFile "Rplots/voronoi_circle01.R" (voronoi2ForR v Nothing)

  -- tess <- delaunay squareLattice False
  -- let v = voronoi2 tess
  -- putStrLn $ voronoi2ForR v (Just tess)

  -- let x = rhombicDodecahedron ++ [[0,0,0]]
  -- tess <- delaunay x False
  -- let v = voronoi3 tess
  --     code1 = voronoi3ForRgl v Nothing
  --     (_, cell) = last v
  -- code2 <- convexHull3DrglCode (cell3Vertices cell) False Nothing
  -- writeFile "rgl/voronoi_centricRhombicDodecahedron.R" (code1 ++ code2)
  -- pPrint v

  -- tess <- delaunay centricCuboctahedron False
  -- let v = voronoi3 tess
  --     code1 = voronoi3ForRgl v Nothing
  --     (_, cell) = last v
  -- code2 <- convexHull3DrglCode (cell3Vertices cell) False Nothing
  -- writeFile "rgl/voronoi_centricCuboctahedron.R" (code1 ++ code2)
  -- pPrint v

  -- x <- randomInSphere 1000
  -- tess <- delaunay2 x False
  -- print $ IM.size (_tiles tess)
