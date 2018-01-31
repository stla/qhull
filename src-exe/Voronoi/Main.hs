module Main
  where
import           ConvexHull
import           ConvexHull.R
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.List
import           Delaunay
import           Delaunay.Examples
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

-- distance :: [Double] -> [Double] -> Double
-- distance p1 p2 = sum (zipWith (\x y -> (subtract x y)^2) p1 p2)
--
-- checkTile :: Tile -> [Double]
-- checkTile tile = map (distance center) vertices
--   where
--     simplex = _simplex tile
--     center = _circumcenter simplex
--     vertices = IM.elems (_points simplex)


main :: IO ()
main = do

  tess <- delaunay projectedTruncatedTesseract False True
  pPrint $ IM.filter (\tile -> _volume tile < 1e-16 && _volume tile > 0) (_tiles tess)
  let v = voronoi3 tess
  -- pPrint $ map (\(_,cell) -> length cell) (restrictVoronoi3 v)
  prettyShowVoronoi3 (roundVoronoi3 10 (restrictVoronoi3 v)) Nothing
  code <- voronoi3ForRgl' v (Just 10) Nothing
  writeFile "rgl/voronoi_truncatedTesseract01.R" code

--   let x1 = let b=0 in
--             [[sin (a*2*pi/100) * cos b, sin (a*2*pi/100) * sin b, cos (a*2*pi/100)] | a <- [0 .. 99]]
--       x2 = let b=pi/2 in
--             [[sin (a*2*pi/100) * cos b, sin (a*2*pi/100) * sin b, cos (a*2*pi/100)] | a <- [0 .. 99]]
--       x3 = [[cos (a*2*pi/100), sin (a*2*pi/100), 0] | a <- [0 .. 99]]
--   tess <- delaunay (nub $ x1 ++ x2 ++ x3 ++ map (map (/5)) cube3) False True
-- --  let code = delaunay3rgl tess False True True (Just 0.5)
--   pPrint $ IM.filter (isNaN . head) (IM.map (_circumcenter . _simplex) (_tiles tess))
--   -- pPrint $ IM.filterWithKey (\k _ -> k `elem` [459, 460, 464]) (_tiles tess)
--   -- pPrint $ IM.map checkTile (IM.filter (\tile -> _volume (_simplex tile) == 0) (_tiles tess))
--   let v = voronoi3 tess
--       v' = filter (\(_,cell) -> not (null cell)) v
-- --      v' = restrictVoronoi3box' ((-2,2),(-2,2),(-2,2)) v
--       v'' = clipVoronoi3 ((-1,1),(-1,1),(-1,1)) v'
--   code <- voronoi3ForRgl' v'' Nothing
--   writeFile "rgl/voronoi_twoCircles04.R" code
--   putStrLn "done"

--   let x1 = let b=0 in
--             [[sin (a*2*pi/100) * cos b, sin (a*2*pi/100) * sin b, cos (a*2*pi/100)] | a <- [0 .. 99]]
--       x2 = let b=pi/2 in
--             [[sin (a*2*pi/100) * cos b, sin (a*2*pi/100) * sin b, cos (a*2*pi/100)] | a <- [0 .. 99]]
--   tess <- delaunay (nub $ x1 ++ x2 ++ map (map (/5)) cube3) False True
--   let v = voronoi3 tess
--       vv = [last v]
--       v' = filter (\(_,cell) -> not (null cell)) v
--       v'' = clipVoronoi3 (-1,1,-1,1,-1,1) v'
-- --  prettyShowVoronoi3 v' (Just 10)
--   code <- voronoi3ForRgl' v'' Nothing
--   writeFile "rgl/voronoi_twoCircles02.R" code
--   putStrLn "done"

  -- x1 <- randomOnSphere 500 4
  -- x2 <- randomOnSphere 500 2
  -- let points = x1 ++ x2 ++ [[1,0,0],[-1,0,0]]
  -- tess <- delaunay points False False
  -- let v = voronoi3 tess
  --     vv = [last v, last (init v)]
  -- code <- voronoi3ForRgl' vv Nothing
  -- writeFile "rgl/voronoi_sphere01.R" code

--   let x1 = map (map (*2)) cube3
--   let x2 = map (map (*(1/2))) cube3
--   tess <- delaunay (x1 ++ x2) False True
--   pPrint $ _tiles tess
--   let ridgeof = filter (\fo -> IS.size fo == 1) (map _facetOf (IM.elems (_tilefacets tess)))
--   print $ length ridgeof
--   print $ IM.size (_tilefacets tess)
--   let ridgevertices = map (IM.keys . _points . _subsimplex) (IM.elems (_tilefacets tess))
--   print $ length $ nub ridgevertices
--   let tilevertices = map (IM.keys . _points . _simplex) (IM.elems (_tiles tess))
--   pPrint $ length $ filter (== [3]) $ map (\verts -> filter (==3) $ map (\tv -> length (intersect tv verts)) tilevertices) ridgevertices
--   let v = voronoi3 tess
--   code <- voronoi3ForRgl' v Nothing
-- --  let code = delaunay3rgl tess False True True (Just 0.5)
--   writeFile "rgl/voronoi_projhcube4.R" code

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
