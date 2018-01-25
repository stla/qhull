module Main
  where
import           ConvexHull
import           ConvexHull.Examples
import           ConvexHull.R
import qualified Data.IntMap.Strict  as IM
import           Delaunay
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

  x <- randomInCircle 50
  tess <- delaunay (x ++ [[0,0]]) False
  let v = voronoi2 tess
  writeFile "Rplots/voronoi_circle01.R" (voronoi2ForR v Nothing)

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
