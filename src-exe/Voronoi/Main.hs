module Main
  where
import           ConvexHull
import           ConvexHull.Examples
import qualified Data.IntMap.Strict  as IM
import           Delaunay
import           System.IO
import           Text.Show.Pretty
import           Voronoi2D
import           Voronoi3D

main :: IO ()
main = do

  tess <- delaunay centricCuboctahedron False
  let v = voronoi3 tess
      code1 = voronoi3ForRgl v Nothing
      (_, cell) = last v
  code2 <- convexHull3DrglCode (cell3Vertices cell) False Nothing
  writeFile "rgl/voronoi_centricCuboctahedron.R" (code1 ++ code2)
  pPrint v

  -- x <- randomInSphere 1000
  -- tess <- delaunay2 x False
  -- print $ IM.size (_tiles tess)
