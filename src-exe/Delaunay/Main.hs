module Main
  where
import           Delaunay.Examples
import qualified Data.IntMap.Strict  as IM
import           Delaunay
import           Delaunay.R
import           System.IO
import           Text.Show.Pretty

main :: IO ()
main = do

  tess <- delaunay nonConvexPolyhedron False False Nothing
  let code = delaunaySpheres tess
  writeFile "rgl/delaunay_spheres_nonConvexPolyhedron.R" code

  -- x <- randomInSphere 100
  -- tess <- delaunay x False
  -- let code = delaunay3rgl tess False True False Nothing
  -- writeFile "rgl/delaunay_sphere.R" code
