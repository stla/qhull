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

  -- tess <- delaunay nonConvexPolyhedron False False Nothing
  -- let code = delaunaySpheres tess
  -- writeFile "rgl/delaunay_spheres_nonConvexPolyhedron.R" code

  x <- randomOnSphere 100 3
  tess <- delaunay ([0,0,0] : x) False False Nothing
  let code = delaunay3rgl tess True False True True Nothing
  writeFile "rgl/delaunay_sphere_interior.R" code
