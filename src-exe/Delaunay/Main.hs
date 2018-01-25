module Main
  where
import           ConvexHull.Examples
import qualified Data.IntMap.Strict  as IM
import           Delaunay
import           Delaunay.R
import           System.IO
import           Text.Show.Pretty

main :: IO ()
main = do

  -- tess <- delaunay rgg False
  -- pPrint tess

  x <- randomInSphere 100
  tess <- delaunay x False
  let code = delaunay3rgl tess False True False Nothing
  writeFile "rgl/delaunay_sphere.R" code
