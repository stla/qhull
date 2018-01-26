module Main
  where
import           ConvexHull
import           ConvexHull.Examples
import           ConvexHull.R
import           Data.List
import qualified Data.Set            as S
import           System.IO
import           Text.Show.Pretty

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

main :: IO ()
main = do

  let c = 4
      a = 1
  let curve3D = map (\x -> [ cos (2*pi*x) * (c + a * cos (2*pi*x))
                          ,  sin (2*pi*x) * (c + a * cos (2*pi*x))
                          ,  a * sin (2*pi*x)]) [i/50 | i <- [0 .. 50]]
  code <- convexHull3DrglCode curve3D True (Just "rgl/convexhull_curveOnTorus.R")
  putStrLn "done"

  -- points <- randomInCube 1000
  -- code <- convexHull3DrglCode (map (map (approx 4)) points) True (Just "rgl/convexhull04.R")
  -- putStrLn "done"

  -- let square3D = [[-1,-1, 0]
  --                ,[-1,-1, 0]
  --                ,[-1, 1, 0]
  --                ,[-1, 1, 0]]
  -- chull <- convexHull square3D False
  -- pPrint chull

  -- let squareLattice = [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]]
  -- chull <- convexHull squareLattice False False Nothing
  -- putStrLn "\n--- SQUARE LATTICE ---"
  -- pPrint chull
  --
  -- let cube = [[-1,-1,-1]
  --            ,[-1,-1, 1]
  --            ,[-1, 1,-1]
  --            ,[-1, 1, 1]
  --            ,[ 1,-1,-1]
  --            ,[ 1,-1, 1]
  --            ,[ 1, 1,-1]
  --            ,[ 1, 1, 1]]
  -- chull2 <- convexHull cube False
  -- putStrLn "\n--- CUBE ---"
  -- pPrint chull2

  -- let hcube = [[-1,-1,-1,-1]
  --             ,[-1,-1, 1,-1]
  --             ,[-1, 1,-1,-1]
  --             ,[-1, 1, 1,-1]
  --             ,[ 1,-1,-1,-1]
  --             ,[ 1,-1, 1,-1]
  --             ,[ 1, 1,-1,-1]
  --             ,[ 1, 1, 1,-1]
  --             ,[-1,-1,-1, 1]
  --             ,[-1,-1, 1, 1]
  --             ,[-1, 1,-1, 1]
  --             ,[-1, 1, 1, 1]
  --             ,[ 1,-1,-1, 1]
  --             ,[ 1,-1, 1, 1]
  --             ,[ 1, 1,-1, 1]
  --             ,[ 1, 1, 1, 1]]
  -- chull <- convexHull hcube False
  -- pPrint (_allridges chull)
  -- pPrint (_allvertices chull)
  -- pPrint (_faces chull)

  -- chull <- convexHull cube5 False False Nothing
  -- putStrLn "done"
  -- pPrint chull
  -- pPrint $ length $ xxx chull
  -- pPrint $ length $ nub $ xxx chull
  -- pPrint $ S.size (_alledges chull)

  -- let square = [[0,0],[0,1],[1,0],[1,1]]
  -- chull <- convexHull square False
  -- pPrint chull
