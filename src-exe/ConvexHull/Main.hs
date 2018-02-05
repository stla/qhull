module Main
  where
import           ConvexHull
import           ConvexHull.Examples
import           ConvexHull.R
import qualified Data.HashMap.Strict.InsOrd as H
import qualified Data.IntMap.Strict         as IM
import           Data.List
import qualified Data.Set                   as S
import           System.IO
import           Text.Printf
import           Text.Show.Pretty

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

main :: IO ()
main = do

  -- code <- convexHull3DrglCode irregularPolyhedron True (Just "rgl/irregularPolyhedron.R")
  h <- convexHull truncatedCuboctahedron False False Nothing
  putStrLn $ hullSummary h
  pPrint $ _vertices h
  pPrint $ IM.elems $ IM.map facetToPolygon' (_hfacets h)
  pPrint $ edgesIds' h
  pPrint $ IM.elems $ IM.map _normal (_hfacets h)

  -- code <- convexHull3DrglCode mobiusStrip True (Just "rgl/mobiusHull02.R")
  -- h <- convexHull mobiusStrip False False Nothing
  -- putStrLn $ hullSummary h

  -- code <- convexHull3DrglCode truncatedCuboctahedron False (Just "rgl/truncatedCuboctahedron.R")
  -- putStrLn "done"

  -- h <- convexHull truncatedCuboctahedron False False Nothing
  -- pPrint $ IM.map toVertex3 (_vertices h)
  -- pPrint $ IM.elems $ IM.map facetToPolygon' (_hfacets h)
  -- pPrint $ edgesIds' h

  -- h <- convexHull truncatedCuboctahedron False False Nothing
  -- pPrint $ IM.elems $ IM.map facetToPolygon (_hfacets h)
  -- putStrLn $ hullSummary h

  -- code <- convexHull3DrglCode spheresPack True (Just "rgl/convexhull_spheresPack.R")
  -- putStrLn "done"

  -- h <- convexHull truncatedTesseract False False Nothing
  -- putStrLn $ hullSummary h

  -- h <- convexHull nonConvexPolyhedron False False Nothing
  -- putStrLn $ hullSummary h
  -- --code <- convexHull3DrglCode nonConvexPolyhedron True (Just "rgl/convexhull_nonConvexPolyhedron.R")
  -- putStrLn "done"

  -- h <- convexHull truncatedTesseract False False Nothing
  -- putStrLn $ hullSummary h
  -- let edges = H.keys (_edges h)
  -- let f (Pair i j) = printf "coolsegment3d(rbind(x[%d,],x[%d,]))\n" (i+1) (j+1)
  -- let code = map f edges
  -- pPrint $ verticesCoordinates h
  -- putStrLn $ concat code

  -- points <- randomInCube 100
  -- hull <- convexHull points False False Nothing
  -- pPrint $ _hfacets hull
  -- pPrint $ _hedges hull
  -- pPrint $ _hvertices hull

  -- code <- convexHull3DrglCode teapot True (Just "rgl/convexhull_teapot.R")
  -- putStrLn "done"

  -- let curve3D = map (\x -> [ sin (pi*x) * cos (2*pi*x)
  --                         ,  sin (pi*x) * sin (2*pi*x)
  --                         ,  cos (pi*x)]) [i/200 | i <- [0 .. 200]]
  -- code <- convexHull3DrglCode (nub $ curve3D ++ map (\[x,y,z] -> [x,y,z+2]) curve3D) True
  --                             (Just "rgl/convexhull_curveOnSphere3.R")
  -- putStrLn "done"

  -- let curve3D = map (\x -> [ sin (pi*x) * cos (2*pi*x)
  --                         ,  sin (pi*x) * sin (2*pi*x)
  --                         ,  cos (pi*x)]) [i/200 | i <- [0 .. 200]]
  -- code <- convexHull3DrglCode curve3D True (Just "rgl/convexhull_curveOnSphere.R")
  -- putStrLn "done"

  -- let c = 4
  --     a = 1
  -- let curve3D = map (\x -> [ cos (2*pi*x) * (c + a * cos (2*pi*x))
  --                         ,  sin (2*pi*x) * (c + a * cos (2*pi*x))
  --                         ,  a * sin (2*pi*x)]) [i/50 | i <- [0 .. 50]]
  -- code <- convexHull3DrglCode curve3D True (Just "rgl/convexhull_curveOnTorus.R")
  -- putStrLn "done"

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

  -- chull <- convexHull cube4 True False Nothing
  -- pPrint chull

  -- chull <- convexHull cube5 False False Nothing
  -- putStrLn "done"
  -- pPrint chull
  -- pPrint $ length $ xxx chull
  -- pPrint $ length $ nub $ xxx chull
  -- pPrint $ S.size (_alledges chull)

  -- let square = [[0,0],[0,1],[1,0],[1,1]]
  -- chull <- convexHull square False
  -- pPrint chull
