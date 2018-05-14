module Main
  where
import           Delaunay.Examples
import qualified Data.IntMap.Strict  as IM
import           Delaunay
import           Delaunay.R
import           System.IO
import           Text.Show.Pretty
import           Data.HashMap.Strict.InsOrd as H hiding (map)

tesseractVertices :: [[Double]]
tesseractVertices =
  map (map (/2))
      [ [-1,-1,-1,-1],
        [-1,-1,-1, 1],
        [-1,-1, 1,-1],
        [-1,-1, 1, 1],
        [-1, 1,-1,-1],
        [-1, 1,-1, 1],
        [-1, 1, 1,-1],
        [-1, 1, 1, 1],
        [ 1,-1,-1,-1],
        [ 1,-1,-1, 1],
        [ 1,-1, 1,-1],
        [ 1,-1, 1, 1],
        [ 1, 1,-1,-1],
        [ 1, 1,-1, 1],
        [ 1, 1, 1,-1],
        [ 1, 1, 1, 1]
      ]

main :: IO ()
main = do

  dtesseract <- delaunay tesseractVertices True False Nothing
  let vertices = IM.elems $ _vertices dtesseract
  let edges = Prelude.map fromPair $ H.keys $ _edges dtesseract
  putStrLn "VERTICES:"
  pPrint vertices
  putStrLn "\nEDGES:"
  pPrint edges
    where
      fromPair (Pair i j) = (i,j)

  -- tess <- delaunay nonConvexPolyhedron False False Nothing
  -- let code = delaunaySpheres tess
  -- writeFile "rgl/delaunay_spheres_nonConvexPolyhedron.R" code

  --  x <- [0,0,0] : randomOnSphere 100 3
  -- tess <- delaunay x False False Nothing
  -- let code = delaunay3rgl tess True False True True Nothing
  -- writeFile "rgl/delaunay_sphere_interior.R" code

  -- tess <- delaunay duoCylinder False False Nothing
  -- let edges = H.elems $ _edges tess
  -- let vertices = IM.elems $ _vertices tess
  -- let edgesKeys = Prelude.map fromPair $ H.keys $ _edges tess
  -- putStrLn "VERTICES:"
  -- pPrint vertices
  -- putStrLn "\nEDGES:"
  -- pPrint edges
  -- putStrLn "\nEDGES KEYS:"
  -- pPrint edgesKeys
  --   where
  --     fromPair (Pair i j) = (i,j)
