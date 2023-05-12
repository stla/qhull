module Main
  where
import           ConvexHull
import           ConvexHull.BiTruncatedTesseract
import           ConvexHull.CantiTrunc600Cell.Data
import           ConvexHull.Examples                          hiding
                                                               (regularSphere,
                                                               regularTetrahedron)
import           ConvexHull.OmniTruncated120Cell
import           ConvexHull.R
import           ConvexHull.SnubDodecahedron.SnubDodecahedron
import           ConvexHull.Truncated120Cell3
import           Data.Function                                (on)
import qualified Data.HashMap.Strict.InsOrd                   as H
import qualified Data.IntMap.Strict                           as IM
import           Data.List
import           Data.List.Index
-- import           Data.Permute                                 (elems, rank)
import qualified Data.Set                                     as S
import           Data.Tuple.Extra
import           System.IO
import           Text.Printf
import           Text.Regex
import           Text.Show.Pretty

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

roundedVertices :: Int -> [[Double]] -> [[Double]]
roundedVertices n = map (map (approx n))


-- fixIndices :: [[Double]] -> [[Int]] -> ([[Double]], [[Int]])
-- fixIndices allVertices faces = (newvertices, newfaces)
--   where
--   faceselems = nub $ foldr union [] faces
--   l = length faceselems
--   permute = elems $ rank l faceselems
--   mapper = IM.fromList $ zip permute faceselems
--   mapper' = IM.fromList $ zip faceselems permute
--   newfaces = map (map ((IM.!) mapper')) faces
--   --newvertices =
-- --    (fromJust <$> (filter isJust $ (map atMay ([allVertices !! (mapper IM.! i) | i <- IM.keys mapper])))) `intersect` IM.keys mapper
--   -- newvertices = [allVertices !! (mapper IM.! i) | i <- IM.keys mapper]
--   newvertices = [allVertices !! i | i <- [0 .. length allVertices-1] `intersect` IM.keys mapper]
-- --
-- -- regularTetrahedron' :: [[Double]]
-- -- regularTetrahedron' =
-- --   [ [0.5 / sqrt 3, -0.5, 0.5 / sqrt 6]
-- --   , [sqrt 3 / 3, 0, -0.5 / sqrt 6]
-- --   , [0.5 / sqrt 3, 0.5, -0.5 / sqrt 6]
-- --   , [0, 0, 0.5 * sqrt 3 / sqrt 2] ]
-- --
-- regularSphere :: Int -> [Double] -> Double -> ([[Double]], [[Int]])
-- regularSphere n center rho =
--   (zipWith (s2c rho) theta phi, [[i,j] | i <- [0 .. n-1], j <- [1 .. n-1]])
--   where
--   gridtheta = [frac i n | i <- [0 .. n-1]]
--   theta = map (*(2*pi)) gridtheta
--   gridphi = [frac i n | i <- [1 .. n-1]]
--   phi = map (*pi) gridphi
--   frac :: Int -> Int -> Double
--   frac p q = realToFrac p / realToFrac q
--   s2c :: Double -> Double -> Double -> [Double]
--   s2c r th ph = [r * cos th * sin ph + center!!0, r * sin th * sin ph + center!!1, r * cos ph + center!!2]
--
-- sphere1,sphere2,sphere3,sphere4 :: ([[Double]], [[Int]])
-- sphere1 = regularSphere 40 (regularTetrahedron !! 0) (sqrt 6 / 4)
-- sphere2 = regularSphere 40 (regularTetrahedron !! 1) (sqrt 6 / 4)
-- sphere3 = regularSphere 40 (regularTetrahedron !! 2) (sqrt 6 / 4)
-- sphere4 = regularSphere 40 (regularTetrahedron !! 3) (sqrt 6 / 4)
--
--
-- -- regular tetrahederon -- --
-- regularTetrahedron :: [[Double]]
-- regularTetrahedron =
--     -- [[i, 0, -1/sqrt 2] | i <- pm] ++ [[0, i , 1/sqrt 2] | i <- pm]
--     -- where pm = [-1,1]
--     [ [ -1.0 , 0.0 , -0.7071067811865475 ]
--     , [ 0.0 , 1.0 , -0.7071067811865475 ]
--     , [ 0.0 , -1.0 , 0.7071067811865475 ]
--     , [ 1.0, 0.0 ,  0.7071067811865475 ]
--     ]
--

-- stringify :: Show a => String -> [a] -> String
-- stringify sep = intercalate sep . map show


main :: IO ()
main = do

  h <- convexHull sixhundredCell False False Nothing
  pPrint $ hullSummary h
  putStrLn "vertices:"
  pPrint $ roundedVertices 4 $ verticesCoordinates h
  putStrLn "edges:"
  pPrint $ edgesIds' h
  putStrLn "\nTETRAHEDRAL FACETS:"
  let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  pPrint $ IM.toList (IM.map verticesIds tetras)

  -- h <- convexHull hexadecachoron False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- pPrint $ IM.toList (IM.map verticesIds tetras)

  -- h <- convexHull runcitruncated5cell False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h

  -- h <- convexHull rectified5cell False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- pPrint $ IM.map verticesIds tetras

  -- h <- convexHull sircope False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTRIANGLE PRISM FACETS:"
  -- let tprisms = IM.filter (\f -> length (verticesIds f) == 6) (_hfacets h)
  -- pPrint $ IM.elems (IM.map verticesIds tprisms)
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) (IM.elems tprisms)
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull tutcup False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- pPrint $ IM.toList (IM.map verticesIds tetras)

  -- h <- convexHull runcinatedTesseract False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- pPrint $ IM.map verticesIds tetras

  -- h <- convexHull runcinated5cells False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) (IM.elems tetras)
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull duoprism330 False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "same vertices:"
  -- print $ verticesCoordinates h == duoprism330
  -- putStrLn "\nALL ROUNDED VERTICES:"
  -- pPrint $ roundedVertices 3 $ verticesCoordinates h

  -- let curve3D = map (\x -> [ sin (pi*x) * cos (2*pi*x)
  --                          , sin (pi*x) * sin (2*pi*x)
  --                          , cos (pi*x)]) [i/100 | i <- [0 .. 100]]
  -- h <- convexHull curve3D True False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ roundedVertices 10 $ verticesCoordinates h
  -- putStrLn "facets:"
  -- pPrint $ map (IM.keys . _vertices) (IM.elems $ _hfacets h) -- TODO FAIRE UNE FONCTION FACETIDS
  -- c'est fait : verticesIds = IM.keys . _vertices
  -- direct : facetsVerticesIds h

  -- h <- convexHull truncated5cells False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ roundedVertices 10 $ verticesCoordinates h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- let tetras = IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) (IM.elems tetras)
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull truncated24cells False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nCUBICAL FACETS:"
  -- let cubes = IM.filter (\f -> length (verticesIds f) == 8) (_hfacets h)
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) (IM.elems cubes)
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull biTruncatedTesseract False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ roundedVertices 3 $ verticesCoordinates h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nTRUNCATED OCTAHEDRAL FACETS:"
  -- let octahedra = IM.filter (\f -> length (verticesIds f) == 24) (_hfacets h)
  -- putStrLn "\nTRUNCATED TETRAHEDRAL FACETS:"
  -- let tetrahedra = IM.filter (\f -> length (verticesIds f) == 12) (_hfacets h)
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) (IM.elems tetrahedra)
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull truncatedTetrahedron False False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ roundedVertices 3 truncatedTetrahedron == roundedVertices 3 (verticesCoordinates h)
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

  -- h <- convexHull duoprism1616 False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h

  -- h <- convexHull thex False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ roundedVertices 3 $ verticesCoordinates h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "\nOCTAHEDRAL FACETS:"
  -- let octahedra = IM.filter (\f -> length (verticesIds f) == 6) (_hfacets h)
  -- pPrint $ IM.elems $ IM.map verticesIds octahedra
  -- pPrint octahedra
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) (IM.elems octahedra)
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

--  h <- convexHull cuboctahedron4d False False Nothing
--  pPrint $ hullSummary h
--  putStrLn "vertices:"
---  pPrint $ verticesCoordinates h
--  putStrLn "facets:"
--  let facets = IM.elems (_hfacets h)
--  let polygons = map (map fst . facetToPolygon') facets
--  pPrint polygons
--  putStrLn "edges:"
--  pPrint $ edgesIds' h
---  putStrLn "ridges:"
--  let ridges = map (IM.elems . facetRidges h) facets
--  pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull daVinci False False Nothing
  -- pPrint $ hullSummary h
  -- code <- convexHull3DrglCode daVinci False (Just "rgl/daVinci.R")
  -- putStrLn "done"

  -- h <- convexHull twocircles False False Nothing
  -- pPrint $ hullSummary h
  -- code <- convexHull3DrglCode twocircles False (Just "rgl/oloid.R")
  -- putStrLn "done"

  -- h <- convexHull vs120omnitrunc False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "ridges:"
  -- let ridges = map (IM.elems . facetRidges h) facets
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

--  let curve3D = map (\x -> [ sin (pi*x) * cos (2*pi*x)
--                         ,  sin (pi*x) * sin (2*pi*x)
--                          ,  cos (pi*x)]) [i/200 | i <- [0 .. 200]]
-- h <- convexHull curve3D True False Nothing
--  hullToSTL h "strangeHull.stl"


--  h <- convexHull snubDodecahedron True False Nothing
--  hullToSTL h "MYTEST.stl"
--  pPrint $ hullSummary h
--  putStrLn "facets:"
--  let facets = IM.elems (_hfacets h)
--  let normals = map _normal facets
--  let facetNormals = map (\n -> "facet normal  " ++ stringify " " n ++ "\nouter loop")
--                     normals
--  pPrint facetNormals
--  putStrLn "facets normals done"
--  putStrLn "facets polygons:"
--  let polygons = map (\f -> "\nvertex " ++ (stringify "\nvertex " . map snd . facetToPolygon') f) facets
--  pPrint polygons
  --let polygons = (map (map stringify . snd) . facetToPolygon') facets
 -- let polygons = map (\f -> stringify ("vertex" ++ ((show . snd) $ facetToPolygon' f))) facets ++ "\nendloop\nendfacet"
--  let vertices = map (\v -> v ++ "\nendloop\nendfacet") polygons
--  let vertices' = map (\v -> subRegex (mkRegex "\\]") (subRegex (mkRegex "\\[") v "") "") vertices
--  putStrLn "vertices:"
--  pPrint vertices'
--  putStrLn "THE CONCATENATION:"
--  pPrint $ subRegex (mkRegex ",") (concat [x ++ y | x <- facetNormals, y <- vertices']) " "
--  putStrLn "concatenation 0:"
--  pPrint $ stringify " " $ concat [unlines [x,y] | x <- facetNormals, y <- vertices']
--  putStrLn "concatenation 1:"
--  pPrint $ [unlines [x,y] | x <- facetNormals, y <- vertices']
--  putStrLn "concatenation 2:"
--  pPrint $ [stringify " " [x,y] | x <- facetNormals, y <- vertices']
  -- putStrLn "vertices:"
--  pPrint $ verticesCoordinates h
--  let polygons = map (map fst . facetToPolygon') facets
--  pPrint polygons
--  putStrLn "edges:"
--  pPrint $ edgesIds' h

--  h <- convexHull biTruncatedTesseract False False Nothing
--  pPrint $ hullSummary h
--  putStrLn "vertices:"
--  pPrint $ verticesCoordinates h
--  putStrLn "facets:"
--  let facets = IM.elems (_hfacets h)
--  let polygons = map (map fst . facetToPolygon') facets
--  pPrint polygons
--  putStrLn "edges:"
--  pPrint $ edgesIds' h

  -- let cube10 = ncube 10
  -- putStrLn "cube10:"
  -- let thecube = map swap $ indexed cube10
  -- pPrint thecube
  -- chull <- convexHull cube10 False False Nothing
  -- putStrLn "done"
  -- pPrint $ hullSummary chull
  -- pPrint $ verticesCoordinates chull
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets chull)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' chull

  -- h <- convexHull allVertices False False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- -- putStrLn "tetrahedral facets:"
  -- -- pPrint $ IM.elems $ IM.map verticesIds $ IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)

  -- -- sphere1'
  -- points <- randomOnSphere 100 1
  -- h <- convexHull points True False Nothing
  -- pPrint $ hullSummary h
  -- putStrLn "vertices sphere 1:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets sphere 1:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

  -- h <- convexHull allVertices False False Nothing
  -- pPrint $ hullSummary h

  -- h <- convexHull reuleuxTetrahedron True False Nothing
  -- let facets = IM.elems (_hfacets h)
  -- putStrLn "facets:"
  -- pPrint facets
  -- putStrLn "facets' (oriented):"
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

  -- h <- convexHull qcube2 True False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ qcube2 == verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  --
-- -- sphere1'
--   let (vs1, fs1) = fixIndices (fst sphere1) (snd sphere1)
--   h <- convexHull (vs1) True False Nothing
--   putStrLn "vertices 1:"
--   pPrint vs1
--   putStrLn "facets:"
--   let facets = IM.elems (_hfacets h)
--   let polygons = map (map fst . facetToPolygon') facets
--   pPrint polygons
--
-- --  sphere2' :: IO ([[Double]], [[Int]])
--   let (vs2,fs2) =  fixIndices (fst sphere2) (snd sphere2)
--   h <- convexHull (vs2) True False Nothing
--   putStrLn "vertices 2:"
--   pPrint vs2
--   putStrLn "facets:"
--   let facets = IM.elems (_hfacets h)
--   let polygons = map (map fst . facetToPolygon') facets
--   pPrint polygons
--
-- --  sphere3' :: IO ([[Double]], [[Int]])
--   let (vs3, fs3) = fixIndices (fst sphere3) (snd sphere3)
--   h <- convexHull (vs3) True False Nothing
--   putStrLn "vertices 3:"
--   pPrint vs3
--   putStrLn "facets:"
--   let facets = IM.elems (_hfacets h)
--   let polygons = map (map fst . facetToPolygon') facets
--   pPrint polygons
--
-- --  sphere4' :: IO ([[Double]], [[Int]])
--   let (vs4, fs4) = fixIndices (fst sphere4) (snd sphere4)
--   h <- convexHull (vs4) True False Nothing
--   putStrLn "vertices 4:"
--   pPrint vs4
--   putStrLn "facets:"
--   let facets = IM.elems (_hfacets h)
--   let polygons = map (map fst . facetToPolygon') facets
--   pPrint polygons

  -- putStrLn "sphere1'"
  -- pPrint sphere1'
  -- let facets = IM.elems (_hfacets h)
  --     polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- putStrLn "shpere2'"
  -- pPrint sphere2'
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- putStrLn "shpere3'"
  -- pPrint sphere3'
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- putStrLn "shpere4'"
  -- pPrint sphere4'
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons


--
--   h <- convexHull (dodecaplex) False False Nothing
--   putStrLn $ hullSummary h
--   putStrLn "edges:"
--   pPrint $ edgesIds' h
--   putStrLn "all vertices:"
--   pPrint $ verticesCoordinates h
--   putStrLn "ridges:"
--   let facets = IM.elems (_hfacets h)
--   let ridges = map (IM.elems . facetRidges h) facets
--   pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull qcube1 True False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ qcube1 == verticesCoordinates h
  -- putStrLn "facets cube1:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  --
  -- h <- convexHull qcube2 True False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ qcube2 == verticesCoordinates h
  -- putStrLn "facets cube2:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  --
  -- h <- convexHull qcube3 True False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ qcube3 == verticesCoordinates h
  -- putStrLn "facets cube3:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  --
  -- h <- convexHull qcube4 True False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ qcube4 == verticesCoordinates h
  -- putStrLn "facets cube4:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  --
  -- h <- convexHull qcube5 True False Nothing
  -- putStrLn "same vertices:"
  -- pPrint $ qcube5 == verticesCoordinates h
  -- putStrLn "facets cube5:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

  -- h <- convexHull regularTetrahedron False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- code <- convexHull3DrglCode regularTetrahedron False (Just "rgl/regularTetrahedron.R")
  -- putStrLn "done"

  -- let points = regularSphere 30
  -- h <- convexHull points True False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons
  -- writeFile "Data/sphere.txt" (show (verticesCoordinates h, polygons))

  -- h <- convexHull octaplex False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "ridges:"
  -- let facets = IM.elems (_hfacets h)
  -- let ridges = map (IM.elems . facetRidges h) facets
  -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull icosahedron False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

  -- let cube = [[-1,-1,-1],
  --             [-1,-1, 1],
  --             [-1, 1,-1],
  --             [-1, 1, 1],
  --             [ 1,-1,-1],
  --             [ 1,-1, 1],
  --             [ 1, 1,-1],
  --             [ 1, 1, 1]]
  -- h <- convexHull cube False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "same vertices:"
  -- print $ cube == verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

  -- points <- randomOnSphere 50 1
  -- h <- convexHull points True False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- let facets = IM.elems (_hfacets h)
  -- let polygons = map (map fst . facetToPolygon') facets
  -- pPrint polygons

    -- h <- convexHull duocylinder False False Nothing
    -- putStrLn $ hullSummary h
    -- putStrLn "\nedges:"
    -- pPrint $ edgesIds' h
    -- putStrLn "\nall vertices:"
    -- pPrint $ verticesCoordinates h
    -- putStrLn "\nridges:"
    -- let facets = IM.elems (_hfacets h)
    -- let ridges = map (IM.elems . facetRidges h) facets
    -- pPrint $ map (map (map fst . ridgeToPolygon)) ridges

  -- h <- convexHull hexaSquare False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h

  -- h <- convexHull snub24cell False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "same vertices:"
  -- print $ verticesCoordinates h == snub24cell
  -- putStrLn "\nALL ROUNDED VERTICES:"
  -- pPrint $ roundedVertices 2 $ verticesCoordinates h
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- pPrint $ IM.elems $ IM.map verticesIds $ IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)

  -- h <- convexHull triangularDuoprism False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "same vertices:"
  -- print $ verticesCoordinates h == triangularDuoprism
  -- putStrLn "one prism:"
  -- let facet = head $ IM.elems (_hfacets h)
  -- let ridges = IM.elems $ facetRidges h facet
  -- pPrint $ map (map fst . ridgeToPolygon) ridges
  -- putStrLn "edges of this facet:"
  -- pPrint $ edgesIds' facet

  -- h <- convexHull duoprism35 False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "same vertices:"
  -- print $ verticesCoordinates h == duoprism34
  -- putStrLn "\nALL ROUNDED VERTICES:"
  -- pPrint $ roundedVertices 3 $ verticesCoordinates h
  -- putStrLn "\nONE PENTAGONAL PRISM:"
  -- let prisms = IM.filter (\f -> length (verticesIds f) == 10) (_hfacets h)
  --     onePrism = head $ IM.elems prisms
  -- putStrLn "\nRIDGES OF THIS PRISM:"
  -- let ridges = IM.elems $ facetRidges h onePrism
  -- pPrint $ map (map fst . ridgeToPolygon) ridges

  -- h <- convexHull duoprism34 False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "same vertices:"
  -- print $ verticesCoordinates h == duoprism34
  -- putStrLn "\nALL ROUNDED VERTICES:"
  -- pPrint $ roundedVertices 3 $ verticesCoordinates h
  -- putStrLn "\nONE SQUARE PRISM:"
  -- let squarePrisms = IM.filter (\f -> length (verticesIds f) == 8) (_hfacets h)
  --     oneSquarePrism = head $ IM.elems squarePrisms
  -- putStrLn "\nRIDGES OF THIS SQUARE PRISM:"
  -- let ridges = IM.elems $ facetRidges h oneSquarePrism
  -- pPrint $ map (map fst . ridgeToPolygon) ridges
  -- putStrLn "\nONE TRIANGULAR PRISM:"
  -- let triPrisms = IM.filter (\f -> length (verticesIds f) == 6) (_hfacets h)
  --     oneTriPrism = head $ IM.elems triPrisms
  -- putStrLn "\nRIDGES OF THIS TRIANGULAR PRISM:"
  -- let ridges' = IM.elems $ facetRidges h oneTriPrism
  -- pPrint $ map (map fst . ridgeToPolygon) ridges'

  -- h <- convexHull hexagonalDuoprism False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "same vertices:"
  -- print $ verticesCoordinates h == hexagonalDuoprism
  -- putStrLn "one hexagonal prism:"
  -- let facet = head $ IM.elems (_hfacets h)
  -- let ridges = IM.elems $ facetRidges h facet
  -- pPrint $ map (map fst . ridgeToPolygon) ridges
  -- putStrLn "edges of this facet:"
  -- pPrint $ edgesIds' facet

  -- h <- convexHull cantellatedTesseract False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "good ridges:"
  -- let goodfacets = IM.elems $ IM.filter (\f -> nEdges f `elem` [9,12]) (_hfacets h)
  -- let ridges = nubBy ((==) `on` verticesIds) $ concatMap (IM.elems . facetRidges h) goodfacets
  -- pPrint $ map (map fst . ridgeToPolygon) ridges

  -- h <- convexHull rectifiedTesseract False False Nothing
  -- putStrLn $ hullSummary h
  -- putStrLn "edges:"
  -- pPrint $ edgesIds' h
  -- putStrLn "all vertices:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "facets:"
  -- pPrint $ IM.map verticesIds (_hfacets h)
  -- putStrLn "tetrahedral facets:"
  -- pPrint $ IM.elems $ IM.map verticesIds $ IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- putStrLn "ridges of facet 0:"
  -- let facet = _hfacets h IM.! 0
  --     ridges = facetRidges h facet
  -- pPrint $ map (map fst . ridgeToPolygon) (IM.elems ridges)

  -- h <- convexHull truncatedTesseract False False Nothing
  -- putStrLn "HULL SUMMARY:"
  -- putStrLn $ hullSummary h
  -- putStrLn "\nEDGES:"
  -- pPrint $ edgesIds' h
  -- -- putStrLn "original vertices:"
  -- -- pPrint $ take 2 truncatedTesseract
  -- -- putStrLn "vertices:"
  -- -- pPrint $ take 2 $ verticesCoordinates h
  -- putStrLn "\nALL VERTICES:"
  -- pPrint $ verticesCoordinates h
  -- putStrLn "\nALL ROUNDED VERTICES:"
  -- pPrint $ roundedVertices 2 $ verticesCoordinates h
  -- putStrLn "\nALL ROUNDED ORIGINAL VERTICES:"
  -- pPrint $ roundedVertices 2 truncatedTesseract
  -- -- putStrLn "facets:"
  -- -- pPrint $ IM.map verticesIds (_hfacets h)
  -- putStrLn "\nTETRAHEDRAL FACETS:"
  -- pPrint $ IM.elems $ IM.map verticesIds $ IM.filter (\f -> length (verticesIds f) == 4) (_hfacets h)
  -- -- putStrLn "ridges of facet 9:" -- pkoi ? parce que je voulais illuster une facette je pense...
  -- -- let facet = _hfacets h IM.! 9
  -- --     ridges = facetRidges h facet
  -- -- pPrint $ map (map fst . ridgeToPolygon) (IM.elems ridges)
  -- -- putStrLn "vertices of facet 9:"
  -- -- pPrint $ _vertices facet

  -- h <- convexHull icosahedron False False Nothing
  -- pPrint $ IM.map facetToPolygon (_hfacets h)
  -- putStrLn $ hullSummary h
  -- pPrint $ IM.elems $ IM.map (IM.elems . _vertices) (_hfacets h)

  -- -- code <- convexHull3DrglCode irregularPolyhedron True (Just "rgl/irregularPolyhedron.R")
  -- h <- convexHull truncatedCuboctahedron False False Nothing
  -- putStrLn $ hullSummary h
  -- pPrint $ _vertices h
  -- pPrint $ IM.elems $ IM.map facetToPolygon' (_hfacets h)
  -- pPrint $ edgesIds' h
  -- pPrint $ IM.elems $ IM.map _normal (_hfacets h)

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
