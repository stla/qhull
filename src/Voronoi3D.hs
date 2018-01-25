module Voronoi3D
  (Edge3(..)
 , Cell3
 , Voronoi3
 , prettyShowVoronoi3
 , clipVoronoi3
 , voronoiCell3
 , voronoi3
 , cell3Vertices
 , voronoi3vertices
 , boundedCell3
 , restrictVoronoi3
 , restrictVoronoi3box)
  where
import           Control.Arrow      (second)
import           Data.List
import           Data.Tuple.Extra   (both)
import           Delaunay
import           Text.Show.Pretty   (ppShow)
import           Voronoi.Voronoi

type Point3 = (Double, Double, Double)
type Vector3 = (Double, Double, Double)
data Edge3 = Edge3 (Point3, Point3) | IEdge3 (Point3, Vector3)
             | TIEdge3 (Point3, Point3)
              deriving Show
type Cell3 = [Edge3]
type Voronoi3 = [([Double], Cell3)]
type Box3 = (Double, Double, Double, Double, Double, Double)

-- | pretty print a 3D Voronoi diagram
prettyShowVoronoi3 :: Voronoi3 -> Maybe Int -> IO ()
prettyShowVoronoi3 v m = do
  let string = intercalate "\n---\n" (map (prettyShowCell3 m) v)
  putStrLn string
  where
    roundPairPoint3 :: (Point3, Point3) -> Int -> (Point3, Point3)
    roundPairPoint3 ((x1,x2,x3), (y1,y2,y3)) n =
      (asTriplet $ map (approx n) [x1,x2,x3],
       asTriplet $ map (approx n) [y1,y2,y3])
    prettyShowEdge3 :: Maybe Int -> Edge3 -> String
    prettyShowEdge3 n edge = case edge of
      Edge3 x   -> " Edge " ++ string x
      IEdge3 x  -> " IEdge " ++ string x
      TIEdge3 x -> " TIEdge " ++ string x
      where
        string x = ppShow $ maybe x (roundPairPoint3 x) n
    prettyShowEdges3 :: Maybe Int -> [Edge3] -> String
    prettyShowEdges3 n edges = intercalate "\n" (map (prettyShowEdge3 n) edges)
    prettyShowCell3 :: Maybe Int -> ([Double], Cell3) -> String
    prettyShowCell3 n (site, edges) =
      "[Double] " ++ ppShow site ++ " :\n" ++ prettyShowEdges3 n edges

asTriplet :: [a] -> (a, a, a)
asTriplet [x,y,z] = (x,y,z)
asTriplet _       = (undefined, undefined, undefined)

edgeToEdge3 :: Edge -> Edge3
edgeToEdge3 (Edge (x, y))  = Edge3 (both asTriplet (x, y))
edgeToEdge3 (IEdge (x, v)) = IEdge3 (both asTriplet (x, v))

equalFacets :: TileFacet -> TileFacet -> Bool
equalFacets tfacet1 tfacet2 =
  length f1 == 1 && length f2 == 1 &&
  _circumcenter p1 == _circumcenter p2 &&
  _normal p1 == _normal p2
  where
    (p1, f1) = tileFacetAsPair tfacet1
    (p2, f2) = tileFacetAsPair tfacet2

-- | Voronoi cell of a vertex given by its index
voronoiCell3 :: Tesselation -> Index -> Cell3
voronoiCell3 = voronoiCell (nubBy equalFacets) edgeToEdge3

-- | 3D Voronoi diagram
voronoi3 :: Tesselation -> Voronoi3
voronoi3 = voronoi voronoiCell3

-- | whether a 3D Voronoi cell is bounded
boundedCell3 :: Cell3 -> Bool
boundedCell3 = all isEdge
  where
    isEdge (Edge3 _) = True
    isEdge _         = False

-- | whether a 3D Voronoi cell is inside a given box
cell3inBox :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Cell3 -> Bool
cell3inBox (xmin,xmax) (ymin, ymax) (zmin,zmax) cell =
  boundedCell3 cell && all edgeInBox cell
  where
    tripletInBox (x,y,z) =
      x > xmin && y > ymin && z > zmin && x < xmax && y < ymax && z < zmax
    edgeInBox (Edge3 (p1,p2)) = tripletInBox p1 && tripletInBox p2
    edgeInBox _ = False

filterVoronoi3 :: (Cell3 -> Bool) -> Voronoi3 -> Voronoi3
filterVoronoi3 cellTester = filter (\(_, cell) -> cellTester cell)

-- | restrict a 3D Voronoi diagram to its bounded cells
restrictVoronoi3 :: Voronoi3 -> Voronoi3
restrictVoronoi3 = filterVoronoi3 boundedCell3

-- | restrict a 3D Voronoi diagram to the cells contained in a box
restrictVoronoi3box :: (Double,Double) -> (Double,Double) -> (Double,Double)
                    -> Voronoi3 -> Voronoi3
restrictVoronoi3box xlim ylim zlim = filterVoronoi3 (cell3inBox xlim ylim zlim)

-- | vertices of a bounded 3D cell
cell3Vertices :: Cell3 -> [[Double]]
cell3Vertices cell = nub $ concatMap extractVertices cell
  where
    extractVertices :: Edge3 -> [[Double]]
    extractVertices (Edge3 ((x1,y1,z1),(x2,y2,z2))) = [[x1,y1,z1],[x2,y2,z2]]
    extractVertices _                               = []

-- | vertices of a 3D Voronoi diagram
voronoi3vertices :: Voronoi3 -> [[Double]]
voronoi3vertices = concatMap (\(_,cell) -> cell3Vertices cell)

truncEdge3 :: Box3 -> Edge3 -> Edge3
truncEdge3 (xmin, xmax, ymin, ymax, zmin, zmax) edge =
  if isIEdge edge
    then TIEdge3 ((p1,p2,p3), (p1 + factor v1 v2 v3 * v1,
                  p2 + factor v1 v2 v3 * v2, p3 + factor v1 v2 v3 * v3))
    else edge
  where
    isIEdge (IEdge3 _) = True
    isIEdge _          = False
    IEdge3 ((p1,p2,p3), (v1,v2,v3)) = edge
    -- factor u1 u2 u3 | u1==0 && u2==0 = if u3>0 then (zmax-p3)/u3 else (zmin-p3)/u3
    --                 | u1==0 && u3==0 = if u2>0 then (ymax-p2)/u2 else (ymin-p2)/u2
    --                 | u2==0 && u3==0 = if u1>0 then (xmax-p1)/u1 else (xmin-p1)/u1
    --                 | otherwise = min (min (factor u1 0 0) (factor 0 u2 0))
    --                                   (factor 0 0 u3)
    -- IEdge3 ((p1,p2,p3), (v1,v2,v3)) = edge
    factor u1 u2 u3 | u3==0 = factor2 (xmin,xmax,ymin,ymax) (p1,p2) (u1,u2)
                    | u2==0 = factor2 (zmin,zmax,xmin,xmax) (p3,p1) (u3,u1)
                    | u1==0 = factor2 (ymin,ymax,zmin,zmax) (p2,p3) (u2,u3)
                    | otherwise = min (min (factor u1 u2 0) (factor 0 u2 u3))
                                      (factor u1 0 u3)

-- | clip 3D Voronoi diagram in a bounding box
clipVoronoi3 :: Box3 -> Voronoi3 -> Voronoi3
clipVoronoi3 box = map (second (map (truncEdge3 box)))
