module Voronoi2D
  (Edge2(..)
 , Cell2
 , Voronoi2
 , prettyShowVoronoi2
 , voronoiCell2
 , voronoi2
 , clipVoronoi2
 , boundedCell2
 , restrictVoronoi2
 , cell2Vertices
 , cell2Vertices')
  where
import           Control.Arrow    (second)
import           Data.Graph       (flattenSCCs, stronglyConnComp)
import           Data.List
import           Data.List.Index  (imap)
import           Data.Tuple.Extra (both)
import           Delaunay
import           Text.Show.Pretty (ppShow)
import           Voronoi.Voronoi


type Point2 = (Double, Double)
type Vector2 = (Double, Double)
data Edge2 = Edge2 (Point2, Point2) | IEdge2 (Point2, Vector2)
             | TIEdge2 (Point2, Point2)
              deriving (Show, Eq)
type Cell2 = [Edge2]
type Voronoi2 = [([Double], Cell2)]
type Box2 = (Double, Double, Double, Double)

-- | pretty print a Voronoi 2D diagram
prettyShowVoronoi2 :: Voronoi2 -> Maybe Int -> IO ()
prettyShowVoronoi2 v m = do
  let string = intercalate "\n---\n" (map (prettyShowCell2 m) v)
  putStrLn string
  where
    roundPairPoint2 :: (Point2, Point2) -> Int -> (Point2, Point2)
    roundPairPoint2 ((x1,x2), (y1,y2)) n =
      (asPair $ map (approx n) [x1,x2], asPair $ map (approx n) [y1,y2])
    prettyShowEdge2 :: Maybe Int -> Edge2 -> String
    prettyShowEdge2 n edge = case edge of
      Edge2 x   -> " Edge " ++ string x
      IEdge2 x  -> " IEdge " ++ string x
      TIEdge2 x -> " TIEdge " ++ string x
      where
        string x = ppShow $ maybe x (roundPairPoint2 x) n
    prettyShowEdges2 :: Maybe Int -> [Edge2] -> String
    prettyShowEdges2 n edges = intercalate "\n" (map (prettyShowEdge2 n) edges)
    prettyShowCell2 :: Maybe Int -> ([Double], Cell2) -> String
    prettyShowCell2 n (site, edges) =
      "[Double] " ++ ppShow site ++ " :\n" ++ prettyShowEdges2 n edges

asPair :: [Double] -> (Double, Double)
asPair [a,b] = (a,b)
asPair _     = (undefined, undefined)

edgeToEdge2 :: Edge -> Edge2
edgeToEdge2 (Edge (x, y))  = Edge2 (both asPair (x, y))
edgeToEdge2 (IEdge (x, v)) = IEdge2 (both asPair (x, v))

-- | Voronoi cell of a vertex given by its index
voronoiCell2 :: Tesselation -> Index -> Cell2
voronoiCell2 = voronoiCell id edgeToEdge2

-- | 2D Voronoi Diagram
voronoi2 :: Tesselation -> Voronoi2
voronoi2 = voronoi voronoiCell2

-- | whether a 2D Voronoi cell is bounded
boundedCell2 :: Cell2 -> Bool
boundedCell2 = all isEdge
  where
    isEdge (Edge2 _) = True
    isEdge _         = False

-- | restrict a 2D Voronoi diagram to its bounded cells
restrictVoronoi2 :: Voronoi2 -> Voronoi2
restrictVoronoi2 = filter (\(_, cell) -> boundedCell2 cell)

-- | vertices of a bounded 2D cell
cell2Vertices :: Cell2 -> [[Double]]
cell2Vertices cell = nub $ concatMap extractVertices cell
  where
    extractVertices :: Edge2 -> [[Double]]
    extractVertices (Edge2 ((x1,x2),(y1,y2))) = [[x1,x2],[y1,y2]]
    extractVertices _                         = []

-- | ordered vertices of a bounded 2D cell
cell2Vertices' :: Cell2 -> [[Double]]
cell2Vertices' cell = flattenSCCs (stronglyConnComp x)
  where
    vs = cell2Vertices cell
    x = imap (\i v -> (v, i, findIndices (connectedVertices v) vs)) vs
    connectedVertices :: [Double] -> [Double] -> Bool
    connectedVertices [x1,x2] [y1,y2] =
      (Edge2 ((x1,x2),(y1,y2)) `elem` cell) ||
      (Edge2 ((y1,y2),(x1,x2)) `elem` cell)
    connectedVertices _ _ = False

truncEdge2 :: Box2 -> Edge2 -> Edge2
truncEdge2 box edge =
  if isIEdge edge
    then TIEdge2 (p, (p1 + factor * v1, p2 + factor * v2))
    else edge
  where
    isIEdge (IEdge2 _) = True
    isIEdge _          = False
    IEdge2 (p@(p1,p2), v@(v1,v2)) = edge
    factor = factor2 box p v

-- | clip a 2D Voronoi diagram in a bounding box
clipVoronoi2 :: Box2 -> Voronoi2 -> Voronoi2
clipVoronoi2 box = map (second (map (truncEdge2 box)))
