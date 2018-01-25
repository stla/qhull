module Voronoi.Voronoi
  where
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.Maybe
import           Data.Tuple.Extra   ((&&&))
import           Delaunay


type Point = [Double]
type Vector = [Double]
data Edge = Edge (Point, Point) | IEdge (Point, Vector)
     deriving Show
type Cell = [Edge]


factor2 :: (Double,Double,Double,Double) -> (Double,Double) -> (Double,Double) -> Double
factor2 box@(xmin, xmax, ymin, ymax) p@(p1,p2) (v1,v2)
  | v1==0 = if v2>0 then (ymax-p2)/v2 else (ymin-p2)/v2
  | v2==0 = if v1>0 then (xmax-p1)/v1 else (xmin-p1)/v1
  | otherwise = min (factor2 box p (v1,0)) (factor2 box p (0,v2))
  --  | v1>0 && v2>0 = min ((r-p1)/v1) ((t-p2)/v2)
  --  | v1>0 && v2<0 = min ((r-p1)/v1) ((b-p2)/v2)
  --  | v1<0 && v2>0 = min ((l-p1)/v1) ((t-p2)/v2)
  --  | v1<0 && v2<0 = min ((l-p1)/v1) ((b-p2)/v2)

approx :: RealFrac a => Int -> a -> a
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

---

tileFacetAsPair :: TileFacet -> (Simplex, [Int])
tileFacetAsPair = _subsimplex &&& (IS.toList . _facetOf)

edgesFromTileFacet :: Tesselation -> TileFacet -> Maybe Edge
edgesFromTileFacet tess tilefacet
  | length tileindices == 1
    = Just $ IEdge (c1, _normal simplex)
  | c1 == c2 = Nothing
  | isJust (_family tile1) && (_family tile1 == _family tile2) = Nothing
  | otherwise = Just $ Edge (c1, c2)
  where
    (simplex, tileindices) = tileFacetAsPair tilefacet
    tiles = _tiles tess
    tile1 = tiles IM.! head tileindices
    tile2 = tiles IM.! last tileindices
    c1 = _circumcenter $ _simplex tile1
    c2 = _circumcenter $ _simplex tile2

voronoiCell :: ([TileFacet] -> [TileFacet]) -> (Edge -> a) -> Tesselation
            -> Index -> [a]
voronoiCell facetsQuotienter edgeTransformer tess i =
  let tilefacets = facetsQuotienter $ vertexNeighborFacets tess i in
  map (edgeTransformer . fromJust) $
      filter isJust $ map (edgesFromTileFacet tess) tilefacets

voronoi :: (Tesselation -> Index -> a) -> Tesselation -> [([Double], a)]
voronoi cellGetter tess =
  let sites = IM.elems $ IM.map _point (_sites tess) in
    zip sites (map (cellGetter tess) [0 .. length sites -1])

voronoi' :: Tesselation -> [([Double], Cell)]
voronoi' = voronoi (voronoiCell id id)

-- | whether a Voronoi cell is bounded
boundedCell :: Cell -> Bool
boundedCell = all isEdge
  where
    isEdge (Edge _) = True
    isEdge _        = False
