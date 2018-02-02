module Qhull.Shared
  where
import qualified Data.HashMap.Strict.InsOrd as H
import qualified Data.IntMap.Strict         as IM
import           Data.Maybe
import           Qhull.Types

-- | whether two families are the same
sameFamily :: Family -> Family -> Bool
sameFamily (Family i) (Family j) = i == j
sameFamily _ _ = False

-- | vertices ids
verticesIds :: HasVertices a => a -> [Index]
verticesIds = IM.keys . _vertices

-- | vertices coordinates
verticesCoordinates :: HasVertices a => a -> [[Double]]
verticesCoordinates = IM.elems . _vertices

-- | number of vertices
nVertices :: HasVertices a => a -> Int
nVertices = IM.size . _vertices

-- | edges ids
edgesIds :: HasEdges a => a -> [IndexPair]
edgesIds = H.keys . _edges

-- | edges ids as pairs of integers
edgesIds' :: HasEdges a => a -> [(Index,Index)]
edgesIds' x = map fromPair (edgesIds x)
  where
    fromPair (Pair i j) = (i,j)

-- | edges coordinates
edgesCoordinates :: HasEdges a => a -> [([Double],[Double])]
edgesCoordinates = H.elems . _edges

-- | number of edges
nEdges :: HasEdges a => a -> Int
nEdges = H.size . _edges

-- | whether a pair of vertices indices form an edge;
-- the order of the indices has no importance
isEdge :: HasEdges a => a -> (Index, Index) -> Bool
isEdge x (i,j) = Pair i j `H.member` _edges x

-- | edge as pair of points; the order of the vertices has no importance
toPoints :: HasEdges a => a -> (Index, Index) -> Maybe ([Double], [Double])
toPoints x (i,j) = H.lookup (Pair i j) (_edges x)

-- | edge as pair of points, without checking the edge exists
toPoints' :: HasEdges a => a -> (Index, Index) -> ([Double], [Double])
toPoints' x (i,j) = fromJust $ toPoints x (i,j)
