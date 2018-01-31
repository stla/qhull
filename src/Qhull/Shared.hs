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
verticesIds x = IM.keys (_vertices x)

-- | vertices coordinates
verticesCoordinates :: HasVertices a => a -> [[Double]]
verticesCoordinates x = IM.elems (_vertices x)

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
