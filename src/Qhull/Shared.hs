module Qhull.Shared
  where
import qualified Data.HashMap.Strict.InsOrd as H
import           Data.Maybe
import           Qhull.Types

-- | whether a pair of vertices indices form an edge
isEdge :: HasEdges a => a -> (Index, Index) -> Bool
isEdge x (i,j) = Pair i j `H.member` _edges x

-- | edge as pair of points
toPoints :: HasEdges a => a -> (Index, Index) -> Maybe ([Double], [Double])
toPoints x (i,j) = H.lookup (Pair i j) (_edges x)

-- | edge as pair of points, without checking the edge exists
toPoints' :: HasEdges a => a -> (Index, Index) -> ([Double], [Double])
toPoints' x (i,j) = fromJust $ toPoints x (i,j)
