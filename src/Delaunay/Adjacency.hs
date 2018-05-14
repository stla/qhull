module Delaunay.Adjacency
  where
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Delaunay.Types


vertices :: [[Double]]
vertices = [
            [ -5, -5,  16 ]  -- 0
          , [ -5,  8,   3 ]  -- 1
          , [  4, -1,   3 ]  -- 2
          , [  4, -5,   7 ]  -- 3
          , [  4, -1, -10 ]  -- 4
          , [  4, -5, -10 ]  -- 5
          , [ -5,  8, -10 ]  -- 6
          , [ -5, -5, -10 ]  -- 7
                           ]


adjMatrix :: Tesselation -> [[Int]]
adjMatrix tess = map (adjacency tess) (IM.keys ( _sites tess ))
  where
  adjacency tess i =
    map (fromEnum . ((i `IS.member`) . _neighsitesIds) . snd) $ IM.toList $ _sites tess
