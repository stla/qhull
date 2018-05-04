module Delaunay.Adjacecny
  where
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS
import           Data.List.Unique      (allUnique)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Delaunay.Types
import Delaunay

-- ad :: IntMap Site -> Int -> Bool
-- ad site i = i `IM.member` site
-- -- adjacency :: IntMap Site -> Int -> Bool
-- -- adjacency site i = map (`elem` IM.keys site) (_neighsitesIds (site IM.! i))
-- add :: IntMap Site -> Int -> Bool
-- add site i = ad site (IM.keys site !! i)
--
-- adj :: IntMap Site -> [Bool]
-- adj site = map (add site) (IM.keys site)
--
-- adj' :: IntMap Site -> Int -> [Bool]
-- adj' site i = map (add site) (IS.toList (_neighsitesIds (site IM.! i)))
--
-- adja :: [IntMap Site] -> [[Bool]]
-- adja = map adj

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

del :: IO Tesselation
del = delaunay vertices False False Nothing

adjac :: Tesselation -> Int -> [Bool]
adjac tess i = map (((i `IS.member`) . _neighsitesIds) . snd) $ IM.toList $ _sites tess

adjace :: Tesselation -> [[Bool]]
adjace tess = map (adjac tess) (IM.keys ( _sites tess ))
