module Delaunay.Adjacecny
  where
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.List
import           Data.List.Unique   (allUnique)
import qualified Data.Map.Strict    as M
import           Data.Maybe
import           Delaunay.Delaunay
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

del :: IO Tesselation
del = delaunay vertices False False Nothing

adjac :: Tesselation -> Int -> [Int]
adjac tess i = map (fromEnum) $ map (((i `IS.member`) . _neighsitesIds) . snd) $ IM.toList $ _sites tess

adjace :: Tesselation -> [[Int]]
adjace tess = map (adjac tess) (IM.keys ( _sites tess ))
-- writeAdj :: Tesselation ->  IO FilePath
-- writeAdj = do
--   tabl <- adjace tess

writeMatrix :: [[Int]] -> IO ()
writeMatrix matrix = appendFile "matrix.txt" (intercalate "\n" (map show matrix))

vv :: IO String
vv = readFile "v.txt"

vvv :: IO [String]
vvv = do
  vv' <- vv
  return $ lines vv'
