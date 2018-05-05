module Adjacency.Read
  where
import Text.Printf
import System.IO


-- vvvv :: IO [String]
-- vvvv = do
--   vvv' <- vvv
--   return $ words $ printf "\\[%s\\]" <$> vvv'

vv :: IO String
vv = readFile "v.txt"

vvv :: IO [String]
vvv = do
  vv' <- vv
  return $ lines vv'

saveArr = do
    outh <- openFile "test.txt" WriteMode
    hPrint outh [1,2,3]
    hClose outh

saveArr2 = do
    outh <- openFile "test.txt" WriteMode
    hPrint outh [[1,2,3], [4,5,6]]
    hClose outh
