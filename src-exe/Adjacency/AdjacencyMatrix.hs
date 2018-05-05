module Adjacency.AdjacencyMatrix
  where
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.List
import           Data.Monoid         ((<>))
import           Delaunay.Delaunay
import           Delaunay.Types
import           Options.Applicative

delaunayTess :: [[Double]] -> IO Tesselation
delaunayTess vertices = delaunay vertices False False Nothing

delaunayVertices :: FilePath -> IO [[Double]]
delaunayVertices verts = do
  v <- readFile verts
  return $ read v :: IO [[Double]]

adjac :: Tesselation -> Int -> [Int]
adjac tess i = map (fromEnum) $ map (((i `IS.member`) . _neighsitesIds) . snd) $ IM.toList $ _sites tess

adjace :: Tesselation -> [[Int]]
adjace tess = map (adjac tess) (IM.keys ( _sites tess ))

data Arguments = Arguments { file :: FilePath }

run :: Parser Arguments
run = Arguments
     <$> argument str
          ( metavar "FILE"
         <> help "File of vertices" )

writeMatrix :: [[Int]] -> IO ()
writeMatrix matrix = appendFile "matrix.txt" (intercalate "\n" (map show matrix))

doMatrix :: Arguments -> IO ()
doMatrix (Arguments file) =
  do
    v <- delaunayVertices file
    tess <- delaunayTess v
    let mat = adjace tess
    writeMatrix mat

main :: IO ()
main = execParser opts >>= doMatrix
  where
    opts = info (helper <*> run)
      ( fullDesc
     <> progDesc "Adjacecncy matrix of a Delaunay tesselation"
     <> header "dadjacecny -- based on qhull" )
