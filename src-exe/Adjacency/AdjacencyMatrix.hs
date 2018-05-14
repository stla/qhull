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

adjacency :: Tesselation -> Int -> [Int]
adjacency tess i =  map (fromEnum . ((i `IS.member`) . _neighsitesIds) . snd) $ IM.toList $ _sites tess

adjacency' :: Tesselation -> [[Int]]
adjacency' tess = map (adjacency tess) (IM.keys ( _sites tess ))

data Arguments = Arguments { infile :: FilePath, outfile :: FilePath }

run :: Parser Arguments
run = Arguments
     <$> argument str
           ( metavar "INFILE"
          <> help "File of vertices" )
      <*> argument str
           ( metavar "OUTFILE"
          <> help "adjacency matrix" )

writeMatrix :: [[Int]] -> FilePath -> IO ()
writeMatrix matrix outfile = writeFile outfile (intercalate "\n" (map show matrix))

doMatrix :: Arguments -> IO ()
doMatrix (Arguments infile outfile) =
  do
    v <- delaunayVertices infile
    tess <- delaunayTess v
    let mat = adjacency' tess
    writeMatrix mat outfile

main :: IO ()
main = execParser opts >>= doMatrix
  where
    opts = info (helper <*> run)
      ( fullDesc
     <> progDesc "Adjacency matrix of a Delaunay tesselation"
     <> header "adjacencymatrix -- based on qhull" )
