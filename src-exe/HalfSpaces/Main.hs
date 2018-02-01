module Main
  where
import           HalfSpaces
import           Text.Show.Pretty

main :: IO ()
main = do
  is <- hsintersections region3D False
  pPrint is
