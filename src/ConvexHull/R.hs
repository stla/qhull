module ConvexHull.R
  where
import           Control.Monad       (when)
import           ConvexHull
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict  as IM
import           Data.List.Index     (iconcatMap)
import           Data.Maybe
import           Data.Tuple.Extra    (snd3)
import           System.IO           (writeFile)

convexHull3DrglCode :: [[Double]] -> Bool -> Maybe FilePath -> IO String
convexHull3DrglCode points rainbow file = do
  -- get edges --
  hull1 <- convexHull points False False Nothing
  let edges = H.elems (_hedges hull1)
  -- get triangles --
  hull2 <- convexHull points True False Nothing
  let grpFaces = groupedFacets hull2
  let triangles = map (map IM.elems . snd3) grpFaces
  -- code for edges --
  let code1 = concatMap rglSegment edges
  -- color palette --
  let code_colors = if rainbow
                      then "colors <- rainbow(" ++ show (length grpFaces + 1) ++
                           ", alpha=0.5)\n"
                      else "colors <- rep(\"blue\", " ++
                           show (length grpFaces + 1) ++ ")\n"
  -- code for triangles --
  let code2 = iconcatMap (\i x -> concatMap (rglTriangle i) x) triangles
  -- whole code --
  let code = "library(rgl)\n" ++ code_colors ++ code1 ++ code2
  -- write file --
  when (isJust file) $
    writeFile (fromJust file) code
  -- --
  return code
  -- auxiliary functions --
  where
    asTriplet p = (p!!0, p!!1, p!!2)
    rglSegment :: ([Double], [Double]) -> String
    rglSegment (p1', p2') =
      let p1 = asTriplet p1' in
      let p2 = asTriplet p2' in
      "segments3d(rbind(c" ++ show p1 ++ ", c" ++ show p2 ++
        "), color=\"black\")\n"
    rglTriangle :: Int -> [[Double]] -> String
    rglTriangle i threepoints =
      "triangles3d(rbind(c" ++ show p1 ++ ", c" ++ show p2 ++
      ", c" ++ show p3 ++ "), color=colors[" ++ show (i+1) ++ "]" ++
      ", alpha=0.75)\n"
      where
        p1 = asTriplet $ threepoints!!0
        p2 = asTriplet $ threepoints!!1
        p3 = asTriplet $ threepoints!!2
