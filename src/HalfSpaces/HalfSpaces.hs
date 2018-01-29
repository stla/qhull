module HalfSpaces.HalfSpaces
  where
import           Control.Monad          (unless, when, (<$!>), (=<<))
import           Foreign.C.Types
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Marshal.Array  (peekArray, pokeArray)
import           Foreign.Storable       (peek, sizeOf)
import           HalfSpaces.CHalfSpaces
import           HalfSpaces.Constraint  (Constraint)
import           HalfSpaces.Internal    (normalizeConstraints)
import           HalfSpaces.ToySolver

hsintersections' :: [[Double]]     -- halfspaces
                 -> [Double]       -- interior point
                 -> Bool           -- print to stdout
                 -> IO [[Double]]
hsintersections' halfspaces ipoint stdout = do
  let n     = length halfspaces
      dim   = length ipoint
  unless (all (== dim+1) (map length halfspaces)) $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim) $
    error "insufficient number of halfspaces"
  hsPtr <- mallocBytes (n * (dim+1) * sizeOf (undefined :: CDouble))
  pokeArray hsPtr (concatMap (map realToFrac) halfspaces)
  ipointPtr <- mallocBytes (dim * sizeOf (undefined :: CDouble))
  pokeArray ipointPtr (map realToFrac ipoint)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  nintersectionsPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_intersections hsPtr ipointPtr
               (fromIntegral dim) (fromIntegral n)
               nintersectionsPtr exitcodePtr (fromIntegral $ fromEnum stdout)
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free hsPtr
  if exitcode /= 0
    then do
      free resultPtr
      free nintersectionsPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      nintersections <- (<$!>) fromIntegral (peek nintersectionsPtr)
      result <- (<$!>) (map (map realToFrac))
                       ((=<<) (mapM (peekArray dim))
                             (peekArray nintersections resultPtr))
      free resultPtr
      free nintersectionsPtr
      return result

hsintersections :: [Constraint] -> Bool -> IO [[Double]]
hsintersections constraints stdout = do
  let halfspacesMatrix = normalizeConstraints constraints
  ipoint <- (<$!>) (map realToFrac) (interiorPoint constraints)
  hsintersections' halfspacesMatrix ipoint stdout

cubeConstraints' :: [[Double]]
cubeConstraints' = [[ 1, 0, 0,-1]
                   ,[-1, 0, 0,-1]
                   ,[ 0, 1, 0,-1]
                   ,[ 0,-1, 0,-1]
                   ,[ 0, 0, 1,-1]
                   ,[ 0, 0,-1,-1]]
