module HalfSpaces.HalfSpaces
  where
import           Control.Monad          ((<$!>), (=<<), unless, when)
import           HalfSpaces.CHalfSpaces
import           Foreign.C.Types
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Marshal.Array  (pokeArray, peekArray)
import           Foreign.Storable       (peek, sizeOf)

hsintersections :: [[Double]]     -- halfspaces
                -> [Double]       -- interior point
                -> IO [[Double]]
hsintersections halfspaces ipoint = do
  let n     = length halfspaces
      dim   = length ipoint
      check = all (== dim+1) (map length halfspaces)
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim) $
    error "insufficient number of points"
  hsPtr <- mallocBytes (n * (dim+1) * sizeOf (undefined :: CDouble))
  pokeArray hsPtr (concatMap (map realToFrac) halfspaces)
  ipointPtr <- mallocBytes (dim * sizeOf (undefined :: CDouble))
  pokeArray ipointPtr (map realToFrac ipoint)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  nintersectionsPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_intersections hsPtr ipointPtr
               (fromIntegral dim) (fromIntegral n)
               nintersectionsPtr exitcodePtr
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

cubeConstraints' :: [[Double]]
cubeConstraints' = [[ 1, 0, 0,-1]
                   ,[-1, 0, 0,-1]
                   ,[ 0, 1, 0,-1]
                   ,[ 0,-1, 0,-1]
                   ,[ 0, 0, 1,-1]
                   ,[ 0, 0,-1,-1]]
