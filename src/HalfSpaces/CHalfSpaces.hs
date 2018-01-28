{-# LANGUAGE ForeignFunctionInterface #-}
module HalfSpaces.CHalfSpaces
  where
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "intersections" c_intersections
  :: Ptr CDouble  -- halfspaces
  -> Ptr CDouble  -- interior point
  -> CUInt        -- dim
  -> CUInt        -- n halfspaces
  -> Ptr CUInt    -- n intersections
  -> Ptr CUInt    -- exitcode
  -> CUInt        -- 0/1 print to stdout
  -> IO (Ptr (Ptr CDouble))
