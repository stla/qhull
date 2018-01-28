{-# LANGUAGE ForeignFunctionInterface #-}
module HalfSpaces.CHalfSpaces
  where
import           Foreign
import           Foreign.C.Types

foreign import ccall unsafe "intersections" c_intersections
  :: Ptr CDouble
  -> Ptr CDouble
  -> CUInt
  -> CUInt
  -> Ptr CUInt
  -> Ptr CUInt
  -> IO (Ptr (Ptr CDouble))
