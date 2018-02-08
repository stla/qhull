{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( cConvexHullToConvexHull
  , c_convexhull )
  where
import           Control.Monad       ((<$!>), (=<<))
import           ConvexHull.Types
import           Data.IntMap.Strict  (IntMap, fromAscList)
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS
import           Data.List
import qualified Data.HashMap.Strict.InsOrd as H
import           Data.Tuple.Extra    (both)
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Qhull.Types

#include "convexhull.h"

data CVertex = CVertex {
    __id :: CUInt
  , __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = #{size VertexT}
    alignment __ = #{alignment VertexT}
    peek ptr = do
      id'     <- #{peek VertexT, id} ptr
      point'  <- #{peek VertexT, point} ptr
      return CVertex { __id = id'
                     , __point = point' }
    poke ptr (CVertex r1 r2)
      = do
          #{poke VertexT, id} ptr r1
          #{poke VertexT, point} ptr r2

--data Vertex = Vertex {
--    _id :: Int
--  , _point :: [Double]
--} deriving Show

cVerticesToMap :: Int -> [CVertex] -> IO (IntMap [Double])
cVerticesToMap dim cvertices = do
  let ids = map (fromIntegral . __id) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point cv)))
                 cvertices
  return $ fromAscList (zip ids points)

data CVertex' = CVertex' {
    __id' :: CUInt
  , __point' :: Ptr CDouble
  , __neighfacets :: Ptr CUInt
  , __nneighfacets :: CUInt
  , __neighvertices :: Ptr CUInt
  , __nneighvertices :: CUInt
  , __neighridges :: Ptr CUInt
  , __nneighridges :: CUInt
}

instance Storable CVertex' where
    sizeOf    __ = #{size FullVertexT}
    alignment __ = #{alignment FullVertexT}
    peek ptr = do
      id'              <- #{peek FullVertexT, id} ptr
      point'           <- #{peek FullVertexT, point} ptr
      neighfacets'     <- #{peek FullVertexT, neighfacets} ptr
      nneighfacets'    <- #{peek FullVertexT, nneighfacets} ptr
      neighvertices'   <- #{peek FullVertexT, neighvertices} ptr
      nneighsvertices' <- #{peek FullVertexT, nneighsvertices} ptr
      neighridges'     <- #{peek FullVertexT, neighridges} ptr
      nneighridges'    <- #{peek FullVertexT, nneighridges} ptr
      return CVertex' { __id'            = id'
                      , __point'         = point'
                      , __neighfacets    = neighfacets'
                      , __nneighfacets   = nneighfacets'
                      , __neighvertices  = neighvertices'
                      , __nneighvertices = nneighsvertices'
                      , __neighridges    = neighridges'
                      , __nneighridges   = nneighridges'
                      }
    poke ptr (CVertex' r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          #{poke FullVertexT, id} ptr r1
          #{poke FullVertexT, point} ptr r2
          #{poke FullVertexT, neighfacets} ptr r3
          #{poke FullVertexT, nneighfacets} ptr r4
          #{poke FullVertexT, neighvertices} ptr r5
          #{poke FullVertexT, nneighsvertices} ptr r6
          #{poke FullVertexT, neighridges} ptr r7
          #{poke FullVertexT, nneighridges} ptr r8

cVerticesToVertexMap :: Int -> [CVertex'] -> IO (IntMap Vertex)
cVerticesToVertexMap dim cvertices = do
  let ids             = map (fromIntegral . __id') cvertices
      nneighfacets    = map (fromIntegral . __nneighfacets) cvertices
      nneighsvertices = map (fromIntegral . __nneighvertices) cvertices
      nneighridges    = map (fromIntegral . __nneighridges) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point' cv)))
                 cvertices
  neighfacets <- mapM (\(i, cv) -> (<$!>) (map fromIntegral)
                                          (peekArray i (__neighfacets cv)))
                       (zip nneighfacets cvertices)
  neighvertices <- mapM (\(i, cv) ->
                          (<$!>) (map fromIntegral)
                                 (peekArray i (__neighvertices cv)))
                        (zip nneighsvertices cvertices)
  neighridges <- mapM (\(i, cv) ->
                       (<$!>) (map fromIntegral)
                              (peekArray i (__neighridges cv)))
                     (zip nneighridges cvertices)
  return $ IM.fromList
           (zip ids (map (\(pt, fneighs, vneighs, eneighs) ->
                          Vertex { _point         = pt
                                 , _neighfacets   = IS.fromAscList fneighs
                                 , _neighvertices = IS.fromAscList vneighs
                                 , _neighridges   = IS.fromAscList eneighs })
                          (zip4 points neighfacets neighvertices neighridges)))

data CRidge = CRidge {
    __rvertices :: Ptr CVertex
  , __ridgeOf1 :: CUInt
  , __ridgeOf2 :: CUInt
  , __ridgeSize :: CUInt
  , __ridgeid   :: CUInt
  , __redges :: Ptr (Ptr CUInt)
  , __nredges :: CUInt
}

instance Storable CRidge where
    sizeOf    __ = #{size RidgeT}
    alignment __ = #{alignment RidgeT}
    peek ptr = do
      rvertices <- #{peek RidgeT, vertices} ptr
      ridgeOf1' <- #{peek RidgeT, ridgeOf1} ptr
      ridgeOf2' <- #{peek RidgeT, ridgeOf2} ptr
      ridgeSize <- #{peek RidgeT, nvertices} ptr
      ridgeid   <- #{peek RidgeT, id} ptr
      edges'    <- #{peek RidgeT, edges} ptr
      nedges'   <- #{peek RidgeT, nedges} ptr
      return CRidge { __rvertices = rvertices
                    , __ridgeOf1  = ridgeOf1'
                    , __ridgeOf2  = ridgeOf2'
                    , __ridgeSize = ridgeSize
                    , __ridgeid   = ridgeid
                    , __redges    = edges'
                    , __nredges   = nedges' }
    poke ptr (CRidge r1 r2 r3 r4 r5 r6 r7)
      = do
          #{poke RidgeT, vertices} ptr r1
          #{poke RidgeT, ridgeOf1} ptr r2
          #{poke RidgeT, ridgeOf2} ptr r3
          #{poke RidgeT, nvertices} ptr r4
          #{poke RidgeT, id} ptr r5
          #{poke RidgeT, edges} ptr r6
          #{poke RidgeT, nedges} ptr r7

cRidgeToRidge :: Int -> CRidge -> IO (Int, Ridge)
cRidgeToRidge dim cridge = do
  let f1     = fromIntegral $ __ridgeOf1 cridge
      f2     = fromIntegral $ __ridgeOf2 cridge
      n      = fromIntegral $ __ridgeSize cridge
      rid    = fromIntegral $ __ridgeid cridge
      nedges = fromIntegral $ __nredges cridge
  vertices <- peekArray n (__rvertices cridge)
  rvertices <- cVerticesToMap dim vertices
  edges' <- if dim > 3
            then (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                        ((=<<) (mapM (peekArray 2))
                               (peekArray nedges (__redges cridge)))
            else return []
  let edges = if dim > 3
              then H.fromList (zip (map (\(i,j) -> Pair i j) edges')
                                   (map (both ((IM.!) rvertices)) edges'))
              else H.empty
  return (rid, Ridge { _rvertices = rvertices
                     , _ridgeOf   = IS.fromAscList [f1,f2]
                     , _redges    = edges })

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __nvertices' :: CUInt
  , __ridges :: Ptr CUInt
  , __nridges' :: CUInt
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble
  , __offset :: CDouble
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
  , __family :: CInt
  , __edges :: Ptr (Ptr CUInt)
  , __nedges :: CUInt
}

instance Storable CFace where
    sizeOf    __ = #{size FaceT}
    alignment __ = #{alignment FaceT}
    peek ptr = do
      fvertices' <- #{peek FaceT, vertices} ptr
      nvertices' <- #{peek FaceT, nvertices} ptr
      ridges'    <- #{peek FaceT, ridgesids} ptr
      nridges'   <- #{peek FaceT, nridges} ptr
      center'    <- #{peek FaceT, center} ptr
      normal'    <- #{peek FaceT, normal} ptr
      offset'    <- #{peek FaceT, offset} ptr
      area'      <- #{peek FaceT, area} ptr
      neighbors' <- #{peek FaceT, neighbors} ptr
      neighsize  <- #{peek FaceT, neighborsize} ptr
      family'    <- #{peek FaceT, family} ptr
      edges'     <- #{peek FaceT, edges} ptr
      nedges'    <- #{peek FaceT, nedges} ptr
      return CFace { __fvertices    = fvertices'
                   , __nvertices'   = nvertices'
                   , __ridges       = ridges'
                   , __nridges'     = nridges'
                   , __center       = center'
                   , __normal       = normal'
                   , __offset       = offset'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize
                   , __family       = family'
                   , __edges        = edges'
                   , __nedges       = nedges'
                 }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13)
      = do
          #{poke FaceT, vertices} ptr r1
          #{poke FaceT, nvertices} ptr r2
          #{poke FaceT, ridgesids} ptr r3
          #{poke FaceT, nridges} ptr r4
          #{poke FaceT, center} ptr r5
          #{poke FaceT, normal} ptr r6
          #{poke FaceT, offset} ptr r7
          #{poke FaceT, area} ptr r8
          #{poke FaceT, neighbors} ptr r9
          #{poke FaceT, neighborsize} ptr r10
          #{poke FaceT, family} ptr r11
          #{poke FaceT, edges} ptr r12
          #{poke FaceT, nedges} ptr r13

cFaceToFacet :: Int -> CFace -> IO Facet
cFaceToFacet dim cface = do
  let area      = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
      offset    = realToFrac (__offset cface)
      family    = fromIntegral (__family cface)
      nridges   = fromIntegral (__nridges' cface)
      nvertices = fromIntegral (__nvertices' cface)
      nedges    = fromIntegral (__nedges cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (cVerticesToMap dim)
                     (peekArray nvertices (__fvertices cface))
  ridges  <- (<$!>) (map fromIntegral)
                    (peekArray nridges (__ridges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  edges' <- (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                      ((=<<) (mapM (peekArray 2))
                             (peekArray nedges (__edges cface)))
  let edges = H.fromList
              (zip (map (\(i,j) -> Pair i j) edges')
                   (map (both ((IM.!) vertices)) edges'))
  return Facet { _fvertices = vertices
               , _fridges   = IS.fromAscList ridges
               , _centroid  = center
               , _normal'   = normal
               , _offset'   = offset
               , _area      = area
               , _neighbors = IS.fromAscList neighbors
               , _family'   = if family == -1 then None else Family family
               , _fedges    = edges }

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __allvertices :: Ptr CVertex'
  , __nvertices :: CUInt
  , __faces :: Ptr CFace
  , __nfaces :: CUInt
  , __allridges :: Ptr CRidge
  , __nridges :: CUInt
  , __alledges :: Ptr (Ptr CUInt)
  , __nalledges :: CUInt
}

instance Storable CConvexHull where
    sizeOf    __ = #{size ConvexHullT}
    alignment __ = #{alignment ConvexHullT}
    peek ptr = do
      dim'         <- #{peek ConvexHullT, dim} ptr
      vertices'    <- #{peek ConvexHullT, vertices} ptr
      nvertices'   <- #{peek ConvexHullT, nvertices} ptr
      faces'       <- #{peek ConvexHullT, faces} ptr
      nfaces'      <- #{peek ConvexHullT, nfaces} ptr
      allridges'   <- #{peek ConvexHullT, ridges} ptr
      nridges'     <- #{peek ConvexHullT, nridges} ptr
      alledges'    <- #{peek ConvexHullT, edges} ptr
      nedges'      <- #{peek ConvexHullT, nedges} ptr
      return CConvexHull { __dim         = dim'
                         , __allvertices = vertices'
                         , __nvertices   = nvertices'
                         , __faces       = faces'
                         , __nfaces      = nfaces'
                         , __allridges   = allridges'
                         , __nridges     = nridges'
                         , __alledges    = alledges'
                         , __nalledges   = nedges'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6 r7 r8 r9)
      = do
          #{poke ConvexHullT, dim} ptr r1
          #{poke ConvexHullT, vertices} ptr r2
          #{poke ConvexHullT, nvertices} ptr r3
          #{poke ConvexHullT, faces} ptr r4
          #{poke ConvexHullT, nfaces} ptr r5
          #{poke ConvexHullT, ridges} ptr r6
          #{poke ConvexHullT, nridges} ptr r7
          #{poke ConvexHullT, edges} ptr r8
          #{poke ConvexHullT, nedges} ptr r9

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> CUInt -- print to stdout
  -> CString -- summary file
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

cConvexHullToConvexHull :: CConvexHull -> IO ConvexHull
cConvexHullToConvexHull cconvexhull = do
  let dim       = fromIntegral (__dim cconvexhull)
      nvertices = fromIntegral (__nvertices cconvexhull)
      nfaces    = fromIntegral (__nfaces cconvexhull)
      nridges   = fromIntegral (__nridges cconvexhull)
      nedges    = fromIntegral (__nalledges cconvexhull)
  vertices <- (=<<) (cVerticesToVertexMap dim)
                    (peekArray nvertices (__allvertices cconvexhull))
  faces <- (=<<) (mapM (cFaceToFacet dim))
                       (peekArray nfaces (__faces cconvexhull))
  allridges <- (=<<) (mapM (cRidgeToRidge dim))
                          (peekArray nridges (__allridges cconvexhull))
  alledges' <- (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                      ((=<<) (mapM (peekArray 2))
                             (peekArray nedges (__alledges cconvexhull)))
  let alledges = let points = IM.map _point vertices in
                  H.fromList
                  (zip (map (\(i,j) -> Pair i j) alledges')
                       (map (both ((IM.!) points)) alledges'))
  return ConvexHull {
                        _hvertices = vertices
                      , _hfacets   = fromAscList (zip [0 .. nfaces-1] faces)
                      , _hridges   = fromAscList allridges
                      , _hedges    = alledges
                    }
