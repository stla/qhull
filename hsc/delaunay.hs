{-# LINE 1 "delaunay.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay.CDelaunay
  ( cTesselationToTesselation
  , c_tesselation )
  where
import           Control.Monad       ((<$!>))
import           Delaunay.Types
import           Data.IntMap.Strict  (fromAscList)
import qualified Data.IntSet         as IS
-- import qualified Data.HashMap.Strict as H
import           Foreign
import           Foreign.C.Types
import           Qhull.Types (Family(..))



data CSite = CSite {
    __id :: CUInt
  , __neighsites :: Ptr CUInt
  , __nneighsites :: CUInt
  , __neighridgesids :: Ptr CUInt
  , __nneighridges :: CUInt
  , __neightiles :: Ptr CUInt
  , __nneightiles :: CUInt
}

instance Storable CSite where
    sizeOf    __ = (56)
{-# LINE 29 "delaunay.hsc" #-}
    alignment __ = 8
{-# LINE 30 "delaunay.hsc" #-}
    peek ptr = do
      id'              <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 32 "delaunay.hsc" #-}
      neighsites'      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 33 "delaunay.hsc" #-}
      nneighsites'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 34 "delaunay.hsc" #-}
      neighridgesids'  <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 35 "delaunay.hsc" #-}
      nneighridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 36 "delaunay.hsc" #-}
      neightiles'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 37 "delaunay.hsc" #-}
      nneightiles'     <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 38 "delaunay.hsc" #-}
      return CSite { __id             = id'
                   , __neighsites     = neighsites'
                   , __nneighsites    = nneighsites'
                   , __neighridgesids = neighridgesids'
                   , __nneighridges   = nneighridges'
                   , __neightiles     = neightiles'
                   , __nneightiles    = nneightiles'
                  }
    poke ptr (CSite r1 r2 r3 r4 r5 r6 r7)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 49 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 50 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 51 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 52 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 53 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
{-# LINE 54 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
{-# LINE 55 "delaunay.hsc" #-}

cSiteToSite :: [[Double]] -> CSite -> IO (Int, Site)
cSiteToSite sites csite = do
  let id'          = fromIntegral $ __id csite
      nneighsites  = fromIntegral $ __nneighsites csite
      nneighridges = fromIntegral $ __nneighridges csite
      nneightiles  = fromIntegral $ __nneightiles csite
      point        = sites !! id'
  neighsites <- (<$!>) (map fromIntegral)
                       (peekArray nneighsites (__neighsites csite))
  neighridges <- (<$!>) (map fromIntegral)
                        (peekArray nneighridges (__neighridgesids csite))
  neightiles <- (<$!>) (map fromIntegral)
                       (peekArray nneightiles (__neightiles csite))
  return (id', Site {
                      _point          = point
                    , _neighsitesIds  = IS.fromAscList neighsites
                    , _neighfacetsIds = IS.fromAscList neighridges
                    , _neightilesIds  = IS.fromAscList neightiles
                  })

data CSimplex = CSimplex {
    __sitesids :: Ptr CUInt
  , __center :: Ptr CDouble
  , __radius :: CDouble
  , __volume :: CDouble
}

instance Storable CSimplex where
    sizeOf    __ = (32)
{-# LINE 85 "delaunay.hsc" #-}
    alignment __ = 8
{-# LINE 86 "delaunay.hsc" #-}
    peek ptr = do
      sitesids'    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 88 "delaunay.hsc" #-}
      center'      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 89 "delaunay.hsc" #-}
      radius'      <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 90 "delaunay.hsc" #-}
      volume'      <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 91 "delaunay.hsc" #-}
      return CSimplex { __sitesids    = sitesids'
                      , __center      = center'
                      , __radius      = radius'
                      , __volume      = volume'
                    }
    poke ptr (CSimplex r1 r2 r3 r4)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 99 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 100 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 101 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 102 "delaunay.hsc" #-}

cSimplexToSimplex :: [[Double]] -> Int -> CSimplex -> IO Simplex
cSimplexToSimplex sites simplexdim csimplex = do
  let radius      = cdbl2dbl $ __radius csimplex
      volume      = cdbl2dbl $ __volume csimplex
      dim         = length (head sites)
  sitesids <- (<$!>) (map fromIntegral)
                     (peekArray simplexdim (__sitesids csimplex))
  let points = fromAscList
               (zip sitesids (map ((!!) sites) sitesids))
  center <- (<$!>) (map cdbl2dbl) (peekArray dim (__center csimplex))
  return Simplex { _points       = points
                 , _circumcenter = center
                 , _circumradius = radius
                 , _volume       = volume }
  where
    cdbl2dbl :: CDouble -> Double
    cdbl2dbl x = if isNaN x then 0/0 else realToFrac x

data CSubTile = CSubTile {
    __id' :: CUInt
  , __subsimplex :: CSimplex
  , __ridgeOf1 :: CUInt
  , __ridgeOf2 :: CInt
  , __normal :: Ptr CDouble
  , __offset :: CDouble
}

instance Storable CSubTile where
    sizeOf    __ = (72)
{-# LINE 132 "delaunay.hsc" #-}
    alignment __ = 8
{-# LINE 133 "delaunay.hsc" #-}
    peek ptr = do
      id'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 135 "delaunay.hsc" #-}
      simplex'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 136 "delaunay.hsc" #-}
      ridgeOf1' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 137 "delaunay.hsc" #-}
      ridgeOf2' <- (\hsc_ptr -> peekByteOff hsc_ptr 44) ptr
{-# LINE 138 "delaunay.hsc" #-}
      normal'   <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 139 "delaunay.hsc" #-}
      offset'   <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 140 "delaunay.hsc" #-}
      return CSubTile { __id'        = id'
                      , __subsimplex = simplex'
                      , __ridgeOf1   = ridgeOf1'
                      , __ridgeOf2   = ridgeOf2'
                      , __normal     = normal'
                      , __offset     = offset' }
    poke ptr (CSubTile r1 r2 r3 r4 r5 r6)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 149 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 150 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r3
{-# LINE 151 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 44) ptr r4
{-# LINE 152 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r5
{-# LINE 153 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r6
{-# LINE 154 "delaunay.hsc" #-}

cSubTiletoTileFacet :: [[Double]] -> CSubTile -> IO (Int, TileFacet)
cSubTiletoTileFacet points csubtile = do
  let dim        = length (head points)
      ridgeOf1   = fromIntegral $ __ridgeOf1 csubtile
      ridgeOf2   = fromIntegral $ __ridgeOf2 csubtile
      ridgeOf    = if ridgeOf2 == -1 then [ridgeOf1] else [ridgeOf1, ridgeOf2]
      id'        = fromIntegral $ __id' csubtile
      subsimplex = __subsimplex csubtile
      offset     = __offset csubtile
  simplex <- cSimplexToSimplex points dim subsimplex
  normal <- <$!> (map realToFrac) (peekArray dim (__normal csubtile))
  return (id', TileFacet { _subsimplex = simplex
                         , _facetOf    = IS.fromAscList ridgeOf
                         , _normal     = normal
                         , _offset     = offset })

data CTile = CTile {
    __id'' :: CUInt
  , __simplex :: CSimplex
  , __neighbors :: Ptr CUInt
  , __nneighbors :: CUInt
  , __ridgesids :: Ptr CUInt
  , __nridges :: CUInt
  , __family :: CInt
  , __orientation :: CInt
}

instance Storable CTile where
    sizeOf    __ = (80)
{-# LINE 184 "delaunay.hsc" #-}
    alignment __ = 8
{-# LINE 185 "delaunay.hsc" #-}
    peek ptr = do
      id'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 187 "delaunay.hsc" #-}
      simplex'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 188 "delaunay.hsc" #-}
      neighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 189 "delaunay.hsc" #-}
      nneighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 190 "delaunay.hsc" #-}
      ridgesids'  <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 191 "delaunay.hsc" #-}
      nridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 192 "delaunay.hsc" #-}
      family'     <- (\hsc_ptr -> peekByteOff hsc_ptr 68) ptr
{-# LINE 193 "delaunay.hsc" #-}
      orient      <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 194 "delaunay.hsc" #-}
      return CTile { __id''        = id'
                   , __simplex     = simplex'
                   , __neighbors   = neighbors'
                   , __nneighbors  = nneighbors'
                   , __ridgesids   = ridgesids'
                   , __nridges     = nridges'
                   , __family      = family'
                   , __orientation = orient
                  }
    poke ptr (CTile r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 206 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 207 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r3
{-# LINE 208 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r4
{-# LINE 209 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r5
{-# LINE 210 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr r6
{-# LINE 211 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 68) ptr r7
{-# LINE 212 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr r8
{-# LINE 213 "delaunay.hsc" #-}

cTileToTile :: [[Double]] -> CTile -> IO (Int, Tile)
cTileToTile points ctile = do
  let id'        = fromIntegral $ __id'' ctile
      csimplex   = __simplex ctile
      nneighbors = fromIntegral $ __nneighbors ctile
      nridges    = fromIntegral $ __nridges ctile
      family     = __family ctile
      orient     = __orientation ctile
      dim        = length (head points)
  simplex <- cSimplexToSimplex points (dim+1) csimplex
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray nneighbors (__neighbors ctile))
  ridgesids <- (<$!>) (map fromIntegral)
                      (peekArray nridges (__ridgesids ctile))
  return (id', Tile {  _simplex      = simplex
                     , _neighborsIds = IS.fromAscList neighbors
                     , _facetsIds    = IS.fromAscList ridgesids
                     , _family       = if family == -1
                                        then None
                                        else Family (fromIntegral family)
                     , _toporiented  = orient == 1 })

data CTesselation = CTesselation {
    __sites :: Ptr CSite
  , __tiles :: Ptr CTile
  , __ntiles :: CUInt
  , __subtiles :: Ptr CSubTile
  , __nsubtiles :: CUInt
}

instance Storable CTesselation where
    sizeOf    __ = (40)
{-# LINE 246 "delaunay.hsc" #-}
    alignment __ = 8
{-# LINE 247 "delaunay.hsc" #-}
    peek ptr = do
      sites'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 249 "delaunay.hsc" #-}
      tiles'     <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 250 "delaunay.hsc" #-}
      ntiles'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 251 "delaunay.hsc" #-}
      subtiles'  <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 252 "delaunay.hsc" #-}
      nsubtiles' <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 253 "delaunay.hsc" #-}
      return CTesselation {
                     __sites     = sites'
                   , __tiles     = tiles'
                   , __ntiles    = ntiles'
                   , __subtiles  = subtiles'
                   , __nsubtiles = nsubtiles'
                  }
    poke ptr (CTesselation r1 r2 r3 r4 r5)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 263 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 264 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 265 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 266 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 267 "delaunay.hsc" #-}

foreign import ccall unsafe "tesselation" c_tesselation
  :: Ptr CDouble -- sites
  -> CUInt       -- dim
  -> CUInt       -- nsites
  -> CUInt       -- 0/1, point at infinity
  -> CUInt       -- 0/1, include degenerate
  -> Ptr CUInt   -- exitcode
  -> IO (Ptr CTesselation)

cTesselationToTesselation :: [[Double]] -> CTesselation -> IO Tesselation
cTesselationToTesselation sites ctess = do
  let ntiles    = fromIntegral $ __ntiles ctess
      nsubtiles = fromIntegral $ __nsubtiles ctess
      nsites    = length sites
  sites''    <- peekArray nsites (__sites ctess)
  tiles''    <- peekArray ntiles (__tiles ctess)
  subtiles'' <- peekArray nsubtiles (__subtiles ctess)
  sites'     <- mapM (cSiteToSite sites) sites''
  tiles'     <- mapM (cTileToTile sites) tiles''
  subtiles'  <- mapM (cSubTiletoTileFacet sites) subtiles''
  return Tesselation
         { _sites      = fromAscList sites'
         , _tiles      = fromAscList tiles'
         , _tilefacets = fromAscList subtiles' }
