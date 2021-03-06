module ConvexHull.Examples
  where
import           Data.List                  hiding (permutations)
import           Data.List.Split            (chunksOf)
import           Math.Combinat.Permutations
import           System.Random

sixhundredCell :: [[Double]]
sixhundredCell =
  [[i, j, k, l] | i <- pm, j <- pm, k <- pm, l <-pm] ++
  [[0, 0, 0, i*2] | i <- pm] ++
  [[0, 0, i*2, 0] | i <- pm] ++
  [[0, i*2, 0, 0] | i <- pm] ++
  [[i*2, 0, 0, 0] | i <- pm] ++
  [ permuteList p [i*phi, j, k/phi, 0] |
    p <- permutations4, isEvenPermutation p, i <- pm, j <- pm, k <- pm] 
  where
    permutations4 = permutations 4
    phi = (1 + sqrt 5) / 2
    pm = [-1,1]

hexadecachoron :: [[Double]]
hexadecachoron =
  [ [1,0,0,0]
  , [-1,0,0,0]
  , [0,1,0,0]
  , [0,-1,0,0]
  , [0,0,1,0]
  , [0,0,-1,0]
  , [0,0,0,1]
  , [0,0,0,-1] ]

runcitruncated5cell :: [[Double]]
runcitruncated5cell =
  [[2/sqrt 10, -2/sqrt 6, 1/sqrt 3, 3]
  ,[2/sqrt 10, 2/sqrt 6, -1/sqrt 3, 3]
  ,[2/sqrt 10, -2/sqrt 6, 4/sqrt 3, 2]
  ,[2/sqrt 10, 2/sqrt 6, -4/sqrt 3, 2]
  ,[2/sqrt 10, -2/sqrt 6, -5/sqrt 3, 1]
  ,[2/sqrt 10, 2/sqrt 6, 5/sqrt 3, 1]
  ,[2/sqrt 10, sqrt 6, 0, 2]
  ,[2/sqrt 10, -sqrt 6, 0, 2]
  ,[2/sqrt 10, sqrt 6, sqrt 3, 1]
  ,[2/sqrt 10, -sqrt 6, sqrt 3, 1]
  ,[7/sqrt 10, -1/sqrt 6, 2/sqrt 3, 2]
  ,[7/sqrt 10, -1/sqrt 6, -4/sqrt 3, 0]
  ,[7/sqrt 10, 3/sqrt 6, 0, 2]
  ,[7/sqrt 10, 3/sqrt 6, sqrt 3, 1]
  ,[7/sqrt 10, 3/sqrt 6, -sqrt 3, 1]
  ,[7/sqrt 10, -5/sqrt 6, 1/sqrt 3, 1]
  ,[7/sqrt 10, -5/sqrt 6, -2/sqrt 3, 0]
  ,[-3/sqrt 10, 1/sqrt 6, 1/sqrt 3, 3]
  ,[-3/sqrt 10, 1/sqrt 6, 4/sqrt 3, 2]
  ,[-3/sqrt 10, 1/sqrt 6, -5/sqrt 3, 1]
  ,[-3/sqrt 10, 5/sqrt 6, 2/sqrt 3, 2]
  ,[-3/sqrt 10, 5/sqrt 6, -4/sqrt 3, 0]
  ,[-3/sqrt 10, -7/sqrt 6, -1/sqrt 3, 1]
  ,[-3/sqrt 10, -7/sqrt 6, 2/sqrt 3, 0]
  ,[-8/sqrt 10, 0, 0, 2]
  ,[-8/sqrt 10, 0, sqrt 3, 1]
  ,[-8/sqrt 10, 0, -sqrt 3, 1]
  ,[-8/sqrt 10, -4/sqrt 6, -1/sqrt 3, 1]
  ,[-8/sqrt 10, 4/sqrt 6, 1/sqrt 3, 1]
  ,[-8/sqrt 10, -4/sqrt 6, 2/sqrt 3, 0]
  ,[-8/sqrt 10, 4/sqrt 6, -2/sqrt 3, 0]
  ,[2/sqrt 10, -2/sqrt 6, 1/sqrt 3, -3]
  ,[2/sqrt 10, 2/sqrt 6, -1/sqrt 3, -3]
  ,[2/sqrt 10, -2/sqrt 6, 4/sqrt 3, -2]
  ,[2/sqrt 10, 2/sqrt 6, -4/sqrt 3, -2]
  ,[2/sqrt 10, -2/sqrt 6, -5/sqrt 3, -1]
  ,[2/sqrt 10, 2/sqrt 6, 5/sqrt 3, -1]
  ,[2/sqrt 10, sqrt 6, 0, -2]
  ,[2/sqrt 10, -sqrt 6, 0, -2]
  ,[2/sqrt 10, sqrt 6, -sqrt 3, -1]
  ,[2/sqrt 10, -sqrt 6, -sqrt 3, -1]
  ,[7/sqrt 10, -1/sqrt 6, 2/sqrt 3, -2]
  ,[7/sqrt 10, 3/sqrt 6, 0, -2]
  ,[7/sqrt 10, 3/sqrt 6, sqrt 3, -1]
  ,[7/sqrt 10, 3/sqrt 6, -sqrt 3, -1]
  ,[7/sqrt 10, -5/sqrt 6, 1/sqrt 3, -1]
  ,[-3/sqrt 10, 1/sqrt 6, 1/sqrt 3, -3]
  ,[-3/sqrt 10, 1/sqrt 6, 4/sqrt 3, -2]
  ,[-3/sqrt 10, 1/sqrt 6, -5/sqrt 3, -1]
  ,[-3/sqrt 10, 5/sqrt 6, 2/sqrt 3, -2]
  ,[-3/sqrt 10, -7/sqrt 6, -1/sqrt 3, -1]
  ,[-8/sqrt 10, 0, 0, -2]
  ,[-8/sqrt 10, 0, sqrt 3, -1]
  ,[-8/sqrt 10, 0, -sqrt 3, -1]
  ,[-8/sqrt 10, -4/sqrt 6, -1/sqrt 3, -1]
  ,[-8/sqrt 10, 4/sqrt 6, 1/sqrt 3, -1] ]


cantellated5cell :: [[Double]]
cantellated5cell =
  [[4/sqrt 10, 0, 0, 2]
  ,[4/sqrt 10, 0, 3/sqrt 3, 1]
  ,[4/sqrt 10, 0, -3/sqrt 3, 1]
  ,[4/sqrt 10, -4/sqrt 6, 2/sqrt 3, 0]
  ,[4/sqrt 10, 4/sqrt 6, -2/sqrt 3, 0]
  ,[4/sqrt 10, -4/sqrt 6, -1/sqrt 3, 1]
  ,[4/sqrt 10, 4/sqrt 6, 1/sqrt 3, 1]
  ,[-6/sqrt 10, -2/sqrt 6, -2/sqrt 3, 0]
  ,[-6/sqrt 10, 2/sqrt 6, 2/sqrt 3, 0]
  ,[-1/sqrt 10, -1/sqrt 6, -4/sqrt 3, 0]
  ,[-1/sqrt 10, -5/sqrt 6, -2/sqrt 3, 0]
  ,[-1/sqrt 10, 3/sqrt 6, 0, 2]
  ,[-1/sqrt 10, -1/sqrt 6, 2/sqrt 3, 2]
  ,[-1/sqrt 10, -5/sqrt 6, 1/sqrt 3, 1]
  ,[-1/sqrt 10, 3/sqrt 6, 3/sqrt 3, 1]
  ,[-1/sqrt 10, 3/sqrt 6, -3/sqrt 3, 1]
  ,[-6/sqrt 10, -2/sqrt 6, 1/sqrt 3, 1]
  ,[-6/sqrt 10, 2/sqrt 6, -1/sqrt 3, 1]
  ,[4/sqrt 10, 0, 0, -2]
  ,[4/sqrt 10, 0, 3/sqrt 3, -1]
  ,[4/sqrt 10, 0, -3/sqrt 3, -1]
  ,[4/sqrt 10, -4/sqrt 6, -1/sqrt 3, -1]
  ,[4/sqrt 10, 4/sqrt 6, 1/sqrt 3, -1]
  ,[-1/sqrt 10, 3/sqrt 6, 0, -2]
  ,[-1/sqrt 10, -1/sqrt 6, 2/sqrt 3, -2]
  ,[-1/sqrt 10, -5/sqrt 6, 1/sqrt 3, -1]
  ,[-1/sqrt 10, 3/sqrt 6, 3/sqrt 3, -1]
  ,[-1/sqrt 10, 3/sqrt 6, -3/sqrt 3, -1]
  ,[-6/sqrt 10, -2/sqrt 6, 1/sqrt 3, -1]
  ,[-6/sqrt 10, 2/sqrt 6, -1/sqrt 3, -1]]

bitruncated5cell :: [[Double]]
bitruncated5cell =
  [ [0, 4/sqrt 6, 4/sqrt 3, 0]
  , [0, -4/sqrt 6, -4/sqrt 3, 0]
  , [0, 4/sqrt 6, -2/sqrt 3, 2]
  , [0, 4/sqrt 6, -2/sqrt 3, -2]
  , [0, -4/sqrt 6, 2/sqrt 3, -2]
  , [0, -4/sqrt 6, 2/sqrt 3, 2]
  , [5/sqrt 10, 1/sqrt 6, 4/sqrt 3, 0]
  , [-5/sqrt 10, -1/sqrt 6, -4/sqrt 3, 0]
  , [5/sqrt 10, 1/sqrt 6, -2/sqrt 3, 2]
  , [5/sqrt 10, 1/sqrt 6, -2/sqrt 3, -2]
  , [-5/sqrt 10, -1/sqrt 6, 2/sqrt 3, -2]
  , [-5/sqrt 10, -1/sqrt 6, 2/sqrt 3, 2]
  , [5/sqrt 10, 5/sqrt 6, 2/sqrt 3, 0]
  , [-5/sqrt 10, -5/sqrt 6, -2/sqrt 3, 0]
  , [5/sqrt 10, 5/sqrt 6, -1/sqrt 3, 1]
  , [5/sqrt 10, 5/sqrt 6, -1/sqrt 3, -1]
  , [-5/sqrt 10, -5/sqrt 6, 1/sqrt 3, -1]
  , [-5/sqrt 10, -5/sqrt 6, 1/sqrt 3, 1]
  , [5/sqrt 10, -3/sqrt 6, 0, 2]
  , [5/sqrt 10, -3/sqrt 6, 0, -2]
  , [-5/sqrt 10, 3/sqrt 6, 0, -2]
  , [-5/sqrt 10, 3/sqrt 6, 0, 2]
  , [5/sqrt 10, -3/sqrt 6, sqrt 3, 1]
  , [5/sqrt 10, -3/sqrt 6, sqrt 3, -1]
  , [5/sqrt 10, -3/sqrt 6, -sqrt 3, 1]
  , [5/sqrt 10, -3/sqrt 6, -sqrt 3, -1]
  , [-5/sqrt 10, 3/sqrt 6, -sqrt 3, -1]
  , [-5/sqrt 10, 3/sqrt 6, -sqrt 3, 1]
  , [-5/sqrt 10, 3/sqrt 6, sqrt 3, -1]
  , [-5/sqrt 10, 3/sqrt 6, sqrt 3, 1] ]

rectified5cell :: [[Double]]
rectified5cell =
  [ [-3/sqrt 10, -3/sqrt 6, 0, 0]
  , [-3/sqrt 10, 1/sqrt 6, -2/sqrt 3, 0]
  , [-3/sqrt 10, 1/sqrt 6, 1/sqrt 3, 1]
  , [-3/sqrt 10, 1/sqrt 6, 1/sqrt 3, -1]
  , [2/sqrt 10, 2/sqrt 6, 2/sqrt 3, 0]
  , [2/sqrt 10, -2/sqrt 6, -2/sqrt 3, 0]
  , [2/sqrt 10, 2/sqrt 6, -1/sqrt 3, 1]
  , [2/sqrt 10, 2/sqrt 6, -1/sqrt 3, -1]
  , [2/sqrt 10, -2/sqrt 6, 1/sqrt 3, 1]
  , [2/sqrt 10, -2/sqrt 6, 1/sqrt 3, -1] ]

sircope :: [[Double]]
sircope =
  let a = (1+ sqrt 2)/2 in
  [
   [-a, -0.5, -0.5, -0.5],
   [a, -0.5, -0.5, -0.5],
   [-a, 0.5, -0.5, -0.5],
   [a, 0.5, -0.5, -0.5],
   [-a, -0.5, 0.5, -0.5],
   [a, -0.5, 0.5, -0.5],
   [-a, 0.5, 0.5, -0.5],
   [a, 0.5, 0.5, -0.5],
   [-a, -0.5, -0.5, 0.5],
   [a, -0.5, -0.5, 0.5],
   [-a, 0.5, -0.5, 0.5],
   [a, 0.5, -0.5, 0.5],
   [-a, -0.5, 0.5, 0.5],
   [a, -0.5, 0.5, 0.5],
   [-a, 0.5, 0.5, 0.5],
   [a, 0.5, 0.5, 0.5],
   [-0.5, -a, -0.5, -0.5],
   [0.5, -a, -0.5, -0.5],
   [-0.5, a, -0.5, -0.5],
   [0.5, a, -0.5, -0.5],
   [-0.5, -a, 0.5, -0.5],
   [0.5, -a, 0.5, -0.5],
   [-0.5, a, 0.5, -0.5],
   [0.5, a, 0.5, -0.5],
   [-0.5, -a, -0.5, 0.5],
   [0.5, -a, -0.5, 0.5],
   [-0.5, a, -0.5, 0.5],
   [0.5, a, -0.5, 0.5],
   [-0.5, -a, 0.5, 0.5],
   [0.5, -a, 0.5, 0.5],
   [-0.5, a, 0.5, 0.5],
   [0.5, a, 0.5, 0.5],
   [-0.5, -0.5, -a, -0.5],
   [0.5, -0.5, -a, -0.5],
   [-0.5, 0.5, -a, -0.5],
   [0.5, 0.5, -a, -0.5],
   [-0.5, -0.5, a, -0.5],
   [0.5, -0.5, a, -0.5],
   [-0.5, 0.5, a, -0.5],
   [0.5, 0.5, a, -0.5],
   [-0.5, -0.5, -a, 0.5],
   [0.5, -0.5, -a, 0.5],
   [-0.5, 0.5, -a, 0.5],
   [0.5, 0.5, -a, 0.5],
   [-0.5, -0.5, a, 0.5],
   [0.5, -0.5, a, 0.5],
   [-0.5, 0.5, a, 0.5],
   [0.5, 0.5, a, 0.5]
  ]

tutcup :: [[Double]]
tutcup =
  [ [1, -1/sqrt 3, 5/sqrt 6, 1/sqrt 2]
  , [-1, -1/sqrt 3, 5/sqrt 6, 1/sqrt 2]
  , [0, 2/sqrt 3, 5/sqrt 6, 1/sqrt 2]
  , [2, -2/sqrt 3, 1/sqrt 6, 1/sqrt 2]
  , [-2, -2/sqrt 3, 1/sqrt 6, 1/sqrt 2]
  , [0, 4/sqrt 3, 1/sqrt 6, 1/sqrt 2]
  , [1, 3/sqrt 3, -3/sqrt 6, 1/sqrt 2]
  , [1, -3/sqrt 3, -3/sqrt 6, 1/sqrt 2]
  , [-1, 3/sqrt 3, -3/sqrt 6, 1/sqrt 2]
  , [-1, -3/sqrt 3, -3/sqrt 6, 1/sqrt 2]
  , [2, 0, -3/sqrt 6, 1/sqrt 2]
  , [-2, 0, -3/sqrt 6, 1/sqrt 2]
  , [1, 3/sqrt 3, 3/sqrt 6, -1/sqrt 2]
  , [1, -3/sqrt 3, 3/sqrt 6, -1/sqrt 2]
  , [-1, 3/sqrt 3, 3/sqrt 6, -1/sqrt 2]
  , [-1, -3/sqrt 3, 3/sqrt 6, -1/sqrt 2]
  , [2, 0, 3/sqrt 6, -1/sqrt 2]
  , [-2, 0, 3/sqrt 6, -1/sqrt 2]
  , [2, 2/sqrt 3, -1/sqrt 6, -1/sqrt 2]
  , [-2, 2/sqrt 3, -1/sqrt 6, -1/sqrt 2]
  , [0, -4/sqrt 3, -1/sqrt 6, -1/sqrt 2]
  , [1, 1/sqrt 3, -5/sqrt 6, -1/sqrt 2]
  , [-1, 1/sqrt 3, -5/sqrt 6, -1/sqrt 2]
  , [0, -2/sqrt 3, -5/sqrt 6, -1/sqrt 2] ]



runcinatedTesseract :: [[Double]]
runcinatedTesseract =
  [[i*a, j*b, k*c, l*d] | i <- pm, j <- pm, k <- pm, l <- pm, (a,b,c,d) <- s]
  where
  pm = [-1,1]
  x = 1 + sqrt 2
  s = [(1,1,1,x),(1,1,x,1),(1,x,1,1),(x,1,1,1)]

runcinated5cells :: [[Double]]
runcinated5cells = [ [sqrt(5/2), 1/sqrt 6, 1/sqrt 3, 1]
                    ,[sqrt(5/2), 1/sqrt 6, 1/sqrt 3, -1]
                    ,[-sqrt(5/2), -1/sqrt 6, -1/sqrt 3, -1]
                    ,[-sqrt(5/2), -1/sqrt 6, -1/sqrt 3, 1]
                    ,[sqrt(5/2), 1/sqrt 6, -2/sqrt 3, 0]
                    ,[-sqrt(5/2), -1/sqrt 6, 2/sqrt 3, 0]
                    ,[sqrt(5/2), -sqrt(3/2), 0, 0]
                    ,[-sqrt(5/2), sqrt(3/2), 0, 0]
                    ,[0, 2*sqrt(2/3), 1/sqrt 3, 1]
                    ,[0, 2*sqrt(2/3), 1/sqrt 3, -1]
                    ,[0, -2*sqrt(2/3), -1/sqrt 3, -1]
                    ,[0, -2*sqrt(2/3), -1/sqrt 3, 1]
                    ,[0, 2*sqrt(2/3), -2/sqrt 3, 0]
                    ,[0, -2*sqrt(2/3), 2/sqrt 3, 0]
                    ,[0, 0, sqrt 3, 1]
                    ,[0, 0, sqrt 3, -1]
                    ,[0, 0, -sqrt 3, 1]
                    ,[0, 0, -sqrt 3, -1]
                    ,[0, 0, 0, 2]
                    ,[0, 0, 0, -2] ]

truncated5cells :: [[Double]]
truncated5cells = [ [3/sqrt 10, -1/sqrt 6, 2/sqrt 3, 2]
                   ,[3/sqrt 10, -1/sqrt 6, 2/sqrt 3, -2]
                   ,[3/sqrt 10, -1/sqrt 6, -4/sqrt 3, 0]
                   ,[3/sqrt 10, 3/sqrt 6, 0, 2]
                   ,[3/sqrt 10, 3/sqrt 6, 0, -2]
                   ,[3/sqrt 10, 3/sqrt 6, sqrt 3, 1]
                   ,[3/sqrt 10, 3/sqrt 6, sqrt 3, -1]
                   ,[3/sqrt 10, 3/sqrt 6, -sqrt 3, 1]
                   ,[3/sqrt 10, 3/sqrt 6, -sqrt 3, -1]
                   ,[3/sqrt 10, -5/sqrt 6, 1/sqrt 3, 1]
                   ,[3/sqrt 10, -5/sqrt 6, 1/sqrt 3, -1]
                   ,[3/sqrt 10, -5/sqrt 6, -2/sqrt 3, 0]
                   ,[-2/sqrt 10, 2/sqrt 6, 2/sqrt 3, 2]
                   ,[-2/sqrt 10, 2/sqrt 6, 2/sqrt 3, -2]
                   ,[-2/sqrt 10, 2/sqrt 6, -4/sqrt 3, 0]
                   ,[-2/sqrt 10, -sqrt 6, 0, 0]
                   ,[-7/sqrt 10, 1/sqrt 6, 1/sqrt 3, 1]
                   ,[-7/sqrt 10, 1/sqrt 6, 1/sqrt 3, -1]
                   ,[-7/sqrt 10, 1/sqrt 6, -2/sqrt 3, 0]
                   ,[-7/sqrt 10, -3/sqrt 6, 0, 0] ]

truncated24cells :: [[Double]]
truncated24cells = [permuteList p [0,i,j*2,k*3] | p <- permutations 4, i <- pm, j <- pm , k <- pm]
  where
    pm = [-1,1]

thex :: [[Double]]
thex = nub [ permuteList p [0,0,i,j*2] | p <- permutations 4, i <- pm, j <- pm]
  where
    pm = [-1,1]

truncatedTetrahedron :: [[Double]]
truncatedTetrahedron = [ [1/sqrt 6, -2/sqrt 3, 2]
                       , [1/sqrt 6, -2/sqrt 3, -2]
                       , [1/sqrt 6, 4/ sqrt 3, 0]
                       , [-3/sqrt 6, 0, 2]
                       , [-3/sqrt 6, 0, -2]
                       , [-3/sqrt 6, sqrt 3, 1]
                       , [-3/sqrt 6, -sqrt 3, 1]
                       , [-3/sqrt 6, sqrt 3, -1]
                       , [-3/sqrt 6, -sqrt 3, -1]
                       , [5/sqrt 6, -1/sqrt 3, 1]
                       , [5/sqrt 6, -1/sqrt 3, -1]
                       , [5/sqrt 6, 2/sqrt 3, 0] ]


daVinci :: [[Double]]
daVinci =   [ [1.61352, -0.43234, 1.1862],
              [1.18118, -1.18118, 1.1862],
              [0.43234, -1.61352, 1.1862],
              [-0.43234, -1.61352, 1.1862],
              [-1.18118, -1.18118, 1.1862],
              [-1.61352, -0.43234, 1.1862],
              [-1.61352, 0.43234, 1.1862],
              [-1.18118, 1.18118, 1.1862],
              [-0.43234, 1.61352, 1.1862],
              [0.43234, 1.61352, 1.1862],
              [1.18118, 1.18118, 1.1862],
              [1.61352, 0.43234, 1.1862],
              [1.61352, -0.43234, -1.1862],
              [1.61352, 0.43234, -1.1862],
              [1.18118, 1.18118, -1.1862],
              [0.43234, 1.61352, -1.1862],
              [-0.43234, 1.61352, -1.1862],
              [-1.18118, 1.18118, -1.1862],
              [-1.61352, 0.43234, -1.1862],
              [-1.61352, -0.43234, -1.1862],
              [-1.18118, -1.18118, -1.1862],
              [-0.43234, -1.61352, -1.1862],
              [0.43234, -1.61352, -1.1862],
              [1.18118, -1.18118, -1.1862],
              [2.0102, 0.53863, 0],
              [1.47157, 1.47157, 0],
              [0.53863, 2.0102, 0],
              [-0.53863, 2.0102, 0],
              [-1.47157, 1.47157, 0],
              [-2.0102, 0.53863, 0],
              [-2.0102, -0.53863, 0],
              [-1.47157, -1.47157, 0],
              [-0.53863, -2.0102, 0],
              [0.53863, -2.0102, 0],
              [1.47157, -1.47157, 0],
              [2.0102, -0.53863, 0],
              [0.89068, 0.23866, 1.77777],
              [0.89068, -0.23866, 1.77777],
              [0.65202, -0.65202, 1.77777],
              [0.23866, -0.89068, 1.77777],
              [-0.23866, -0.89068, 1.77777],
              [-0.65202, -0.65202, 1.77777],
              [-0.89068, -0.23866, 1.77777],
              [-0.89068, 0.23866, 1.77777],
              [-0.65202, 0.65202, 1.77777],
              [-0.23866, 0.89068, 1.77777],
              [0.23866, 0.89068, 1.77777],
              [0.65202, 0.65202, 1.77777],
              [0.65202, -0.65202, -1.77777],
              [0.89068, -0.23866, -1.77777],
              [0.89068, 0.23866, -1.77777],
              [0.65202, 0.65202, -1.77777],
              [0.23866, 0.89068, -1.77777],
              [-0.23866, 0.89068, -1.77777],
              [-0.65202, 0.65202, -1.77777],
              [-0.89068, 0.23866, -1.77777],
              [-0.89068, -0.23866, -1.77777],
              [-0.65202, -0.65202, -1.77777],
              [-0.23866, -0.89068, -1.77777],
              [0.23866, -0.89068, -1.77777],
              [0, 0, 2.04922],
              [0, 0, -2.04922]]


-- Cuboctohadron4D - does not work
cuboctahedron4d :: [[Double]]
cuboctahedron4d = nub [ permuteList p [0,1,1,2] | p <- permutations 4]

-- OLOID --
twocircles :: [[Double]]
twocircles = circle1 `union` circle2
  where
    circle1 = [[cos (realToFrac i * 2*pi/30), sin (realToFrac i * 2*pi/30), 0] | i <- [0 .. 29]]
    circle2 = [[0, cos (realToFrac i * 2*pi/30) - 1, sin (realToFrac i * 2*pi/30)] | i <- [0 .. 29]]

translate3 :: [[Double]] -> [Double] -> [[Double]]
translate3 points u = map (zipWith (+) u) points

duplicate3 :: [[Double]] -> [Double] -> [[Double]]
duplicate3 points u = nub $ points ++ translate3 points u

rgg :: [[Double]]
rgg = [[-5,-5, 16], [-5, 8, 3 ], [ 4,-1, 3 ], [ 4,-5, 7], [ 4,-1,-10],
       [ 4,-5,-10], [-5, 8,-10], [-5,-5,-10]]

centricCube :: [[Double]]
centricCube =  [[-1,-1,-1],[-1,-1, 1],[-1, 1,-1],[-1, 1, 1],[ 1,-1,-1],
                [ 1,-1, 1],[ 1, 1,-1],[ 1, 1, 1],[ 0, 0, 0]]

squareLattice :: [[Double]]
squareLattice = [[0,0],[0,1],[0,2]
                ,[1,0],[1,1],[1,2]
                ,[2,0],[2,1],[2,2]]

centricSquare :: [[Double]]
centricSquare = [[0,0],[0,2],[2,0],[2,2],[1,1]]

cuboctahedron :: [[Double]]
cuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
                [[i,0,j] | i <- [-1,1], j <- [-1,1]] ++
                [[0,i,j] | i <- [-1,1], j <- [-1,1]]

truncatedCuboctahedron :: [[Double]]
truncatedCuboctahedron =
  [[i, j * (1 + sqrt 2), k * (1 + 2*sqrt 2)] | i <- pm, j <- pm, k <- pm] ++
  [[j * (1 + sqrt 2), i, k * (1 + 2*sqrt 2)] | i <- pm, j <- pm, k <- pm] ++
  [[j * (1 + sqrt 2), k * (1 + 2*sqrt 2), i] | i <- pm, j <- pm, k <- pm] ++
  [[i, k * (1 + 2*sqrt 2), j * (1 + sqrt 2)] | i <- pm, j <- pm, k <- pm] ++
  [[k * (1 + 2*sqrt 2), i, j * (1 + sqrt 2)] | i <- pm, j <- pm, k <- pm] ++
  [[k * (1 + 2*sqrt 2), j * (1 + sqrt 2), i] | i <- pm, j <- pm, k <- pm]
  where
    pm = [-1,1]

rhombicDodecahedron :: [[Double]]
rhombicDodecahedron = [[-1.0, 0.0, 0.0], [-0.5,-0.5,-0.5], [-0.5,-0.5, 0.5],
                       [ 0.0,-1.0, 0.0], [-0.5, 0.5,-0.5], [-0.5, 0.5, 0.5],
                       [ 0.0, 1.0, 0.0], [ 1.0, 0.0, 0.0], [ 0.5,-0.5,-0.5],
                       [ 0.5,-0.5, 0.5], [ 0.5, 0.5,-0.5], [ 0.5, 0.5, 0.5],
                       [ 0.0, 0.0,-1.0], [ 0.0, 0.0, 1.0]]

faceCenteredCubic :: [[Double]]
faceCenteredCubic = [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1]
                    ,[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1]
                    ,[1,0,0],[-1,0,0]
                    ,[0,1,0],[0,-1,0]
                    ,[0,0,1],[0,0,-1]]

waves :: [[Double]]
waves = [f u v | u <- seq_u, v <- seq_v]
  where
    f u v = [u, 0.05*(sin(v*4*pi)+sin(u*4*pi)), v]
    frac p q = realToFrac p/ realToFrac q
    n = 10
    seq_u = [8 * frac i n - 4 | i <- [0 .. n]]
    seq_v = [4 * frac i n - 2 | i <- [0 .. n]]

waves2D :: [[Double]]
waves2D = concatMap (\y' -> map (\[x,y] -> [x, y+y']) sinusoid) seq_y
  where
    seq_x = [realToFrac i * pi/2 | i <- [0 .. 20]]
    sinusoid = map (\(x,y) -> [x,y]) (zip seq_x (map sin seq_x))
    seq_y = [realToFrac 2*i | i <- [0 .. 20]]


type Vertex = [Double]
ncube :: Int -> [Vertex]
ncube n = concatMap (mapM (\x -> nub [x,-x])) [replicate n 1]

cube3 :: [[Double]]
cube3 = [[i,j,k] | i <- [-1,1], j <- [-1,1], k <- [-1,1]]

cube4 :: [[Double]]
cube4 = [[i,j,k,l] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]]

cube5 :: [[Double]]
cube5 = [[i,j,k,l,m] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1],
                       m <- [-1,1]]

cube6 :: [[Double]]
cube6 = [[i,j,k,l,m,n] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1],
                         m <- [-1,1], n <- [-1,1]]


mobiusStrip :: [[Double]]
mobiusStrip = map (\(u,v) -> [ cos u * (1 + v/2 * cos(u/2))
                             , sin u * (1 + v/2 * cos(u/2))
                             , v/2 * sin(u/2)              ]) uv
  where
    uv = [(u,v) | u <- u_, v <- v_]
    u_ = [i/50 * 2 *pi | i <- [0 .. 50]]
    v_ = [-1,1] --  [i/50 | i <- [-50 .. 50]]

irregularPolyhedron :: [[Double]]
irregularPolyhedron =
  [ [ -0.586233 , 0.192482 , -1.9732e-2 ]
  , [ -0.344233 , 0.301871 , 0.358301 ]
  , [ 0.344233 , -0.301871 , 0.358301 ]
  , [ 0.179112 , -0.514398 , -0.290554 ]
  , [ 1.1612e-2 , -0.564879 , 0.137037 ]
  , [ -1.1612e-2 , 0.564879 , 0.137037 ]
  , [ -0.179112 , 0.514398 , -0.290554 ]
  , [ -0.151548 , -0.185001 , -0.529915 ]
  , [ -5.5686e-2 , -0.430284 , -0.411612 ]
  , [ -0.364663 , -0.382652 , -0.277601 ]
  , [ -0.394163 , -0.151218 , -0.422176 ]
  , [ 5.5686e-2 , 0.430284 , -0.411612 ]
  , [ 0.364663 , 0.382652 , -0.277601 ]
  , [ 0.394163 , 0.151218 , -0.422176 ]
  , [ 0.151548 , 0.185001 , -0.529915 ]
  , [ 0.361372 , 0.268614 , 0.393607 ]
  , [ 0.246149 , 0.511456 , 0.185169 ]
  , [ 0.417404 , 0.426728 , -1.1876e-2 ]
  , [ 0.510787 , 0.234322 , 0.148969 ]
  , [ -0.361372 , -0.268614 , 0.393608 ]
  , [ -0.510787 , -0.234322 , 0.148969 ]
  , [ -0.417404 , -0.426728 , -1.1876e-2 ]
  , [ -0.246149 , -0.511456 , 0.185169 ]
  , [ 0.430396 , -0.334808 , -0.249032 ]
  , [ 0.586233 , -0.192482 , -1.9732e-2 ]
  , [ 0.587567 , -0.188189 , -2.1408e-2 ]
  , [ 0.509201 , -0.136702 , -0.282307 ]
  , [ -0.430396 , 0.334808 , -0.249032 ]
  , [ -0.509201 , 0.136702 , -0.282307 ]
  , [ -0.587567 , 0.188189 , -2.1408e-2 ]
  , [ -0.272269 , -0.118712 , 0.520703 ]
  , [ -0.248355 , 9.5223e-2 , 0.535651 ]
  , [ 0.272269 , 0.118712 , 0.520703 ]
  , [ -2.22e-4 , 2.388e-3 , 0.617335 ]
  , [ 2.22e-4 , -2.388e-3 , 0.617335 ]
  , [ 0.248355 , -9.5223e-2 , 0.535651 ]
  ]

truncatedCube :: [[Double]]
truncatedCube =
  let x = 1 + sqrt 2 in
  [[i, j*x, k*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1]] ++
  [[i*x, j, k*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1]] ++
  [[i*x, j*x, k] | i <- [-1,1], j <- [-1,1], k <- [-1,1]]

truncatedTesseract :: [[Double]]
truncatedTesseract =
  let x = 1 + sqrt 2 in
  [[i, j*x, k*x, l*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
  [[i*x, j, k*x, l*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
  [[i*x, j*x, k, l*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
  [[i*x, j*x, k*x, l] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]]

projectedTruncatedTesseract :: [[Double]]
projectedTruncatedTesseract =
  [ [-x1,-x2,-x2],
    [-x4,-x3,-x3],
    [-x1,-x2, x2],
    [-x4,-x3, x3],
    [-x1, x2,-x2],
    [-x4, x3,-x3],
    [-x1, x2, x2],
    [-x4, x3, x3],
    [ x1,-x2,-x2],
    [ x4,-x3,-x3],
    [ x1,-x2, x2],
    [ x4,-x3, x3],
    [ x1, x2,-x2],
    [ x4, x3,-x3],
    [ x1, x2, x2],
    [ x4, x3, x3],
    [-x2,-x1,-x2],
    [-x3,-x4,-x3],
    [-x2,-x1, x2],
    [-x3,-x4, x3],
    [-x2, x1,-x2],
    [-x3, x4,-x3],
    [-x2, x1, x2],
    [-x3, x4, x3],
    [ x2,-x1,-x2],
    [ x3,-x4,-x3],
    [ x2,-x1, x2],
    [ x3,-x4, x3],
    [ x2, x1,-x2],
    [ x3, x4,-x3],
    [ x2, x1, x2],
    [ x3, x4, x3],
    [-x2,-x2,-x1],
    [-x3,-x3,-x4],
    [-x2,-x2, x1],
    [-x3,-x3, x4],
    [-x2, x2,-x1],
    [-x3, x3,-x4],
    [-x2, x2, x1],
    [-x3, x3, x4],
    [ x2,-x2,-x1],
    [ x3,-x3,-x4],
    [ x2,-x2, x1],
    [ x3,-x3, x4],
    [ x2, x2,-x1],
    [ x3, x3,-x4],
    [ x2, x2, x1],
    [ x3, x3, x4],
    [-x6,-x6,-x6],
    [-x5,-x5,-x5],
    [-x6,-x6, x6],
    [-x5,-x5, x5],
    [-x6, x6,-x6],
    [-x5, x5,-x5],
    [-x6, x6, x6],
    [-x5, x5, x5],
    [ x6,-x6,-x6],
    [ x5,-x5,-x5],
    [ x6,-x6, x6],
    [ x5,-x5, x5],
    [ x6, x6,-x6],
    [ x5, x5,-x5],
    [ x6, x6, x6],
    [ x5, x5, x5]]
    where
      a = 1 + sqrt 2
      x1 = 2 / (sqrt(1 + 3*a*a) + a) -- 0.29
      x2 = 2*a / (sqrt(1 + 3*a*a) + a) -- 0.71
      x3 = 2*a / (sqrt(1 + 3*a*a) - a) -- 2.56
      x4 = 2 / (sqrt(1 + 3*a*a) - a) -- 1.06
      x5 = 2*a / (sqrt(1 + 3*a*a) - 1) -- 1.46
      x6 = 2*a / (sqrt(1 + 3*a*a) + 1) -- 0.91

rectifiedTesseract :: [[Double]]
rectifiedTesseract =
  let x = sqrt 2 in
  [[0, j*x, k*x, l*x] | j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
  [[i*x, 0, k*x, l*x] | i <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
  [[i*x, j*x, 0, l*x] | i <- [-1,1], j <- [-1,1], l <- [-1,1]] ++
  [[i*x, j*x, k*x, 0] | i <- [-1,1], j <- [-1,1], k <- [-1,1]]

cantellatedTesseract :: [[Double]]
cantellatedTesseract =
  let x = 1 + sqrt 2 in
  map (map (/ sqrt(2 + 2*x*x))) $
    [[i, j, k*x, l*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
    [[i*x, j, k, l*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
    [[i*x, j*x, k, l] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
    [[i, j*x, k, l*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
    [[i, j*x, k*x, l] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]] ++
    [[i*x, j, k*x, l] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]]

projectedCantellatedTesseract :: [[Double]]
projectedCantellatedTesseract = map stereographic cantellatedTesseract
  where
    stereographic x = map (/(1-x!!3)) [2 * x!!0, 2 * x!!1, 2 * x!!2]

octaplex :: [[Double]]
octaplex = map (map (/ sqrt 2)) $
  [[i,j,0,0] | i <- pm, j <- pm] ++
  [[i,0,j,0] | i <- pm, j <- pm] ++
  [[i,0,0,j] | i <- pm, j <- pm] ++
  [[0,0,i,j] | i <- pm, j <- pm] ++
  [[0,i,0,j] | i <- pm, j <- pm] ++
  [[0,i,j,0] | i <- pm, j <- pm]
  where
    pm = [-1,1]


nonConvexPolyhedron :: [[Double]]
nonConvexPolyhedron =
  [[i*x, j*x, k*x] | i <- [-1,1], j <- [-1,1], k <- [-1,1]] ++
  [[i*y, 0, 0] | i <- [-1,1]] ++
  [[0, j*y, 0] | j <- [-1,1]] ++
  [[0, 0, k*y] | k <- [-1,1]]
  where x = 2.1806973249
        y = 3.5617820682
  -- tetrahedra: (i 2.18, j 2.18, k 2.18), (i 3.56, 0, 0), (0, j 3.56, 0), (0, 0, k 3.56)

dodecahedron :: [[Double]] -- it's icosahedron !
dodecahedron = let phi = (1 + sqrt 5)/2 in
               [[0,i,j] | i <- [-1,1], j <- [-phi, phi]] ++
               [[j,0,i] | i <- [-1,1], j <- [-phi, phi]] ++
               [[i,j,0] | i <- [-1,1], j <- [-phi, phi]]

icosahedron :: [[Double]]
icosahedron = dodecahedron

duoprism330 :: [[Double]]
duoprism330 = [a ++ b | a <- triangle, b <- p30]
  where
    triangle = [[cos (realToFrac i * 2*pi/3), sin (realToFrac i * 2*pi/3)] | i <- [0 .. 2]]
    p30 = [[cos (realToFrac i * 2*pi/30), sin (realToFrac i * 2*pi/30)] | i <- [0 .. 29]]


duoprism35 :: [[Double]]
duoprism35 = [a ++ b | a <- triangle, b <- pentagon]
  where
    triangle = [ [sqrt 3 / 2 ,  0.5]
               , [-sqrt 3 / 2,  0.5]
               , [0          , -1  ] ]
    pentagon = [ [1          , 0          ]
               , [cos(2*pi/5), sin(2*pi/5)]
               , [cos(4*pi/5), sin(4*pi/5)]
               , [cos(6*pi/5), sin(6*pi/5)]
               , [cos(8*pi/5), sin(8*pi/5)] ]

duoprism34 :: [[Double]]
duoprism34 = [a ++ b | a <- triangle, b <- square]
  where
    triangle = [ [sqrt 3 / 2 ,  0.5]
               , [-sqrt 3 / 2,  0.5]
               , [0          , -1  ] ]
    square = [[i * sqrt 2 / 2, j * sqrt 2 / 2] | i <- [-1,1], j <- [-1,1]]

triangularDuoprism :: [[Double]]
triangularDuoprism = [a ++ b | a <- triangle, b <- triangle]
  where
    triangle = [ [sqrt 3 / 2 ,  0.5]
               , [-sqrt 3 / 2,  0.5]
               , [0          , -1  ] ]

hexagonalDuoprism :: [[Double]]
hexagonalDuoprism = [a ++ b | a <- hexagon, b <- hexagon]
  where
    hexagon = [ [sqrt 3 / 2,   0.5]
              , [0         ,   1  ]
              , [-sqrt 3 / 2,  0.5]
              , [-sqrt 3 / 2, -0.5]
              , [0          , -1  ]
              , [sqrt 3 / 2 , -0.5] ]

projectedHexagonalDuoprims :: [[Double]]
projectedHexagonalDuoprims =
    [ [ 1.8945800837529239 , 1.0938363213560542 , 1.8945800837529239 ]
    , [ 4.181540550352056 , 2.414213562373096 , 0.0 ]
    , [ 1.8945800837529239 , 1.0938363213560542 , -1.8945800837529239 ]
    , [ 0.904836765142137 , 0.522407749927483 , -0.904836765142137 ]
    , [ 0.7174389352143009 , 0.4142135623730951 , 0.0 ]
    , [ 0.904836765142137 , 0.522407749927483 , 0.904836765142137 ]
    , [ 0.0 , 2.1876726427121085 , 1.8945800837529239 ]
    , [ 0.0 , 4.828427124746192 , 0.0 ]
    , [ 0.0 , 2.1876726427121085 , -1.8945800837529239 ]
    , [ 0.0 , 1.044815499854966 , -0.904836765142137 ]
    , [ 0.0 , 0.8284271247461902 , 0.0 ]
    , [ 0.0 , 1.044815499854966 , 0.904836765142137 ]
    , [ -1.8945800837529239 , 1.0938363213560542 , 1.8945800837529239 ]
    , [ -4.181540550352056 , 2.414213562373096 , 0.0 ]
    , [ -1.8945800837529239
      , 1.0938363213560542
      , -1.8945800837529239
      ]
    , [ -0.904836765142137 , 0.522407749927483 , -0.904836765142137 ]
    , [ -0.7174389352143009 , 0.4142135623730951 , 0.0 ]
    , [ -0.904836765142137 , 0.522407749927483 , 0.904836765142137 ]
    , [ -1.8945800837529239
      , -1.0938363213560542
      , 1.8945800837529239
      ]
    , [ -4.181540550352056 , -2.414213562373096 , 0.0 ]
    , [ -1.8945800837529239
      , -1.0938363213560542
      , -1.8945800837529239
      ]
    , [ -0.904836765142137 , -0.522407749927483 , -0.904836765142137 ]
    , [ -0.7174389352143009 , -0.4142135623730951 , 0.0 ]
    , [ -0.904836765142137 , -0.522407749927483 , 0.904836765142137 ]
    , [ 0.0 , -2.1876726427121085 , 1.8945800837529239 ]
    , [ 0.0 , -4.828427124746192 , 0.0 ]
    , [ 0.0 , -2.1876726427121085 , -1.8945800837529239 ]
    , [ 0.0 , -1.044815499854966 , -0.904836765142137 ]
    , [ 0.0 , -0.8284271247461902 , 0.0 ]
    , [ 0.0 , -1.044815499854966 , 0.904836765142137 ]
    , [ 1.8945800837529239 , -1.0938363213560542 , 1.8945800837529239 ]
    , [ 4.181540550352056 , -2.414213562373096 , 0.0 ]
    , [ 1.8945800837529239
      , -1.0938363213560542
      , -1.8945800837529239
      ]
    , [ 0.904836765142137 , -0.522407749927483 , -0.904836765142137 ]
    , [ 0.7174389352143009 , -0.4142135623730951 , 0.0 ]
    , [ 0.904836765142137 , -0.522407749927483 , 0.904836765142137 ]
    ]

hexaSquare :: [[Double]]
hexaSquare = [a ++ b | a <- hexagon, b <- square]
  where
    hexagon = [ [sqrt 3 / 2,   0.5]
              , [0         ,   1  ]
              , [-sqrt 3 / 2,  0.5]
              , [-sqrt 3 / 2, -0.5]
              , [0          , -1  ]
              , [sqrt 3 / 2 , -0.5] ]
    square = [[i * sqrt 2 / 2, j * sqrt 2 / 2] | i <- [-1,1], j <- [-1,1]]

cubinder :: [[Double]]
cubinder = [a ++ b | a <- circle, b <- square]
  where
    circle = [[cos (realToFrac i * 2*pi/30), sin (realToFrac i * 2*pi/30)] | i <- [0 .. 29]]
    square = [[i * sqrt 2 / 2, j * sqrt 2 / 2] | i <- [-1,1], j <- [-1,1]]

duoprism1616 :: [[Double]]
duoprism1616 = [a ++ b | a <- p16, b <- p16]
  where
    p16 = [[cos (realToFrac i * 2*pi/16), sin (realToFrac i * 2*pi/16)] | i <- [0 .. 15]]

duocylinder :: [[Double]]
duocylinder = [a ++ b | a <- circle, b <- circle]
  where
    circle = [[cos (realToFrac i * 2*pi/30), sin (realToFrac i * 2*pi/30)] | i <- [0 .. 29]]

reuleuxTetrahedron :: [[Double]]
reuleuxTetrahedron =
  [ [0.5 / sqrt 3, -0.5, 0.5 / sqrt 6]
  , [sqrt 3 / 3, 0, -0.5 / sqrt 6]
  , [0.5 / sqrt 3, 0.5, -0.5 / sqrt 6]
  , [0, 0, 0.5 * sqrt 3 / sqrt 2] ]


hexacosichoron :: [[Double]]
hexacosichoron =
  [[i*0.5, j*0.5, k*0.5, l*0.5] | i <- pm, j <- pm, k <- pm, l <-pm] ++
  [[0, 0, 0, i] | i <- pm] ++
  [[0, 0, i, 0] | i <- pm] ++
  [[0, i, 0, 0] | i <- pm] ++
  [[i, 0, 0, 0] | i <- pm] ++
  [ permuteList p [phi/2, 1/2, 1/2/phi, 0]    | p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [phi/2, 1/2, -1/2/phi, 0]   |  p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [phi/2, -1/2, 1/2/phi, 0]   |  p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [phi/2, -1/2, -1/2/phi, 0]  |  p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [-phi/2, 1/2, 1/2/phi, 0]  |  p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [-phi/2, 1/2, -1/2/phi, 0] |  p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [-phi/2, -1/2, 1/2/phi, 0]  |  p <- permutations4, isEvenPermutation p] ++
  [ permuteList p [-phi/2, -1/2, -1/2/phi, 0]   |  p <- permutations4, isEvenPermutation p]
  where
    permutations4 = permutations 4
    phi = (1 + sqrt 5) / 2
    pm = [-1,1]

snub24cell :: [[Double]]
snub24cell =
  [ permuteList p [0, i, i'*phi, i''*phi*phi] | i <- pm, i' <- pm, i'' <- pm, p <- permutations4, isEvenPermutation p]
  where
    permutations4 = permutations 4
    phi = (1 + sqrt 5) / 2
    pm = [-1,1]

dodecaplex :: [[Double]]
dodecaplex = nub $
  [permuteList p [0.0, 0.0, i, j] | i <- pm2 , j <- pm2,  p <- perms4] ++
  [permuteList p [i, j, k, l] | i <- pm, j <- pm, k <-pm, l <- pmsqrt5, p <- perms4] ++
  [permuteList p [i ,j ,k ,l] | i <- pmphipowminus2, j <- pmphi, k <- pmphi, l <- pmphi, p <- perms4] ++
  [permuteList p [i ,j ,k ,l] | i <- pmphipowminus1, j <- pmphipowminus1, k <- pmphipowminus1, l <- pmphipow2, p <- perms4] ++
  [permuteList p [0, i, j, k] | i <- pmphipowminus2, j <- pm, k <- pmphipow2, p <- perms4, isEvenPermutation p] ++
  [permuteList p [0, i, j, k] | i <- pmphipowminus1, j <- pmphi, k <- pmsqrt5, p <- perms4, isEvenPermutation p] ++
  [permuteList p [i, j, k, l] | i <- pmphipowminus1, j <- pm, k <- pmphi, l <- pm2, p <- perms4, isEvenPermutation p]
  where
    pm = [-1.0,1.0]
    pm2 = [-2, 2]
    perms4 = permutations 4
    pmsqrt5 = [-sqrt 5, sqrt 5]
    pmphipowminus2 = [-1/phi/phi, 1/phi/phi]
    pmphipow2 = [-phi*phi, phi*phi]
    phi = (1 + sqrt 5) / 2
--    pmphiminus1 = phi-1
    pmphi = [phi, -phi]
    pmphipowminus1 = [-1/phi, 1/phi]

spheresPack :: [[Double]]
spheresPack = [ [2,1,1], [4,1,1]
              , [1, 1 + sqrt 3, 1], [3, 1 + sqrt 3, 1], [5, 1 + sqrt 3, 1]
              , [2, 1 + 2*sqrt 3, 1], [4, 1 + 2*sqrt 3, 1]
              , [3, 1 + sqrt 3 / 3, 1 + 2*sqrt 6 / 3]
              , [2, 1 + 4*sqrt 3 / 3, 1 + 2*sqrt 6 / 3], [4, 1 + 4*sqrt 3 / 3, 1 + 2*sqrt 6 / 3]
              , [3, 1 + sqrt 3, 1 + 4*sqrt 6 /3]
              , [3, 1 + sqrt 3 / 3, 1 - 2*sqrt 6 / 3]
              , [2, 1 + 4*sqrt 3 / 3, 1 - 2*sqrt 6 / 3], [4, 1 + 4*sqrt 3 / 3, 1 - 2*sqrt 6 / 3]
              , [3, 1 + sqrt 3, 1 - 4*sqrt 6 /3] ]


randomInCircle :: Int -> IO [[Double]]
randomInCircle n = do
  g1 <- newStdGen
  let theta = map (*(2*pi)) (take n (randoms g1 :: [Double]))
  g2 <- newStdGen
  let rho   = map (/2) (take n (randoms g2 :: [Double]))
  return $ zipWith (\r a  -> [(r+0.5) * cos a, (r+0.5) * sin a]) rho theta

randomInSquare :: Int -> IO [[Double]]
randomInSquare n = do
  g <- newStdGen
  return $ chunksOf 2 (take (2*n) (randoms g :: [Double]))


randomInSphere :: Int -> IO [[Double]]
randomInSphere n = do
  g1 <- newStdGen
  let theta = map (*(2*pi)) (take n (randoms g1 :: [Double]))
  g2 <- newStdGen
  let phi   = map (*pi) (take n (randoms g2 :: [Double]))
  g3 <- newStdGen
  let rho   = take n (randoms g3 :: [Double])
  return $ zipWith3 (\r a b -> [r * cos a * sin b,
                                r * sin a * sin b,
                                r * cos b         ])
                     rho theta phi

regularSphere :: Int -> [[Double]]
regularSphere n =
  concatMap (\a -> map (s2c a) phi) theta
  where
  theta = map (*(2*pi)) [frac i n | i <- [0 .. n-1]]
  phi = map (*pi) [frac i n | i <- [1 .. n-1]]
  frac :: Int -> Int -> Double
  frac p q = realToFrac p / realToFrac q
  s2c :: Double -> Double -> [Double]
  s2c th ph = [cos th * sin ph, sin th * sin ph, cos ph]

randomOnSphere :: Int -> Double -> IO [[Double]]
randomOnSphere n r = do
  g <- newStdGen
  let x = take (2*n) (randoms g :: [Double])
  let u_ = map (*(2*pi)) (take n x)
  let v_ = drop n x
  return $ zipWith (\u v -> [r * cos u * sin (acos (2*v-1)),
                             r * sin u * sin (acos (2*v-1)),
                             r * (2*v-1)                  ]) u_ v_

randomInCube :: Int -> IO [[Double]]
randomInCube n = do
  g <- newStdGen
  return $ chunksOf 3 (take (3*n) (randoms g :: [Double]))

randomOnTorus :: Int -> Double -> Double -> IO [[Double]]
randomOnTorus n c a = do
  g <- newStdGen
  let x = take (2*n) (randoms g :: [Double])
  let u_ = map (*(2*pi)) (take n x)
  let v_ = map (*(2*pi)) (drop n x)
  return $ zipWith (\u v -> [cos u * (c + a * cos v),
                             sin u * (c + a * cos v),
                             a * sin v              ]) u_ v_

qcube1,qcube2,qcube3,qcube4,qcube5 :: [[Double]]
qcube1 =
  [ [ 0.0 , 0.6180339887498949 , 1.618033988749895 ]
  , [ -0.6180339887498949 , 1.618033988749895 , 0.0 ]
  , [ 1.618033988749895 , 0.0 , 0.6180339887498949 ]
  , [ 1.0 , 1.0 , -1.0 ]
  , [ 0.6180339887498949 , -1.618033988749895 , 0.0 ]
  , [ -1.0 , -1.0 , 1.0 ]
  , [ -1.618033988749895 , 0.0 , -0.6180339887498949 ]
  , [ 0.0 , -0.6180339887498949 , -1.618033988749895 ]
  ]
qcube2 =
  [ [ -1.0 , 1.0 , 1.0 ]
  , [ 0.6180339887498949 , 1.618033988749895 , 0.0 ]
  , [ 0.0 , 0.6180339887498949 , -1.618033988749895 ]
  , [ 0.0 , -0.6180339887498949 , 1.618033988749895 ]
  , [ 1.618033988749895 , 0.0 , 0.6180339887498949 ]
  , [ 1.0 , -1.0 , -1.0 ]
  , [ -1.618033988749895 , 0.0 , -0.6180339887498949 ]
  , [ -0.6180339887498949 , -1.618033988749895 , 0.0 ]
  ]
qcube3 =
  [ [ -0.6180339887498949 , 1.618033988749895 , 0.0 ]
  , [ 0.0 , 0.6180339887498949 , -1.618033988749895 ]
  , [ 0.0 , -0.6180339887498949 , 1.618033988749895 ]
  , [ 1.0 , 1.0 , 1.0 ]
  , [ 1.618033988749895 , 0.0 , -0.6180339887498949 ]
  , [ 0.6180339887498949 , -1.618033988749895 , 0.0 ]
  , [ -1.618033988749895 , 0.0 , 0.6180339887498949 ]
  , [ -1.0 , -1.0 , -1.0 ]
  ]
qcube4 =
  [ [ -1.0 , 1.0 , 1.0 ]
  , [ 1.0 , -1.0 , 1.0 ]
  , [ 1.0 , 1.0 , 1.0 ]
  , [ 1.0 , 1.0 , -1.0 ]
  , [ 1.0 , -1.0 , -1.0 ]
  , [ -1.0 , -1.0 , 1.0 ]
  , [ -1.0 , 1.0 , -1.0 ]
  , [ -1.0 , -1.0 , -1.0 ]
  ]
qcube5 =
  [ [ 0.0 , 0.6180339887498949 , 1.618033988749895 ]
  , [ 0.6180339887498949 , 1.618033988749895 , 0.0 ]
  , [ 1.0 , -1.0 , 1.0 ]
  , [ 1.618033988749895 , 0.0 , -0.6180339887498949 ]
  , [ -1.618033988749895 , 0.0 , 0.6180339887498949 ]
  , [ -1.0 , 1.0 , -1.0 ]
  , [ -0.6180339887498949 , -1.618033988749895 , 0.0 ]
  , [ 0.0 , -0.6180339887498949 , -1.618033988749895 ]
  ]

teapot :: [[Double]]
teapot =
  [ [-3, 1.64999997615814, 0]
  , [-2.98710989952087, 1.64999997615814, -0.0984380021691322]
  , [-2.98710989952087, 1.64999997615814, 0.0984380021691322]
  , [-2.98537993431091, 1.56731998920441, -0.0492190010845661]
  , [-2.98537993431091, 1.56731998920441, 0.0492190010845661]
  , [-2.9835000038147, 1.48308002948761, 0]
  , [-2.98188996315002, 1.72346997261047, -0.0492190010845661]
  , [-2.98188996315002, 1.72346997261047, 0.0492190010845661]
  , [-2.97656011581421, 1.79852998256683, 0]
  , [-2.97090005874634, 1.48620998859406, -0.0984380021691322]
  , [-2.97090005874634, 1.48620998859406, 0.0984380021691322]
  , [-2.96388006210327, 1.79533994197845, -0.0984380021691322]
  , [-2.96388006210327, 1.79533994197845, 0.0984380021691322]
  , [-2.96220993995667, 1.57017004489899, -0.133594006299973]
  , [-2.96220993995667, 1.57017004489899, 0.133594006299973]
  , [-2.95864009857178, 1.72056996822357, -0.133594006299973]
  , [-2.95864009857178, 1.72056996822357, 0.133594006299973]
  , [-2.95313000679016, 1.64999997615814, -0.168750002980232]
  , [-2.95313000679016, 1.64999997615814, 0.168750002980232]
  , [-2.95247006416321, 1.40374004840851, -0.0492190010845661]
  , [-2.95247006416321, 1.40374004840851, 0.0492190010845661]
  , [-2.93770003318787, 1.49447000026703, -0.168750002980232]
  , [-2.93770003318787, 1.49447000026703, 0.168750002980232]
  , [-2.93523001670837, 1.85214996337891, -0.0492190010845661]
  , [-2.93523001670837, 1.85214996337891, 0.0492190010845661]
  , [-2.93358993530273, 1.32011997699738, 0]
  , [-2.93044996261597, 1.78692996501923, -0.168750002980232]
  , [-2.93044996261597, 1.78692996501923, 0.168750002980232]
  , [-2.93037009239197, 1.41149997711182, -0.133594006299973]
  , [-2.93037009239197, 1.41149997711182, 0.133594006299973]
  , [-2.92188000679016, 1.32553005218506, -0.0984380021691322]
  , [-2.92188000679016, 1.32553005218506, 0.0984380021691322]
  , [-2.91278004646301, 1.84416997432709, -0.133594006299973]
  , [-2.91278004646301, 1.84416997432709, 0.133594006299973]
  , [-2.90625, 1.91015994548798, 0]
  , [-2.89422988891602, 1.90456998348236, -0.0984380021691322]
  , [-2.89422988891602, 1.90456998348236, 0.0984380021691322]
  , [-2.89138007164001, 1.57910001277924, -0.196875005960464]
  , [-2.89138007164001, 1.57910001277924, 0.196875005960464]
  , [-2.8909900188446, 1.33980000019073, -0.168750002980232]
  , [-2.8909900188446, 1.33980000019073, 0.168750002980232]
  , [-2.89065003395081, 1.71208000183105, -0.196875005960464]
  , [-2.89065003395081, 1.71208000183105, 0.196875005960464]
  , [-2.88346004486084, 1.24579000473022, -0.0483429990708828]
  , [-2.88346004486084, 1.24579000473022, 0.0483429990708828]
  , [-2.86346006393433, 1.25713002681732, -0.132717996835709]
  , [-2.86346006393433, 1.25713002681732, 0.132717996835709]
  , [-2.86265993118286, 1.43482995033264, -0.196875005960464]
  , [-2.86265993118286, 1.43482995033264, 0.196875005960464]
  , [-2.8625500202179, 1.88982999324799, -0.168750002980232]
  , [-2.8625500202179, 1.88982999324799, 0.168750002980232]
  , [-2.84999990463257, 1.64999997615814, -0.224999994039536]
  , [-2.84999990463257, 1.64999997615814, 0.224999994039536]
  , [-2.84970998764038, 1.16155004501343, 0]
  , [-2.84710001945496, 1.82081997394562, -0.196875005960464]
  , [-2.84710001945496, 1.82081997394562, 0.196875005960464]
  , [-2.84193992614746, 1.94692003726959, -0.0492190010845661]
  , [-2.84193992614746, 1.94692003726959, 0.0492190010845661]
  , [-2.8289999961853, 1.76139998435974, -0.224999994039536]
  , [-2.8289999961853, 1.76139998435974, 0.224999994039536]
  , [-2.82867002487183, 1.17597997188568, -0.0949330031871796]
  , [-2.82867002487183, 1.17597997188568, 0.0949330031871796]
  , [-2.82470011711121, 1.52193999290466, -0.224999994039536]
  , [-2.82470011711121, 1.52193999290466, 0.224999994039536]
  , [-2.82115006446838, 1.93519997596741, -0.133594006299973]
  , [-2.82115006446838, 1.93519997596741, 0.133594006299973]
  , [-2.81230998039246, 1.18719005584717, -0.168750002980232]
  , [-2.81230998039246, 1.18719005584717, 0.168750002980232]
  , [-2.80501008033752, 1.28997004032135, -0.196875005960464]
  , [-2.80501008033752, 1.28997004032135, 0.196875005960464]
  , [-2.79727005958557, 1.38311004638672, -0.224999994039536]
  , [-2.79727005958557, 1.38311004638672, 0.224999994039536]
  , [-2.78906011581421, 1.99013996124268, 0]
  , [-2.78836011886597, 1.69931995868683, -0.196875005960464]
  , [-2.78836011886597, 1.69931995868683, 0.196875005960464]
  , [-2.77820992469788, 1.98283004760742, -0.0984380021691322]
  , [-2.77820992469788, 1.98283004760742, 0.0984380021691322]
  , [-2.77442002296448, 1.52737998962402, -0.196875005960464]
  , [-2.77442002296448, 1.52737998962402, 0.196875005960464]
  , [-2.77356004714966, 1.09860002994537, -0.0843750014901161]
  , [-2.77356004714966, 1.09860002994537, 0.0843750014901161]
  , [-2.76641011238098, 1.84511995315552, -0.224999994039536]
  , [-2.76641011238098, 1.84511995315552, 0.224999994039536]
  , [-2.76033997535706, 1.90090000629425, -0.196875005960464]
  , [-2.76033997535706, 1.90090000629425, 0.196875005960464]
  , [-2.74959993362427, 1.96355998516083, -0.168750002980232]
  , [-2.74959993362427, 1.96355998516083, 0.168750002980232]
  , [-2.74831008911133, 1.78569996356964, -0.196875005960464]
  , [-2.74831008911133, 1.78569996356964, 0.196875005960464]
  , [-2.74688005447388, 1.64999997615814, -0.168750002980232]
  , [-2.74688005447388, 1.64999997615814, 0.168750002980232]
  , [-2.73125004768372, 1.00780999660492, 0]
  , [-2.72756004333496, 1.73587000370026, -0.168750002980232]
  , [-2.72756004333496, 1.73587000370026, 0.168750002980232]
  , [-2.72036004066467, 1.69082999229431, -0.133594006299973]
  , [-2.72036004066467, 1.69082999229431, 0.133594006299973]
  , [-2.71948003768921, 1.24977004528046, -0.224999994039536]
  , [-2.71948003768921, 1.24977004528046, 0.224999994039536]
  , [-2.71677994728088, 1.14468002319336, -0.196875005960464]
  , [-2.71677994728088, 1.14468002319336, 0.196875005960464]
  , [-2.71288990974426, 1.64999997615814, -0.0984380021691322]
  , [-2.71288990974426, 1.64999997615814, 0.0984380021691322]
  , [-2.7089900970459, 1.54176998138428, -0.133594006299973]
  , [-2.7089900970459, 1.54176998138428, 0.133594006299973]
  , [-2.70354008674622, 1.42640995979309, -0.168750002980232]
  , [-2.70354008674622, 1.42640995979309, 0.168750002980232]
  , [-2.70097994804382, 1.03784000873566, -0.168750002980232]
  , [-2.70097994804382, 1.03784000873566, 0.168750002980232]
  , [-2.70000004768372, 1.64999997615814, 0]
  , [-2.69965004920959, 2.0107901096344, -0.0483460016548634]
  , [-2.69965004920959, 2.0107901096344, 0.0483460016548634]
  , [-2.69711995124817, 1.68792998790741, -0.0492190010845661]
  , [-2.69711995124817, 1.68792998790741, 0.0492190010845661]
  , [-2.69412994384766, 1.72746002674103, -0.0984380021691322]
  , [-2.69412994384766, 1.72746002674103, 0.0984380021691322]
  , [-2.68661999702454, 1.54668998718262, -0.0492190010845661]
  , [-2.68661999702454, 1.54668998718262, 0.0492190010845661]
  , [-2.68263006210327, 1.76234996318817, -0.133594006299973]
  , [-2.68263006210327, 1.76234996318817, 0.133594006299973]
  , [-2.68147993087769, 1.9964599609375, -0.13272100687027]
  , [-2.68147993087769, 1.9964599609375, 0.13272100687027]
  , [-2.68144011497498, 1.72426998615265, 0]
  , [-2.67574000358582, 1.27084994316101, -0.196875005960464]
  , [-2.67574000358582, 1.27084994316101, 0.196875005960464]
  , [-2.67265009880066, 1.44068002700806, -0.0984380021691322]
  , [-2.67265009880066, 1.44068002700806, 0.0984380021691322]
  , [-2.67025995254517, 1.80040001869202, -0.168750002980232]
  , [-2.67025995254517, 1.80040001869202, 0.168750002980232]
  , [-2.667799949646, 1.84623003005981, -0.196875005960464]
  , [-2.667799949646, 1.84623003005981, 0.196875005960464]
  , [-2.66279006004333, 1.9050999879837, -0.224999994039536]
  , [-2.66279006004333, 1.9050999879837, 0.224999994039536]
  , [-2.66093993186951, 1.44608998298645, 0]
  , [-2.66018009185791, 1.75436997413635, -0.0492190010845661]
  , [-2.66018009185791, 1.75436997413635, 0.0492190010845661]
  , [-2.63858008384705, 1.78567004203796, -0.0984380021691322]
  , [-2.63858008384705, 1.78567004203796, 0.0984380021691322]
  , [-2.63438010215759, 1.10390996932983, -0.224999994039536]
  , [-2.63438010215759, 1.10390996932983, 0.224999994039536]
  , [-2.63073992729187, 1.95674002170563, -0.196875005960464]
  , [-2.63073992729187, 1.95674002170563, 0.196875005960464]
  , [-2.62655997276306, 1.78007996082306, 0]
  , [-2.625, 2.04375004768372, 0]
  , [-2.62463998794556, 1.30501997470856, -0.132813006639481]
  , [-2.62463998794556, 1.30501997470856, 0.132813006639481]
  , [-2.60642004013062, 1.31745004653931, -0.0484380014240742]
  , [-2.60642004013062, 1.31745004653931, 0.0484380014240742]
  , [-2.60631990432739, 2.02643990516663, -0.0949449986219406]
  , [-2.60631990432739, 2.02643990516663, 0.0949449986219406]
  , [-2.59179997444153, 2.01298999786377, -0.168750002980232]
  , [-2.59179997444153, 2.01298999786377, 0.168750002980232]
  , [-2.57172989845276, 1.83429002761841, -0.168750002980232]
  , [-2.57172989845276, 1.83429002761841, 0.168750002980232]
  , [-2.56777000427246, 1.16997003555298, -0.168750002980232]
  , [-2.56777000427246, 1.16997003555298, 0.168750002980232]
  , [-2.55460000038147, 1.18304002285004, -0.0953150019049644]
  , [-2.55460000038147, 1.18304002285004, 0.0953150019049644]
  , [-2.54975008964539, 1.89058995246887, -0.196875005960464]
  , [-2.54975008964539, 1.89058995246887, 0.196875005960464]
  , [-2.5495400428772, 0.878983974456787, -0.0843750014901161]
  , [-2.5495400428772, 0.878983974456787, 0.0843750014901161]
  , [-2.5464301109314, 1.83196997642517, -0.13272100687027]
  , [-2.5464301109314, 1.83196997642517, 0.13272100687027]
  , [-2.53749990463257, 1.20000004768372, 0]
  , [-2.52720999717712, 1.81920003890991, -0.0483460016548634]
  , [-2.52720999717712, 1.81920003890991, 0.0483460016548634]
  , [-2.51874995231628, 1.94530999660492, -0.224999994039536]
  , [-2.51874995231628, 1.94530999660492, 0.224999994039536]
  , [-2.51682996749878, 0.932671010494232, -0.196875005960464]
  , [-2.51682996749878, 0.932671010494232, 0.196875005960464]
  , [-2.47183990478516, 1.00648999214172, -0.196875005960464]
  , [-2.47183990478516, 1.00648999214172, 0.196875005960464]
  , [-2.44569993019104, 1.87764000892639, -0.168750002980232]
  , [-2.44569993019104, 1.87764000892639, 0.168750002980232]
  , [-2.43913006782532, 1.06017994880676, -0.0843750014901161]
  , [-2.43913006782532, 1.06017994880676, 0.0843750014901161]
  , [-2.43118000030518, 1.86417996883392, -0.0949449986219406]
  , [-2.43118000030518, 1.86417996883392, 0.0949449986219406]
  , [-2.41249990463257, 1.84686994552612, 0]
  , [-2.38827991485596, 0.716602027416229, 0]
  , [-2.3822500705719, 0.737662971019745, -0.0958539992570877]
  , [-2.3822500705719, 0.737662971019745, 0.0958539992570877]
  , [-2.37883996963501, 2.05202007293701, -0.0843750014901161]
  , [-2.37883996963501, 2.05202007293701, 0.0843750014901161]
  , [-2.37766003608704, 0.753679990768433, -0.168750002980232]
  , [-2.37766003608704, 0.753679990768433, 0.168750002980232]
  , [-2.36474990844727, 0.798761010169983, -0.199836000800133]
  , [-2.36474990844727, 0.798761010169983, 0.199836000800133]
  , [-2.35430002212524, 0.835254013538361, -0.224999994039536]
  , [-2.35430002212524, 0.835254013538361, 0.224999994039536]
  , [-2.34383988380432, 0.871747016906738, -0.199836000800133]
  , [-2.34383988380432, 0.871747016906738, 0.199836000800133]
  , [-2.3411500453949, 1.99971997737885, -0.196875005960464]
  , [-2.3411500453949, 1.99971997737885, 0.196875005960464]
  , [-2.33092999458313, 0.916827023029327, -0.168750002980232]
  , [-2.33092999458313, 0.916827023029327, 0.168750002980232]
  , [-2.32031011581421, 0.953905999660492, 0]
  , [-2.28931999206543, 1.9278199672699, -0.196875005960464]
  , [-2.28931999206543, 1.9278199672699, 0.196875005960464]
  , [-2.251620054245, 1.87551999092102, -0.0843750014901161]
  , [-2.251620054245, 1.87551999092102, 0.0843750014901161]
  , [-2.24741005897522, 0.882284998893738, -0.0843750014901161]
  , [-2.24741005897522, 0.882284998893738, 0.0843750014901161]
  , [-2.17362999916077, 0.844043016433716, 0]
  , [-2.16852998733521, 0.826951026916504, -0.0971840023994446]
  , [-2.16852998733521, 0.826951026916504, 0.0971840023994446]
  , [-2.16476988792419, 0.814364016056061, -0.168750002980232]
  , [-2.16476988792419, 0.814364016056061, 0.168750002980232]
  , [-2.15687990188599, 0.78669399023056, -0.187068000435829]
  , [-2.15687990188599, 0.78669399023056, 0.187068000435829]
  , [-2.15625, 2.09296989440918, 0]
  , [-2.15411996841431, 0.740520000457764, -0.215193003416061]
  , [-2.15411996841431, 0.740520000457764, 0.215193003416061]
  , [-2.15017008781433, 0.69473397731781, -0.215193003416061]
  , [-2.15017008781433, 0.69473397731781, 0.215193003416061]
  , [-2.14741992950439, 0.648559987545013, -0.187068000435829]
  , [-2.14741992950439, 0.648559987545013, 0.187068000435829]
  , [-2.14495992660522, 0.6127769947052, -0.132947996258736]
  , [-2.14495992660522, 0.6127769947052, 0.132947996258736]
  , [-2.143709897995, 0.59178900718689, -0.0485729984939098]
  , [-2.143709897995, 0.59178900718689, 0.0485729984939098]
  , [-2.14232993125916, 2.05836009979248, -0.168750002980232]
  , [-2.14232993125916, 2.05836009979248, 0.168750002980232]
  , [-2.11172008514404, 1.98222994804382, -0.224999994039536]
  , [-2.11172008514404, 1.98222994804382, 0.224999994039536]
  , [-2.08447003364563, 0.789525985717773, -0.0489050000905991]
  , [-2.08447003364563, 0.789525985717773, 0.0489050000905991]
  , [-2.08109998703003, 1.90609002113342, -0.168750002980232]
  , [-2.08109998703003, 1.90609002113342, 0.168750002980232]
  , [-2.07834005355835, 0.77038699388504, -0.133279994130135]
  , [-2.07834005355835, 0.77038699388504, 0.133279994130135]
  , [-2.06718993186951, 1.87147998809814, 0]
  , [-2, 0.75, 0]
  , [-1.99570000171661, 0.737109005451202, -0.0984380021691322]
  , [-1.99570000171661, 0.737109005451202, 0.0984380021691322]
  , [-1.98438000679016, 0.703125, -0.168750002980232]
  , [-1.98438000679016, 0.703125, 0.168750002980232]
  , [-1.97852003574371, 0.591650009155273, 0]
  , [-1.96937000751495, 0.670825004577637, -0.202656000852585]
  , [-1.96937000751495, 0.670825004577637, 0.202656000852585]
  , [-1.96835994720459, 0.655077993869781, -0.210938006639481]
  , [-1.96835994720459, 0.655077993869781, 0.210938006639481]
  , [-1.96000003814697, 0.75, -0.407499998807907]
  , [-1.96000003814697, 0.75, 0.407499998807907]
  , [-1.9587299823761, 0.925194978713989, -0.201561003923416]
  , [-1.9587299823761, 0.925194978713989, 0.201561003923416]
  , [-1.9570300579071, 1.10038995742798, 0]
  , [-1.95000004768372, 0.600000023841858, -0.224999994039536]
  , [-1.95000004768372, 0.600000023841858, 0.224999994039536]
  , [-1.93894994258881, 0.591650009155273, -0.403122991323471]
  , [-1.93894994258881, 0.591650009155273, 0.403122991323471]
  , [-1.93164002895355, 0.54492199420929, -0.210938006639481]
  , [-1.93164002895355, 0.54492199420929, 0.210938006639481]
  , [-1.93069005012512, 0.5225830078125, -0.198676005005836]
  , [-1.93069005012512, 0.5225830078125, 0.198676005005836]
  , [-1.92188000679016, 0.453516006469727, 0]
  , [-1.91788995265961, 1.10038995742798, -0.398745000362396]
  , [-1.91788995265961, 1.10038995742798, 0.398745000362396]
  , [-1.91561996936798, 0.496874988079071, -0.168750002980232]
  , [-1.91561996936798, 0.496874988079071, 0.168750002980232]
  , [-1.90429997444153, 0.462891012430191, -0.0984380021691322]
  , [-1.90429997444153, 0.462891012430191, 0.0984380021691322]
  , [-1.89999997615814, 0.449999988079071, 0]
  , [-1.89227998256683, 0.670825004577637, -0.593047022819519]
  , [-1.89227998256683, 0.670825004577637, 0.593047022819519]
  , [-1.8834400177002, 0.453516006469727, -0.391582012176514]
  , [-1.8834400177002, 0.453516006469727, 0.391582012176514]
  , [-1.88206005096436, 0.925194978713989, -0.58984500169754]
  , [-1.88206005096436, 0.925194978713989, 0.58984500169754]
  , [-1.88138997554779, 1.28612995147705, -0.193601995706558]
  , [-1.88138997554779, 1.28612995147705, 0.193601995706558]
  , [-1.85511994361877, 0.5225830078125, -0.581402003765106]
  , [-1.85511994361877, 0.5225830078125, 0.581402003765106]
  , [-1.84500002861023, 0.75, -0.785000026226044]
  , [-1.84500002861023, 0.75, 0.785000026226044]
  , [-1.84375, 1.47186994552612, 0]
  , [-1.83317005634308, 1.89067995548248, -0.0843750014901161]
  , [-1.83317005634308, 1.89067995548248, 0.0843750014901161]
  , [-1.83179998397827, 1.94649004936218, -0.196875005960464]
  , [-1.83179998397827, 1.94649004936218, 0.196875005960464]
  , [-1.82992005348206, 2.02323007583618, -0.196875005960464]
  , [-1.82992005348206, 2.02323007583618, 0.196875005960464]
  , [-1.82854998111725, 2.07904005050659, -0.0843750014901161]
  , [-1.82854998111725, 2.07904005050659, 0.0843750014901161]
  , [-1.82518005371094, 0.591650009155273, -0.776566982269287]
  , [-1.82518005371094, 0.591650009155273, 0.776566982269287]
  , [-1.81757998466492, 0.343944996595383, -0.187035992741585]
  , [-1.81757998466492, 0.343944996595383, 0.187035992741585]
  , [-1.80774998664856, 1.28612995147705, -0.566554009914398]
  , [-1.80774998664856, 1.28612995147705, 0.566554009914398]
  , [-1.8068699836731, 1.47186994552612, -0.375663995742798]
  , [-1.8068699836731, 1.47186994552612, 0.375663995742798]
  , [-1.80535995960236, 1.10038995742798, -0.768135011196136]
  , [-1.80535995960236, 1.10038995742798, 0.768135011196136]
  , [-1.77293002605438, 0.453516006469727, -0.754335999488831]
  , [-1.77293002605438, 0.453516006469727, 0.754335999488831]
  , [-1.75, 0.234375, 0]
  , [-1.74644005298615, 0.343944996595383, -0.547339022159576]
  , [-1.74644005298615, 0.343944996595383, 0.547339022159576]
  , [-1.7443300485611, 0.670825004577637, -0.949871003627777]
  , [-1.7443300485611, 0.670825004577637, 0.949871003627777]
  , [-1.7349100112915, 0.925194978713989, -0.944741010665894]
  , [-1.7349100112915, 0.925194978713989, 0.944741010665894]
  , [-1.7150000333786, 0.234375, -0.356563001871109]
  , [-1.7150000333786, 0.234375, 0.356561988592148]
  , [-1.71008002758026, 0.5225830078125, -0.931218028068542]
  , [-1.71008002758026, 0.5225830078125, 0.931218028068542]
  , [-1.70086002349854, 1.47186994552612, -0.723671972751617]
  , [-1.70086002349854, 1.47186994552612, 0.723671972751617]
  , [-1.66639995574951, 1.28612995147705, -0.907437026500702]
  , [-1.66639995574951, 1.28612995147705, 0.907437026500702]
  , [-1.66250002384186, 0.75, -1.125]
  , [-1.66250002384186, 0.75, 1.125]
  , [-1.65515995025635, 1.86093997955322, -0.170322000980377]
  , [-1.65515995025635, 1.86093997955322, 0.170322000980377]
  , [-1.64742004871368, 0.159961000084877, -0.169525995850563]
  , [-1.64742004871368, 0.159961000084877, 0.169525995850563]
  , [-1.64463996887207, 0.591650009155273, -1.11292004585266]
  , [-1.64463996887207, 0.591650009155273, 1.11292004585266]
  , [-1.62678003311157, 1.10038995742798, -1.10082995891571]
  , [-1.62678003311157, 1.10038995742798, 1.10082995891571]
  , [-1.61436998844147, 0.234375, -0.686874985694885]
  , [-1.61436998844147, 0.234375, 0.686874985694885]
  , [-1.60988998413086, 0.343944996595383, -0.876659989356995]
  , [-1.60988998413086, 0.343944996595383, 0.876659989356995]
  , [-1.60000002384186, 1.875, 0]
  , [-1.59756004810333, 0.453516006469727, -1.08106005191803]
  , [-1.59756004810333, 0.453516006469727, 1.08106005191803]
  , [-1.59037005901337, 1.86093997955322, -0.498427987098694]
  , [-1.59037005901337, 1.86093997955322, 0.498427987098694]
  , [-1.58438003063202, 1.91015994548798, -0.168750002980232]
  , [-1.58438003063202, 1.91015994548798, 0.168750002980232]
  , [-1.58293998241425, 0.159961000084877, -0.49609899520874]
  , [-1.58293998241425, 0.159961000084877, 0.49609899520874]
  , [-1.57813000679016, 0.085547000169754, 0]
  , [-1.54999995231628, 1.98749995231628, -0.224999994039536]
  , [-1.54999995231628, 1.98749995231628, 0.224999994039536]
  , [-1.54656004905701, 0.085547000169754, -0.321543008089066]
  , [-1.54656004905701, 0.085547000169754, 0.321543008089066]
  , [-1.53296995162964, 0.670825004577637, -1.26566994190216]
  , [-1.53296995162964, 0.670825004577637, 1.26566994190216]
  , [-1.53261995315552, 1.47186994552612, -1.03710997104645]
  , [-1.53261995315552, 1.47186994552612, 1.03710997104645]
  , [-1.52469003200531, 0.925194978713989, -1.25882995128632]
  , [-1.52469003200531, 0.925194978713989, 1.25882995128632]
  , [-1.52366995811462, 0.042773000895977, -0.156791999936104]
  , [-1.52366995811462, 0.042773000895977, 0.156791999936104]
  , [-1.51563000679016, 2.06484007835388, -0.168750002980232]
  , [-1.51563000679016, 2.06484007835388, 0.168750002980232]
  , [-1.50286996364594, 0.5225830078125, -1.24081003665924]
  , [-1.50286996364594, 0.5225830078125, 1.24081003665924]
  , [-1.5, 0, 0]
  , [-1.5, 2.09999990463257, 0]
  , [-1.5, 2.25, 0]
  , [-1.47000002861023, 0, -0.30562499165535]
  , [-1.47000002861023, 0, 0.30562499165535]
  , [-1.47000002861023, 2.25, -0.30562499165535]
  , [-1.47000002861023, 2.25, 0.30562499165535]
  , [-1.46601998806, 1.86093997955322, -0.79831999540329]
  , [-1.46601998806, 1.86093997955322, 0.79831999540329]
  , [-1.4644900560379, 1.28612995147705, -1.20912003517151]
  , [-1.4644900560379, 1.28612995147705, 1.20912003517151]
  , [-1.46403002738953, 0.042773000895977, -0.458833009004593]
  , [-1.46403002738953, 0.042773000895977, 0.458833009004593]
  , [-1.45985996723175, 2.28691005706787, -0.15022599697113]
  , [-1.45985996723175, 2.28691005706787, 0.15022599697113]
  , [-1.45916998386383, 0.159961000084877, -0.794589996337891]
  , [-1.45916998386383, 0.159961000084877, 0.794589996337891]
  , [-1.45581996440887, 0.085547000169754, -0.61941397190094]
  , [-1.45581996440887, 0.085547000169754, 0.61941397190094]
  , [-1.45468997955322, 0.234375, -0.984375]
  , [-1.45468997955322, 0.234375, 0.984375]
  , [-1.4492199420929, 2.32382988929749, 0]
  , [-1.42023003101349, 2.32382988929749, -0.295278012752533]
  , [-1.42023003101349, 2.32382988929749, 0.295278012752533]
  , [-1.41999995708466, 0.75, -1.41999995708466]
  , [-1.41999995708466, 0.75, 1.41999995708466]
  , [-1.41481995582581, 0.343944996595383, -1.16812002658844]
  , [-1.41481995582581, 0.343944996595383, 1.16812002658844]
  , [-1.41191005706787, 2.33612990379333, -0.14529100060463]
  , [-1.41191005706787, 2.33612990379333, 0.14529100060463]
  , [-1.40474998950958, 0.591650009155273, -1.40474998950958]
  , [-1.40474998950958, 0.591650009155273, 1.40474998950958]
  , [-1.40313005447388, 2.34843993186951, 0]
  , [-1.40271997451782, 2.28691005706787, -0.439617991447449]
  , [-1.40271997451782, 2.28691005706787, 0.439617991447449]
  , [-1.39999997615814, 2.25, 0]
  , [-1.38949000835419, 1.10038995742798, -1.38949000835419]
  , [-1.38949000835419, 1.10038995742798, 1.38949000835419]
  , [-1.38374996185303, 0, -0.588750004768372]
  , [-1.38374996185303, 0, 0.588750004768372]
  , [-1.38374996185303, 2.25, -0.588750004768372]
  , [-1.38374996185303, 2.25, 0.588750004768372]
  , [-1.38047003746033, 2.32382988929749, 0]
  , [-1.37787997722626, 2.33612990379333, -0.141789004206657]
  , [-1.37787997722626, 2.33612990379333, 0.141789004206657]
  , [-1.37633001804352, 2.28691005706787, -0.141629993915558]
  , [-1.37633001804352, 2.28691005706787, 0.141629993915558]
  , [-1.37505996227264, 2.34843993186951, -0.285887002944946]
  , [-1.37505996227264, 2.34843993186951, 0.285887002944946]
  , [-1.37199997901917, 2.25, -0.285250008106232]
  , [-1.37199997901917, 2.25, 0.285250008106232]
  , [-1.36452996730804, 0.453516006469727, -1.36452996730804]
  , [-1.36452996730804, 0.453516006469727, 1.36452996730804]
  , [-1.35664999485016, 2.33612990379333, -0.425177007913589]
  , [-1.35664999485016, 2.33612990379333, 0.425177007913589]
  , [-1.35285997390747, 2.32382988929749, -0.281271010637283]
  , [-1.35285997390747, 2.32382988929749, 0.281271010637283]
  , [-1.34957003593445, 0.042773000895977, -0.734902024269104]
  , [-1.34957003593445, 0.042773000895977, 0.734902024269104]
  , [-1.33689999580383, 2.32382988929749, -0.568817973136902]
  , [-1.33689999580383, 2.32382988929749, 0.568817973136902]
  , [-1.32395005226135, 2.33612990379333, -0.414929002523422]
  , [-1.32395005226135, 2.33612990379333, 0.414929002523422]
  , [-1.32246005535126, 2.28691005706787, -0.414463996887207]
  , [-1.32246005535126, 2.28691005706787, 0.414463996887207]
  , [-1.3118200302124, 0.085547000169754, -0.887695014476776]
  , [-1.3118200302124, 0.085547000169754, 0.887695014476776]
  , [-1.30905997753143, 1.47186994552612, -1.30905997753143]
  , [-1.30905997753143, 1.47186994552612, 1.30905997753143]
  , [-1.29999995231628, 2.25, 0]
  , [-1.2943799495697, 2.34843993186951, -0.550727009773254]
  , [-1.2943799495697, 2.34843993186951, 0.550727009773254]
  , [-1.29305005073547, 2.28691005706787, -0.704126000404358]
  , [-1.29305005073547, 2.28691005706787, 0.704126000404358]
  , [-1.29149997234344, 2.25, -0.549499988555908]
  , [-1.29149997234344, 2.25, 0.549499988555908]
  , [-1.28839004039764, 1.86093997955322, -1.06373000144958]
  , [-1.28839004039764, 1.86093997955322, 1.06373000144958]
  , [-1.28236997127533, 0.159961000084877, -1.05876004695892]
  , [-1.28236997127533, 0.159961000084877, 1.05876004695892]
  , [-1.27400004863739, 2.25, -0.264874994754791]
  , [-1.27400004863739, 2.25, 0.264874994754791]
  , [-1.27348005771637, 2.32382988929749, -0.541833996772766]
  , [-1.27348005771637, 2.32382988929749, 0.541833996772766]
  , [-1.26766002178192, 2.27489995956421, -0.130447998642921]
  , [-1.26766002178192, 2.27489995956421, 0.130447998642921]
  , [-1.26566994190216, 0.670825004577637, -1.53296995162964]
  , [-1.26566994190216, 0.670825004577637, 1.53296995162964]
  , [-1.26093995571136, 2.29979991912842, 0]
  , [-1.25882995128632, 0.925194978713989, -1.52469003200531]
  , [-1.25882995128632, 0.925194978713989, 1.52469003200531]
  , [-1.25057005882263, 2.33612990379333, -0.680997014045715]
  , [-1.25057005882263, 2.33612990379333, 0.680997014045715]
  , [-1.24688005447388, 0, -0.84375]
  , [-1.24688005447388, 0, 0.84375]
  , [-1.24688005447388, 2.25, -0.84375]
  , [-1.24688005447388, 2.25, 0.84375]
  , [-1.24249994754791, 0.234375, -1.24249994754791]
  , [-1.24249994754791, 0.234375, 1.24249994754791]
  , [-1.24081003665924, 0.5225830078125, -1.50286996364594]
  , [-1.24081003665924, 0.5225830078125, 1.50286996364594]
  , [-1.235720038414, 2.29979991912842, -0.256915986537933]
  , [-1.235720038414, 2.29979991912842, 0.256915986537933]
  , [-1.22043001651764, 2.33612990379333, -0.664583027362823]
  , [-1.22043001651764, 2.33612990379333, 0.664583027362823]
  , [-1.21905994415283, 2.28691005706787, -0.663837015628815]
  , [-1.21905994415283, 2.28691005706787, 0.663837015628815]
  , [-1.21805000305176, 2.27489995956421, -0.381740003824234]
  , [-1.21805000305176, 2.27489995956421, 0.381740003824234]
  , [-1.20912003517151, 1.28612995147705, -1.4644900560379]
  , [-1.20912003517151, 1.28612995147705, 1.4644900560379]
  , [-1.20466005802155, 2.32382988929749, -0.815186023712158]
  , [-1.20466005802155, 2.32382988929749, 0.815186023712158]
  , [-1.19924998283386, 2.25, -0.510249972343445]
  , [-1.19924998283386, 2.25, 0.510249972343445]
  , [-1.19650995731354, 2.31943011283875, -0.123125001788139]
  , [-1.19650995731354, 2.31943011283875, 0.123125001788139]
  , [-1.18604004383087, 0.042773000895977, -0.979228973388672]
  , [-1.18604004383087, 0.042773000895977, 0.979228973388672]
  , [-1.16812002658844, 0.343944996595383, -1.41481995582581]
  , [-1.16812002658844, 0.343944996595383, 1.41481995582581]
  , [-1.16635000705719, 2.34843993186951, -0.789258003234863]
  , [-1.16635000705719, 2.34843993186951, 0.789258003234863]
  , [-1.16375005245209, 2.25, -0.787500023841858]
  , [-1.16375005245209, 2.25, 0.787500023841858]
  , [-1.16322004795074, 2.29979991912842, -0.494917988777161]
  , [-1.16322004795074, 2.29979991912842, 0.494917988777161]
  , [-1.15625, 2.33906006813049, 0]
  , [-1.14968001842499, 2.31943011283875, -0.360312014818192]
  , [-1.14968001842499, 2.31943011283875, 0.360312014818192]
  , [-1.14751994609833, 2.32382988929749, -0.776513993740082]
  , [-1.14751994609833, 2.32382988929749, 0.776513993740082]
  , [-1.13636994361877, 2.28691005706787, -0.938220024108887]
  , [-1.13636994361877, 2.28691005706787, 0.938220024108887]
  , [-1.13311994075775, 2.33906006813049, -0.235586002469063]
  , [-1.13311994075775, 2.33906006813049, 0.235586002469063]
  , [-1.125, 0.75, -1.66250002384186]
  , [-1.125, 0.75, 1.66250002384186]
  , [-1.12281000614166, 2.27489995956421, -0.611424028873444]
  , [-1.12281000614166, 2.27489995956421, 0.611424028873444]
  , [-1.12047004699707, 0.085547000169754, -1.12047004699707]
  , [-1.12047004699707, 0.085547000169754, 1.12047004699707]
  , [-1.11292004585266, 0.591650009155273, -1.64463996887207]
  , [-1.11292004585266, 0.591650009155273, 1.64463996887207]
  , [-1.10082995891571, 1.10038995742798, -1.62678003311157]
  , [-1.10082995891571, 1.10038995742798, 1.62678003311157]
  , [-1.09904003143311, 2.33612990379333, -0.907401978969574]
  , [-1.09904003143311, 2.33612990379333, 0.907401978969574]
  , [-1.08106005191803, 0.453516006469727, -1.59756004810333]
  , [-1.08106005191803, 0.453516006469727, 1.59756004810333]
  , [-1.08062994480133, 2.25, -0.731249988079071]
  , [-1.08062994480133, 2.25, 0.731249988079071]
  , [-1.07255005836487, 2.33612990379333, -0.885531008243561]
  , [-1.07255005836487, 2.33612990379333, 0.885531008243561]
  , [-1.07134997844696, 2.28691005706787, -0.884536981582642]
  , [-1.07134997844696, 2.28691005706787, 0.884536981582642]
  , [-1.06664001941681, 2.33906006813049, -0.453828006982803]
  , [-1.06664001941681, 2.33906006813049, 0.453828006982803]
  , [-1.06500005722046, 0, -1.06500005722046]
  , [-1.06500005722046, 0, 1.06500005722046]
  , [-1.06500005722046, 2.25, -1.06500005722046]
  , [-1.06500005722046, 2.25, 1.06500005722046]
  , [-1.06373000144958, 1.86093997955322, -1.28839004039764]
  , [-1.06373000144958, 1.86093997955322, 1.28839004039764]
  , [-1.05979001522064, 2.31943011283875, -0.577103972434998]
  , [-1.05979001522064, 2.31943011283875, 0.577103972434998]
  , [-1.05876004695892, 0.159961000084877, -1.28236997127533]
  , [-1.05876004695892, 0.159961000084877, 1.28236997127533]
  , [-1.04814994335175, 2.29979991912842, -0.709276974201202]
  , [-1.04814994335175, 2.29979991912842, 0.709276974201202]
  , [-1.03710997104645, 1.47186994552612, -1.53261995315552]
  , [-1.03710997104645, 1.47186994552612, 1.53261995315552]
  , [-1.02893996238708, 2.32382988929749, -1.02893996238708]
  , [-1.02893996238708, 2.32382988929749, 1.02893996238708]
  , [-0.996218979358673, 2.34843993186951, -0.996218979358673]
  , [-0.996218979358673, 2.34843993186951, 0.996218979358673]
  , [-0.994000017642975, 2.25, -0.994000017642975]
  , [-0.994000017642975, 2.25, 0.994000017642975]
  , [-0.986760973930359, 2.27489995956421, -0.814697980880737]
  , [-0.986760973930359, 2.27489995956421, 0.814697980880737]
  , [-0.984375, 0.234375, -1.45468997955322]
  , [-0.984375, 0.234375, 1.45468997955322]
  , [-0.980718970298767, 2.36952996253967, -0.100919999182224]
  , [-0.980718970298767, 2.36952996253967, 0.100919999182224]
  , [-0.98013299703598, 2.32382988929749, -0.98013299703598]
  , [-0.98013299703598, 2.32382988929749, 0.98013299703598]
  , [-0.979228973388672, 0.042773000895977, -1.18604004383087]
  , [-0.979228973388672, 0.042773000895977, 1.18604004383087]
  , [-0.961133003234863, 2.33906006813049, -0.650390982627869]
  , [-0.961133003234863, 2.33906006813049, 0.650390982627869]
  , [-0.949871003627777, 0.670825004577637, -1.7443300485611]
  , [-0.949871003627777, 0.670825004577637, 1.7443300485611]
  , [-0.944741010665894, 0.925194978713989, -1.7349100112915]
  , [-0.944741010665894, 0.925194978713989, 1.7349100112915]
  , [-0.942332029342651, 2.36952996253967, -0.295329988002777]
  , [-0.942332029342651, 2.36952996253967, 0.295329988002777]
  , [-0.938220024108887, 2.28691005706787, -1.13636994361877]
  , [-0.938220024108887, 2.28691005706787, 1.13636994361877]
  , [-0.931373000144958, 2.31943011283875, -0.768967986106873]
  , [-0.931373000144958, 2.31943011283875, 0.768967986106873]
  , [-0.931218028068542, 0.5225830078125, -1.71008002758026]
  , [-0.931218028068542, 0.5225830078125, 1.71008002758026]
  , [-0.922999978065491, 2.25, -0.922999978065491]
  , [-0.922999978065491, 2.25, 0.922999978065491]
  , [-0.907437026500702, 1.28612995147705, -1.66639995574951]
  , [-0.907437026500702, 1.28612995147705, 1.66639995574951]
  , [-0.907401978969574, 2.33612990379333, -1.09904003143311]
  , [-0.907401978969574, 2.33612990379333, 1.09904003143311]
  , [-0.895265996456146, 2.29979991912842, -0.895265996456146]
  , [-0.895265996456146, 2.29979991912842, 0.895265996456146]
  , [-0.887695014476776, 0.085547000169754, -1.3118200302124]
  , [-0.887695014476776, 0.085547000169754, 1.3118200302124]
  , [-0.885531008243561, 2.33612990379333, -1.07255005836487]
  , [-0.885531008243561, 2.33612990379333, 1.07255005836487]
  , [-0.884536981582642, 2.28691005706787, -1.07134997844696]
  , [-0.884536981582642, 2.28691005706787, 1.07134997844696]
  , [-0.876659989356995, 0.343944996595383, -1.60988998413086]
  , [-0.876659989356995, 0.343944996595383, 1.60988998413086]
  , [-0.868654012680054, 2.36952996253967, -0.473022997379303]
  , [-0.868654012680054, 2.36952996253967, 0.473022997379303]
  , [-0.84375, 0, -1.24688005447388]
  , [-0.84375, 0, 1.24688005447388]
  , [-0.84375, 2.25, -1.24688005447388]
  , [-0.84375, 2.25, 1.24688005447388]
  , [-0.824999988079071, 2.40000009536743, 0]
  , [-0.820937991142273, 2.33906006813049, -0.820937991142273]
  , [-0.820937991142273, 2.33906006813049, 0.820937991142273]
  , [-0.815186023712158, 2.32382988929749, -1.20466005802155]
  , [-0.815186023712158, 2.32382988929749, 1.20466005802155]
  , [-0.814697980880737, 2.27489995956421, -0.986760973930359]
  , [-0.814697980880737, 2.27489995956421, 0.986760973930359]
  , [-0.808499991893768, 2.40000009536743, -0.168093994259834]
  , [-0.808499991893768, 2.40000009536743, 0.168093994259834]
  , [-0.79831999540329, 1.86093997955322, -1.46601998806]
  , [-0.79831999540329, 1.86093997955322, 1.46601998806]
  , [-0.794589996337891, 0.159961000084877, -1.45916998386383]
  , [-0.794589996337891, 0.159961000084877, 1.45916998386383]
  , [-0.789258003234863, 2.34843993186951, -1.16635000705719]
  , [-0.789258003234863, 2.34843993186951, 1.16635000705719]
  , [-0.787500023841858, 2.25, -1.16375005245209]
  , [-0.787500023841858, 2.25, 1.16375005245209]
  , [-0.785000026226044, 0.75, -1.84500002861023]
  , [-0.785000026226044, 0.75, 1.84500002861023]
  , [-0.776566982269287, 0.591650009155273, -1.82518005371094]
  , [-0.776566982269287, 0.591650009155273, 1.82518005371094]
  , [-0.776513993740082, 2.32382988929749, -1.14751994609833]
  , [-0.776513993740082, 2.32382988929749, 1.14751994609833]
  , [-0.768967986106873, 2.31943011283875, -0.931373000144958]
  , [-0.768967986106873, 2.31943011283875, 0.931373000144958]
  , [-0.768135011196136, 1.10038995742798, -1.80535995960236]
  , [-0.768135011196136, 1.10038995742798, 1.80535995960236]
  , [-0.763400018215179, 2.36952996253967, -0.630285024642944]
  , [-0.763400018215179, 2.36952996253967, 0.630285024642944]
  , [-0.761062979698181, 2.40000009536743, -0.323812991380692]
  , [-0.761062979698181, 2.40000009536743, 0.323812991380692]
  , [-0.754335999488831, 0.453516006469727, -1.77293002605438]
  , [-0.754335999488831, 0.453516006469727, 1.77293002605438]
  , [-0.734902024269104, 0.042773000895977, -1.34957003593445]
  , [-0.734902024269104, 0.042773000895977, 1.34957003593445]
  , [-0.731249988079071, 2.25, -1.08062994480133]
  , [-0.731249988079071, 2.25, 1.08062994480133]
  , [-0.723671972751617, 1.47186994552612, -1.70086002349854]
  , [-0.723671972751617, 1.47186994552612, 1.70086002349854]
  , [-0.709276974201202, 2.29979991912842, -1.04814994335175]
  , [-0.709276974201202, 2.29979991912842, 1.04814994335175]
  , [-0.704126000404358, 2.28691005706787, -1.29305005073547]
  , [-0.704126000404358, 2.28691005706787, 1.29305005073547]
  , [-0.686874985694885, 0.234375, -1.61436998844147]
  , [-0.686874985694885, 0.234375, 1.61436998844147]
  , [-0.685781002044678, 2.40000009536743, -0.464062988758087]
  , [-0.685781002044678, 2.40000009536743, 0.464062988758087]
  , [-0.680997014045715, 2.33612990379333, -1.25057005882263]
  , [-0.680997014045715, 2.33612990379333, 1.25057005882263]
  , [-0.664583027362823, 2.33612990379333, -1.22043001651764]
  , [-0.664583027362823, 2.33612990379333, 1.22043001651764]
  , [-0.663837015628815, 2.28691005706787, -1.21905994415283]
  , [-0.663837015628815, 2.28691005706787, 1.21905994415283]
  , [-0.650390982627869, 2.33906006813049, -0.961133003234863]
  , [-0.650390982627869, 2.33906006813049, 0.961133003234863]
  , [-0.631998002529144, 2.43046998977661, -0.0648249983787537]
  , [-0.631998002529144, 2.43046998977661, 0.0648249983787537]
  , [-0.630285024642944, 2.36952996253967, -0.763400018215179]
  , [-0.630285024642944, 2.36952996253967, 0.763400018215179]
  , [-0.61941397190094, 0.085547000169754, -1.45581996440887]
  , [-0.61941397190094, 0.085547000169754, 1.45581996440887]
  , [-0.611424028873444, 2.27489995956421, -1.12281000614166]
  , [-0.611424028873444, 2.27489995956421, 1.12281000614166]
  , [-0.607173979282379, 2.43046998977661, -0.190548002719879]
  , [-0.607173979282379, 2.43046998977661, 0.190548002719879]
  , [-0.593047022819519, 0.670825004577637, -1.89227998256683]
  , [-0.593047022819519, 0.670825004577637, 1.89227998256683]
  , [-0.58984500169754, 0.925194978713989, -1.88206005096436]
  , [-0.58984500169754, 0.925194978713989, 1.88206005096436]
  , [-0.588750004768372, 0, -1.38374996185303]
  , [-0.588750004768372, 0, 1.38374996185303]
  , [-0.588750004768372, 2.25, -1.38374996185303]
  , [-0.588750004768372, 2.25, 1.38374996185303]
  , [-0.585749983787537, 2.40000009536743, -0.585749983787537]
  , [-0.585749983787537, 2.40000009536743, 0.585749983787537]
  , [-0.581402003765106, 0.5225830078125, -1.85511994361877]
  , [-0.581402003765106, 0.5225830078125, 1.85511994361877]
  , [-0.577103972434998, 2.31943011283875, -1.05979001522064]
  , [-0.577103972434998, 2.31943011283875, 1.05979001522064]
  , [-0.568817973136902, 2.32382988929749, -1.33689999580383]
  , [-0.568817973136902, 2.32382988929749, 1.33689999580383]
  , [-0.566554009914398, 1.28612995147705, -1.80774998664856]
  , [-0.566554009914398, 1.28612995147705, 1.80774998664856]
  , [-0.559973001480103, 2.43046998977661, -0.304711014032364]
  , [-0.559973001480103, 2.43046998977661, 0.304711014032364]
  , [-0.550727009773254, 2.34843993186951, -1.2943799495697]
  , [-0.550727009773254, 2.34843993186951, 1.2943799495697]
  , [-0.549499988555908, 2.25, -1.29149997234344]
  , [-0.549499988555908, 2.25, 1.29149997234344]
  , [-0.547339022159576, 0.343944996595383, -1.74644005298615]
  , [-0.547339022159576, 0.343944996595383, 1.74644005298615]
  , [-0.541833996772766, 2.32382988929749, -1.27348005771637]
  , [-0.541833996772766, 2.32382988929749, 1.27348005771637]
  , [-0.510249972343445, 2.25, -1.19924998283386]
  , [-0.510249972343445, 2.25, 1.19924998283386]
  , [-0.498427987098694, 1.86093997955322, -1.59037005901337]
  , [-0.498427987098694, 1.86093997955322, 1.59037005901337]
  , [-0.49609899520874, 0.159961000084877, -1.58293998241425]
  , [-0.49609899520874, 0.159961000084877, 1.58293998241425]
  , [-0.494917988777161, 2.29979991912842, -1.16322004795074]
  , [-0.494917988777161, 2.29979991912842, 1.16322004795074]
  , [-0.491907000541687, 2.43046998977661, -0.4064100086689]
  , [-0.491907000541687, 2.43046998977661, 0.4064100086689]
  , [-0.473022997379303, 2.36952996253967, -0.868654012680054]
  , [-0.473022997379303, 2.36952996253967, 0.868654012680054]
  , [-0.464062988758087, 2.40000009536743, -0.685781002044678]
  , [-0.464062988758087, 2.40000009536743, 0.685781002044678]
  , [-0.458833009004593, 0.042773000895977, -1.46403002738953]
  , [-0.458833009004593, 0.042773000895977, 1.46403002738953]
  , [-0.456250011920929, 2.46093988418579, 0]
  , [-0.453828006982803, 2.33906006813049, -1.06664001941681]
  , [-0.453828006982803, 2.33906006813049, 1.06664001941681]
  , [-0.439617991447449, 2.28691005706787, -1.40271997451782]
  , [-0.439617991447449, 2.28691005706787, 1.40271997451782]
  , [-0.438241004943848, 2.46093988418579, -0.0912069976329803]
  , [-0.438241004943848, 2.46093988418579, 0.0912069976329803]
  , [-0.425177007913589, 2.33612990379333, -1.35664999485016]
  , [-0.425177007913589, 2.33612990379333, 1.35664999485016]
  , [-0.420890986919403, 2.46093988418579, -0.179077997803688]
  , [-0.420890986919403, 2.46093988418579, 0.179077997803688]
  , [-0.414929002523422, 2.33612990379333, -1.32395005226135]
  , [-0.414929002523422, 2.33612990379333, 1.32395005226135]
  , [-0.414463996887207, 2.28691005706787, -1.32246005535126]
  , [-0.414463996887207, 2.28691005706787, 1.32246005535126]
  , [-0.407499998807907, 0.75, -1.96000003814697]
  , [-0.407499998807907, 0.75, 1.96000003814697]
  , [-0.4064100086689, 2.43046998977661, -0.491907000541687]
  , [-0.4064100086689, 2.43046998977661, 0.491907000541687]
  , [-0.403122991323471, 0.591650009155273, -1.93894994258881]
  , [-0.403122991323471, 0.591650009155273, 1.93894994258881]
  , [-0.398745000362396, 1.10038995742798, -1.91788995265961]
  , [-0.398745000362396, 1.10038995742798, 1.91788995265961]
  , [-0.391582012176514, 0.453516006469727, -1.8834400177002]
  , [-0.391582012176514, 0.453516006469727, 1.8834400177002]
  , [-0.381740003824234, 2.27489995956421, -1.21805000305176]
  , [-0.381740003824234, 2.27489995956421, 1.21805000305176]
  , [-0.375663995742798, 1.47186994552612, -1.8068699836731]
  , [-0.375663995742798, 1.47186994552612, 1.8068699836731]
  , [-0.372159004211426, 2.46093988418579, -0.251888990402222]
  , [-0.372159004211426, 2.46093988418579, 0.251888990402222]
  , [-0.362109005451202, 2.8971700668335, 0]
  , [-0.360312014818192, 2.31943011283875, -1.14968001842499]
  , [-0.360312014818192, 2.31943011283875, 1.14968001842499]
  , [-0.356563001871109, 0.234375, 1.7150000333786]
  , [-0.356561988592148, 0.234375, -1.7150000333786]
  , [-0.340624988079071, 2.95077991485596, 0]
  , [-0.337859004735947, 2.92396998405457, -0.0692780017852783]
  , [-0.337859004735947, 2.92396998405457, 0.0692780017852783]
  , [-0.334237992763519, 2.8971700668335, -0.142704993486404]
  , [-0.334237992763519, 2.8971700668335, 0.142704993486404]
  , [-0.33032500743866, 2.8642098903656, -0.067671999335289]
  , [-0.33032500743866, 2.8642098903656, 0.067671999335289]
  , [-0.324999988079071, 2.83124995231628, 0]
  , [-0.323938012123108, 2.46093988418579, -0.323938012123108]
  , [-0.323938012123108, 2.46093988418579, 0.323938012123108]
  , [-0.323812991380692, 2.40000009536743, -0.761062979698181]
  , [-0.323812991380692, 2.40000009536743, 0.761062979698181]
  , [-0.321543008089066, 0.085547000169754, -1.54656004905701]
  , [-0.321543008089066, 0.085547000169754, 1.54656004905701]
  , [-0.315409988164902, 2.50547003746033, -0.0643950030207634]
  , [-0.315409988164902, 2.50547003746033, 0.0643950030207634]
  , [-0.314464002847672, 2.95077991485596, -0.134406998753548]
  , [-0.314464002847672, 2.95077991485596, 0.134406998753548]
  , [-0.30562499165535, 0, -1.47000002861023]
  , [-0.30562499165535, 0, 1.47000002861023]
  , [-0.30562499165535, 2.25, -1.47000002861023]
  , [-0.30562499165535, 2.25, 1.47000002861023]
  , [-0.304711014032364, 2.43046998977661, -0.559973001480103]
  , [-0.304711014032364, 2.43046998977661, 0.559973001480103]
  , [-0.299953013658524, 2.83124995231628, -0.127984002232552]
  , [-0.299953013658524, 2.83124995231628, 0.127984002232552]
  , [-0.295329988002777, 2.36952996253967, -0.942332029342651]
  , [-0.295329988002777, 2.36952996253967, 0.942332029342651]
  , [-0.295278012752533, 2.32382988929749, -1.42023003101349]
  , [-0.295278012752533, 2.32382988929749, 1.42023003101349]
  , [-0.28719699382782, 2.92396998405457, -0.1942999958992]
  , [-0.28719699382782, 2.92396998405457, 0.1942999958992]
  , [-0.285887002944946, 2.34843993186951, -1.37505996227264]
  , [-0.285887002944946, 2.34843993186951, 1.37505996227264]
  , [-0.285250008106232, 2.25, -1.37199997901917]
  , [-0.285250008106232, 2.25, 1.37199997901917]
  , [-0.281271010637283, 2.32382988929749, -1.35285997390747]
  , [-0.281271010637283, 2.32382988929749, 1.35285997390747]
  , [-0.280732005834579, 2.8642098903656, -0.189855992794037]
  , [-0.280732005834579, 2.8642098903656, 0.189855992794037]
  , [-0.274421006441116, 2.96880006790161, -0.0563799999654293]
  , [-0.274421006441116, 2.96880006790161, 0.0563799999654293]
  , [-0.267832010984421, 2.50547003746033, -0.18087899684906]
  , [-0.267832010984421, 2.50547003746033, 0.18087899684906]
  , [-0.264874994754791, 2.25, -1.27400004863739]
  , [-0.264874994754791, 2.25, 1.27400004863739]
  , [-0.257609993219376, 2.8971700668335, -0.257609993219376]
  , [-0.257609993219376, 2.8971700668335, 0.257609993219376]
  , [-0.256915986537933, 2.29979991912842, -1.235720038414]
  , [-0.256915986537933, 2.29979991912842, 1.235720038414]
  , [-0.251888990402222, 2.46093988418579, -0.372159004211426]
  , [-0.251888990402222, 2.46093988418579, 0.372159004211426]
  , [-0.250871986150742, 2.7574200630188, -0.0513469986617565]
  , [-0.250871986150742, 2.7574200630188, 0.0513469986617565]
  , [-0.242476999759674, 2.95077991485596, -0.242476999759674]
  , [-0.242476999759674, 2.95077991485596, 0.242476999759674]
  , [-0.235586002469063, 2.33906006813049, -1.13311994075775]
  , [-0.235586002469063, 2.33906006813049, 1.13311994075775]
  , [-0.233382001519203, 2.96880006790161, -0.158017992973328]
  , [-0.233382001519203, 2.96880006790161, 0.158017992973328]
  , [-0.231124997138977, 2.83124995231628, -0.231124997138977]
  , [-0.231124997138977, 2.83124995231628, 0.231124997138977]
  , [-0.230077996850014, 2.98681998252869, 0]
  , [-0.213158994913101, 2.7574200630188, -0.14410300552845]
  , [-0.213158994913101, 2.7574200630188, 0.14410300552845]
  , [-0.212515994906425, 2.98681998252869, -0.0911130011081696]
  , [-0.212515994906425, 2.98681998252869, 0.0911130011081696]
  , [-0.202656000852585, 0.670825004577637, -1.96937000751495]
  , [-0.202656000852585, 0.670825004577637, 1.96937000751495]
  , [-0.201561003923416, 0.925194978713989, -1.9587299823761]
  , [-0.201561003923416, 0.925194978713989, 1.9587299823761]
  , [-0.200000002980232, 2.54999995231628, 0]
  , [-0.198676005005836, 0.5225830078125, -1.93069005012512]
  , [-0.198676005005836, 0.5225830078125, 1.93069005012512]
  , [-0.196875005960464, 2.68358993530273, 0]
  , [-0.1942999958992, 2.92396998405457, -0.28719699382782]
  , [-0.1942999958992, 2.92396998405457, 0.28719699382782]
  , [-0.193601995706558, 1.28612995147705, -1.88138997554779]
  , [-0.193601995706558, 1.28612995147705, 1.88138997554779]
  , [-0.190548002719879, 2.43046998977661, -0.607173979282379]
  , [-0.190548002719879, 2.43046998977661, 0.607173979282379]
  , [-0.189855992794037, 2.8642098903656, -0.280732005834579]
  , [-0.189855992794037, 2.8642098903656, 0.280732005834579]
  , [-0.187035992741585, 0.343944996595383, -1.81757998466492]
  , [-0.187035992741585, 0.343944996595383, 1.81757998466492]
  , [-0.184499993920326, 2.54999995231628, -0.0785000026226044]
  , [-0.184499993920326, 2.54999995231628, 0.0785000026226044]
  , [-0.181660994887352, 2.68358993530273, -0.0774049982428551]
  , [-0.181660994887352, 2.68358993530273, 0.0774049982428551]
  , [-0.18087899684906, 2.50547003746033, -0.267832010984421]
  , [-0.18087899684906, 2.50547003746033, 0.267832010984421]
  , [-0.179077997803688, 2.46093988418579, -0.420890986919403]
  , [-0.179077997803688, 2.46093988418579, 0.420890986919403]
  , [-0.17629499733448, 2.58119988441467, -0.0360010005533695]
  , [-0.17629499733448, 2.58119988441467, 0.0360010005533695]
  , [-0.174804002046585, 2.64800000190735, -0.0357270017266273]
  , [-0.174804002046585, 2.64800000190735, 0.0357270017266273]
  , [-0.170322000980377, 1.86093997955322, -1.65515995025635]
  , [-0.170322000980377, 1.86093997955322, 1.65515995025635]
  , [-0.169525995850563, 0.159961000084877, -1.64742004871368]
  , [-0.169525995850563, 0.159961000084877, 1.64742004871368]
  , [-0.168093994259834, 2.40000009536743, -0.808499991893768]
  , [-0.168093994259834, 2.40000009536743, 0.808499991893768]
  , [-0.166796997189522, 2.61240005493164, 0]
  , [-0.164073005318642, 2.98681998252869, -0.164073005318642]
  , [-0.164073005318642, 2.98681998252869, 0.164073005318642]
  , [-0.158017992973328, 2.96880006790161, -0.233382001519203]
  , [-0.158017992973328, 2.96880006790161, 0.233382001519203]
  , [-0.156791999936104, 0.042773000895977, -1.52366995811462]
  , [-0.156791999936104, 0.042773000895977, 1.52366995811462]
  , [-0.153881996870041, 2.61240005493164, -0.0655039995908737]
  , [-0.153881996870041, 2.61240005493164, 0.0655039995908737]
  , [-0.15022599697113, 2.28691005706787, -1.45985996723175]
  , [-0.15022599697113, 2.28691005706787, 1.45985996723175]
  , [-0.14970999956131, 2.58119988441467, -0.101116001605988]
  , [-0.14970999956131, 2.58119988441467, 0.101116001605988]
  , [-0.148475006222725, 2.64800000190735, -0.100316002964973]
  , [-0.148475006222725, 2.64800000190735, 0.100316002964973]
  , [-0.14529100060463, 2.33612990379333, -1.41191005706787]
  , [-0.14529100060463, 2.33612990379333, 1.41191005706787]
  , [-0.14410300552845, 2.7574200630188, -0.213158994913101]
  , [-0.14410300552845, 2.7574200630188, 0.213158994913101]
  , [-0.142704993486404, 2.8971700668335, -0.334237992763519]
  , [-0.142704993486404, 2.8971700668335, 0.334237992763519]
  , [-0.142000004649162, 2.54999995231628, -0.142000004649162]
  , [-0.142000004649162, 2.54999995231628, 0.142000004649162]
  , [-0.141789004206657, 2.33612990379333, -1.37787997722626]
  , [-0.141789004206657, 2.33612990379333, 1.37787997722626]
  , [-0.141629993915558, 2.28691005706787, -1.37633001804352]
  , [-0.141629993915558, 2.28691005706787, 1.37633001804352]
  , [-0.139898002147675, 2.68358993530273, -0.139898002147675]
  , [-0.139898002147675, 2.68358993530273, 0.139898002147675]
  , [-0.134406998753548, 2.95077991485596, -0.314464002847672]
  , [-0.134406998753548, 2.95077991485596, 0.314464002847672]
  , [-0.130447998642921, 2.27489995956421, -1.26766002178192]
  , [-0.130447998642921, 2.27489995956421, 1.26766002178192]
  , [-0.127984002232552, 2.83124995231628, -0.299953013658524]
  , [-0.127984002232552, 2.83124995231628, 0.299953013658524]
  , [-0.123125001788139, 2.31943011283875, -1.19650995731354]
  , [-0.123125001788139, 2.31943011283875, 1.19650995731354]
  , [-0.11845800280571, 2.61240005493164, -0.11845800280571]
  , [-0.11845800280571, 2.61240005493164, 0.11845800280571]
  , [-0.11064899712801, 2.99341011047363, -0.0227780006825924]
  , [-0.11064899712801, 2.99341011047363, 0.0227780006825924]
  , [-0.101116001605988, 2.58119988441467, -0.14970999956131]
  , [-0.101116001605988, 2.58119988441467, 0.14970999956131]
  , [-0.100919999182224, 2.36952996253967, -0.980718970298767]
  , [-0.100919999182224, 2.36952996253967, 0.980718970298767]
  , [-0.100316002964973, 2.64800000190735, -0.148475006222725]
  , [-0.100316002964973, 2.64800000190735, 0.148475006222725]
  , [-0.0941469967365265, 2.99341011047363, -0.0637969970703125]
  , [-0.0941469967365265, 2.99341011047363, 0.0637969970703125]
  , [-0.0912069976329803, 2.46093988418579, -0.438241004943848]
  , [-0.0912069976329803, 2.46093988418579, 0.438241004943848]
  , [-0.0911130011081696, 2.98681998252869, -0.212515994906425]
  , [-0.0911130011081696, 2.98681998252869, 0.212515994906425]
  , [-0.0785000026226044, 2.54999995231628, -0.184499993920326]
  , [-0.0785000026226044, 2.54999995231628, 0.184499993920326]
  , [-0.0774049982428551, 2.68358993530273, -0.181660994887352]
  , [-0.0774049982428551, 2.68358993530273, 0.181660994887352]
  , [-0.0692780017852783, 2.92396998405457, -0.337859004735947]
  , [-0.0692780017852783, 2.92396998405457, 0.337859004735947]
  , [-0.067671999335289, 2.8642098903656, -0.33032500743866]
  , [-0.067671999335289, 2.8642098903656, 0.33032500743866]
  , [-0.0655039995908737, 2.61240005493164, -0.153881996870041]
  , [-0.0655039995908737, 2.61240005493164, 0.153881996870041]
  , [-0.0648249983787537, 2.43046998977661, -0.631998002529144]
  , [-0.0648249983787537, 2.43046998977661, 0.631998002529144]
  , [-0.0643950030207634, 2.50547003746033, -0.315409988164902]
  , [-0.0643950030207634, 2.50547003746033, 0.315409988164902]
  , [-0.0637969970703125, 2.99341011047363, -0.0941469967365265]
  , [-0.0637969970703125, 2.99341011047363, 0.0941469967365265]
  , [-0.0563799999654293, 2.96880006790161, -0.274421006441116]
  , [-0.0563799999654293, 2.96880006790161, 0.274421006441116]
  , [-0.0513469986617565, 2.7574200630188, -0.250871986150742]
  , [-0.0513469986617565, 2.7574200630188, 0.250871986150742]
  , [-0.0360010005533695, 2.58119988441467, -0.17629499733448]
  , [-0.0360010005533695, 2.58119988441467, 0.17629499733448]
  , [-0.0357270017266273, 2.64800000190735, -0.174804002046585]
  , [-0.0357270017266273, 2.64800000190735, 0.174804002046585]
  , [-0.0227780006825924, 2.99341011047363, -0.11064899712801]
  , [-0.0227780006825924, 2.99341011047363, 0.11064899712801]
  , [0, 0, -1.5]
  , [0, 0, 1.5]
  , [0, 0.085547000169754, -1.57813000679016]
  , [0, 0.085547000169754, 1.57813000679016]
  , [0, 0.234375, -1.75]
  , [0, 0.234375, 1.75]
  , [0, 0.453516006469727, -1.92188000679016]
  , [0, 0.453516006469727, 1.92188000679016]
  , [0, 0.591650009155273, -1.97852003574371]
  , [0, 0.591650009155273, 1.97852003574371]
  , [0, 0.75, -2]
  , [0, 0.75, 2]
  , [0, 1.10038995742798, -1.9570300579071]
  , [0, 1.10038995742798, 1.9570300579071]
  , [0, 1.47186994552612, -1.84375]
  , [0, 1.47186994552612, 1.84375]
  , [0, 2.25, -1.5]
  , [0, 2.25, -1.39999997615814]
  , [0, 2.25, -1.29999995231628]
  , [0, 2.25, 1.29999995231628]
  , [0, 2.25, 1.39999997615814]
  , [0, 2.25, 1.5]
  , [0, 2.29979991912842, -1.26093995571136]
  , [0, 2.29979991912842, 1.26093995571136]
  , [0, 2.32382988929749, -1.4492199420929]
  , [0, 2.32382988929749, -1.38047003746033]
  , [0, 2.32382988929749, 1.38047003746033]
  , [0, 2.32382988929749, 1.4492199420929]
  , [0, 2.33906006813049, -1.15625]
  , [0, 2.33906006813049, 1.15625]
  , [0, 2.34843993186951, -1.40313005447388]
  , [0, 2.34843993186951, 1.40313005447388]
  , [0, 2.40000009536743, -0.824999988079071]
  , [0, 2.40000009536743, 0.824999988079071]
  , [0, 2.46093988418579, -0.456250011920929]
  , [0, 2.46093988418579, 0.456250011920929]
  , [0, 2.54999995231628, -0.200000002980232]
  , [0, 2.54999995231628, 0.200000002980232]
  , [0, 2.61240005493164, -0.166796997189522]
  , [0, 2.61240005493164, 0.166796997189522]
  , [0, 2.68358993530273, -0.196875005960464]
  , [0, 2.68358993530273, 0.196875005960464]
  , [0, 2.83124995231628, -0.324999988079071]
  , [0, 2.83124995231628, 0.324999988079071]
  , [0, 2.8971700668335, -0.362109005451202]
  , [0, 2.8971700668335, 0.362109005451202]
  , [0, 2.95077991485596, -0.340624988079071]
  , [0, 2.95077991485596, 0.340624988079071]
  , [0, 2.98681998252869, -0.230077996850014]
  , [0, 2.98681998252869, 0.230077996850014]
  , [0, 3, 0]
  , [0.0227780006825924, 2.99341011047363, -0.11064899712801]
  , [0.0227780006825924, 2.99341011047363, 0.11064899712801]
  , [0.0357270017266273, 2.64800000190735, -0.174804002046585]
  , [0.0357270017266273, 2.64800000190735, 0.174804002046585]
  , [0.0360010005533695, 2.58119988441467, -0.17629499733448]
  , [0.0360010005533695, 2.58119988441467, 0.17629499733448]
  , [0.0513469986617565, 2.7574200630188, -0.250871986150742]
  , [0.0513469986617565, 2.7574200630188, 0.250871986150742]
  , [0.0563799999654293, 2.96880006790161, -0.274421006441116]
  , [0.0563799999654293, 2.96880006790161, 0.274421006441116]
  , [0.0637969970703125, 2.99341011047363, -0.0941469967365265]
  , [0.0637969970703125, 2.99341011047363, 0.0941469967365265]
  , [0.0643950030207634, 2.50547003746033, -0.315409988164902]
  , [0.0643950030207634, 2.50547003746033, 0.315409988164902]
  , [0.0648249983787537, 2.43046998977661, -0.631998002529144]
  , [0.0648249983787537, 2.43046998977661, 0.631998002529144]
  , [0.0655039995908737, 2.61240005493164, -0.153881996870041]
  , [0.0655039995908737, 2.61240005493164, 0.153881996870041]
  , [0.067671999335289, 2.8642098903656, -0.33032500743866]
  , [0.067671999335289, 2.8642098903656, 0.33032500743866]
  , [0.0692780017852783, 2.92396998405457, -0.337859004735947]
  , [0.0692780017852783, 2.92396998405457, 0.337859004735947]
  , [0.0774049982428551, 2.68358993530273, -0.181660994887352]
  , [0.0774049982428551, 2.68358993530273, 0.181660994887352]
  , [0.0785000026226044, 2.54999995231628, -0.184499993920326]
  , [0.0785000026226044, 2.54999995231628, 0.184499993920326]
  , [0.0911130011081696, 2.98681998252869, -0.212515994906425]
  , [0.0911130011081696, 2.98681998252869, 0.212515994906425]
  , [0.0912069976329803, 2.46093988418579, -0.438241004943848]
  , [0.0912069976329803, 2.46093988418579, 0.438241004943848]
  , [0.0941469967365265, 2.99341011047363, -0.0637969970703125]
  , [0.0941469967365265, 2.99341011047363, 0.0637969970703125]
  , [0.100316002964973, 2.64800000190735, -0.148475006222725]
  , [0.100316002964973, 2.64800000190735, 0.148475006222725]
  , [0.100919999182224, 2.36952996253967, -0.980718970298767]
  , [0.100919999182224, 2.36952996253967, 0.980718970298767]
  , [0.101116001605988, 2.58119988441467, -0.14970999956131]
  , [0.101116001605988, 2.58119988441467, 0.14970999956131]
  , [0.11064899712801, 2.99341011047363, -0.0227780006825924]
  , [0.11064899712801, 2.99341011047363, 0.0227780006825924]
  , [0.11845800280571, 2.61240005493164, -0.11845800280571]
  , [0.11845800280571, 2.61240005493164, 0.11845800280571]
  , [0.123125001788139, 2.31943011283875, -1.19650995731354]
  , [0.123125001788139, 2.31943011283875, 1.19650995731354]
  , [0.127984002232552, 2.83124995231628, -0.299953013658524]
  , [0.127984002232552, 2.83124995231628, 0.299953013658524]
  , [0.130447998642921, 2.27489995956421, -1.26766002178192]
  , [0.130447998642921, 2.27489995956421, 1.26766002178192]
  , [0.134406998753548, 2.95077991485596, -0.314464002847672]
  , [0.134406998753548, 2.95077991485596, 0.314464002847672]
  , [0.139898002147675, 2.68358993530273, -0.139898002147675]
  , [0.139898002147675, 2.68358993530273, 0.139898002147675]
  , [0.141629993915558, 2.28691005706787, -1.37633001804352]
  , [0.141629993915558, 2.28691005706787, 1.37633001804352]
  , [0.141789004206657, 2.33612990379333, -1.37787997722626]
  , [0.141789004206657, 2.33612990379333, 1.37787997722626]
  , [0.142000004649162, 2.54999995231628, -0.142000004649162]
  , [0.142000004649162, 2.54999995231628, 0.142000004649162]
  , [0.142704993486404, 2.8971700668335, -0.334237992763519]
  , [0.142704993486404, 2.8971700668335, 0.334237992763519]
  , [0.14410300552845, 2.7574200630188, -0.213158994913101]
  , [0.14410300552845, 2.7574200630188, 0.213158994913101]
  , [0.14529100060463, 2.33612990379333, -1.41191005706787]
  , [0.14529100060463, 2.33612990379333, 1.41191005706787]
  , [0.148475006222725, 2.64800000190735, -0.100316002964973]
  , [0.148475006222725, 2.64800000190735, 0.100316002964973]
  , [0.14970999956131, 2.58119988441467, -0.101116001605988]
  , [0.14970999956131, 2.58119988441467, 0.101116001605988]
  , [0.15022599697113, 2.28691005706787, -1.45985996723175]
  , [0.15022599697113, 2.28691005706787, 1.45985996723175]
  , [0.153881996870041, 2.61240005493164, -0.0655039995908737]
  , [0.153881996870041, 2.61240005493164, 0.0655039995908737]
  , [0.156791999936104, 0.042773000895977, -1.52366995811462]
  , [0.156791999936104, 0.042773000895977, 1.52366995811462]
  , [0.158017992973328, 2.96880006790161, -0.233382001519203]
  , [0.158017992973328, 2.96880006790161, 0.233382001519203]
  , [0.164073005318642, 2.98681998252869, -0.164073005318642]
  , [0.164073005318642, 2.98681998252869, 0.164073005318642]
  , [0.166796997189522, 2.61240005493164, 0]
  , [0.168093994259834, 2.40000009536743, -0.808499991893768]
  , [0.168093994259834, 2.40000009536743, 0.808499991893768]
  , [0.169525995850563, 0.159961000084877, -1.64742004871368]
  , [0.169525995850563, 0.159961000084877, 1.64742004871368]
  , [0.170322000980377, 1.86093997955322, -1.65515995025635]
  , [0.170322000980377, 1.86093997955322, 1.65515995025635]
  , [0.174804002046585, 2.64800000190735, -0.0357270017266273]
  , [0.174804002046585, 2.64800000190735, 0.0357270017266273]
  , [0.17629499733448, 2.58119988441467, -0.0360010005533695]
  , [0.17629499733448, 2.58119988441467, 0.0360010005533695]
  , [0.179077997803688, 2.46093988418579, -0.420890986919403]
  , [0.179077997803688, 2.46093988418579, 0.420890986919403]
  , [0.18087899684906, 2.50547003746033, -0.267832010984421]
  , [0.18087899684906, 2.50547003746033, 0.267832010984421]
  , [0.181660994887352, 2.68358993530273, -0.0774049982428551]
  , [0.181660994887352, 2.68358993530273, 0.0774049982428551]
  , [0.184499993920326, 2.54999995231628, -0.0785000026226044]
  , [0.184499993920326, 2.54999995231628, 0.0785000026226044]
  , [0.187035992741585, 0.343944996595383, -1.81757998466492]
  , [0.187035992741585, 0.343944996595383, 1.81757998466492]
  , [0.189855992794037, 2.8642098903656, -0.280732005834579]
  , [0.189855992794037, 2.8642098903656, 0.280732005834579]
  , [0.190548002719879, 2.43046998977661, -0.607173979282379]
  , [0.190548002719879, 2.43046998977661, 0.607173979282379]
  , [0.193601995706558, 1.28612995147705, -1.88138997554779]
  , [0.193601995706558, 1.28612995147705, 1.88138997554779]
  , [0.1942999958992, 2.92396998405457, -0.28719699382782]
  , [0.1942999958992, 2.92396998405457, 0.28719699382782]
  , [0.196875005960464, 2.68358993530273, 0]
  , [0.198676005005836, 0.5225830078125, -1.93069005012512]
  , [0.198676005005836, 0.5225830078125, 1.93069005012512]
  , [0.200000002980232, 2.54999995231628, 0]
  , [0.201561003923416, 0.925194978713989, -1.9587299823761]
  , [0.201561003923416, 0.925194978713989, 1.9587299823761]
  , [0.202656000852585, 0.670825004577637, -1.96937000751495]
  , [0.202656000852585, 0.670825004577637, 1.96937000751495]
  , [0.212515994906425, 2.98681998252869, -0.0911130011081696]
  , [0.212515994906425, 2.98681998252869, 0.0911130011081696]
  , [0.213158994913101, 2.7574200630188, -0.14410300552845]
  , [0.213158994913101, 2.7574200630188, 0.14410300552845]
  , [0.230077996850014, 2.98681998252869, 0]
  , [0.231124997138977, 2.83124995231628, -0.231124997138977]
  , [0.231124997138977, 2.83124995231628, 0.231124997138977]
  , [0.233382001519203, 2.96880006790161, -0.158017992973328]
  , [0.233382001519203, 2.96880006790161, 0.158017992973328]
  , [0.235586002469063, 2.33906006813049, -1.13311994075775]
  , [0.235586002469063, 2.33906006813049, 1.13311994075775]
  , [0.242476999759674, 2.95077991485596, -0.242476999759674]
  , [0.242476999759674, 2.95077991485596, 0.242476999759674]
  , [0.250871986150742, 2.7574200630188, -0.0513469986617565]
  , [0.250871986150742, 2.7574200630188, 0.0513469986617565]
  , [0.251888990402222, 2.46093988418579, -0.372159004211426]
  , [0.251888990402222, 2.46093988418579, 0.372159004211426]
  , [0.256915986537933, 2.29979991912842, -1.235720038414]
  , [0.256915986537933, 2.29979991912842, 1.235720038414]
  , [0.257609993219376, 2.8971700668335, -0.257609993219376]
  , [0.257609993219376, 2.8971700668335, 0.257609993219376]
  , [0.264874994754791, 2.25, -1.27400004863739]
  , [0.264874994754791, 2.25, 1.27400004863739]
  , [0.267832010984421, 2.50547003746033, -0.18087899684906]
  , [0.267832010984421, 2.50547003746033, 0.18087899684906]
  , [0.274421006441116, 2.96880006790161, -0.0563799999654293]
  , [0.274421006441116, 2.96880006790161, 0.0563799999654293]
  , [0.280732005834579, 2.8642098903656, -0.189855992794037]
  , [0.280732005834579, 2.8642098903656, 0.189855992794037]
  , [0.281271010637283, 2.32382988929749, -1.35285997390747]
  , [0.281271010637283, 2.32382988929749, 1.35285997390747]
  , [0.285250008106232, 2.25, -1.37199997901917]
  , [0.285250008106232, 2.25, 1.37199997901917]
  , [0.285887002944946, 2.34843993186951, -1.37505996227264]
  , [0.285887002944946, 2.34843993186951, 1.37505996227264]
  , [0.28719699382782, 2.92396998405457, -0.1942999958992]
  , [0.28719699382782, 2.92396998405457, 0.1942999958992]
  , [0.295278012752533, 2.32382988929749, -1.42023003101349]
  , [0.295278012752533, 2.32382988929749, 1.42023003101349]
  , [0.295329988002777, 2.36952996253967, -0.942332029342651]
  , [0.295329988002777, 2.36952996253967, 0.942332029342651]
  , [0.299953013658524, 2.83124995231628, -0.127984002232552]
  , [0.299953013658524, 2.83124995231628, 0.127984002232552]
  , [0.304711014032364, 2.43046998977661, -0.559973001480103]
  , [0.304711014032364, 2.43046998977661, 0.559973001480103]
  , [0.30562499165535, 0, -1.47000002861023]
  , [0.30562499165535, 0, 1.47000002861023]
  , [0.30562499165535, 2.25, -1.47000002861023]
  , [0.30562499165535, 2.25, 1.47000002861023]
  , [0.314464002847672, 2.95077991485596, -0.134406998753548]
  , [0.314464002847672, 2.95077991485596, 0.134406998753548]
  , [0.315409988164902, 2.50547003746033, -0.0643950030207634]
  , [0.315409988164902, 2.50547003746033, 0.0643950030207634]
  , [0.321543008089066, 0.085547000169754, -1.54656004905701]
  , [0.321543008089066, 0.085547000169754, 1.54656004905701]
  , [0.323812991380692, 2.40000009536743, -0.761062979698181]
  , [0.323812991380692, 2.40000009536743, 0.761062979698181]
  , [0.323938012123108, 2.46093988418579, -0.323938012123108]
  , [0.323938012123108, 2.46093988418579, 0.323938012123108]
  , [0.324999988079071, 2.83124995231628, 0]
  , [0.33032500743866, 2.8642098903656, -0.067671999335289]
  , [0.33032500743866, 2.8642098903656, 0.067671999335289]
  , [0.334237992763519, 2.8971700668335, -0.142704993486404]
  , [0.334237992763519, 2.8971700668335, 0.142704993486404]
  , [0.337859004735947, 2.92396998405457, -0.0692780017852783]
  , [0.337859004735947, 2.92396998405457, 0.0692780017852783]
  , [0.340624988079071, 2.95077991485596, 0]
  , [0.356561988592148, 0.234375, 1.7150000333786]
  , [0.356563001871109, 0.234375, -1.7150000333786]
  , [0.360312014818192, 2.31943011283875, -1.14968001842499]
  , [0.360312014818192, 2.31943011283875, 1.14968001842499]
  , [0.362109005451202, 2.8971700668335, 0]
  , [0.372159004211426, 2.46093988418579, -0.251888990402222]
  , [0.372159004211426, 2.46093988418579, 0.251888990402222]
  , [0.375663995742798, 1.47186994552612, -1.8068699836731]
  , [0.375663995742798, 1.47186994552612, 1.8068699836731]
  , [0.381740003824234, 2.27489995956421, -1.21805000305176]
  , [0.381740003824234, 2.27489995956421, 1.21805000305176]
  , [0.391582012176514, 0.453516006469727, -1.8834400177002]
  , [0.391582012176514, 0.453516006469727, 1.8834400177002]
  , [0.398745000362396, 1.10038995742798, -1.91788995265961]
  , [0.398745000362396, 1.10038995742798, 1.91788995265961]
  , [0.403122991323471, 0.591650009155273, -1.93894994258881]
  , [0.403122991323471, 0.591650009155273, 1.93894994258881]
  , [0.4064100086689, 2.43046998977661, -0.491907000541687]
  , [0.4064100086689, 2.43046998977661, 0.491907000541687]
  , [0.407499998807907, 0.75, -1.96000003814697]
  , [0.407499998807907, 0.75, 1.96000003814697]
  , [0.414463996887207, 2.28691005706787, -1.32246005535126]
  , [0.414463996887207, 2.28691005706787, 1.32246005535126]
  , [0.414929002523422, 2.33612990379333, -1.32395005226135]
  , [0.414929002523422, 2.33612990379333, 1.32395005226135]
  , [0.420890986919403, 2.46093988418579, -0.179077997803688]
  , [0.420890986919403, 2.46093988418579, 0.179077997803688]
  , [0.425177007913589, 2.33612990379333, -1.35664999485016]
  , [0.425177007913589, 2.33612990379333, 1.35664999485016]
  , [0.438241004943848, 2.46093988418579, -0.0912069976329803]
  , [0.438241004943848, 2.46093988418579, 0.0912069976329803]
  , [0.439617991447449, 2.28691005706787, -1.40271997451782]
  , [0.439617991447449, 2.28691005706787, 1.40271997451782]
  , [0.453828006982803, 2.33906006813049, -1.06664001941681]
  , [0.453828006982803, 2.33906006813049, 1.06664001941681]
  , [0.456250011920929, 2.46093988418579, 0]
  , [0.458833009004593, 0.042773000895977, -1.46403002738953]
  , [0.458833009004593, 0.042773000895977, 1.46403002738953]
  , [0.464062988758087, 2.40000009536743, -0.685781002044678]
  , [0.464062988758087, 2.40000009536743, 0.685781002044678]
  , [0.473022997379303, 2.36952996253967, -0.868654012680054]
  , [0.473022997379303, 2.36952996253967, 0.868654012680054]
  , [0.491907000541687, 2.43046998977661, -0.4064100086689]
  , [0.491907000541687, 2.43046998977661, 0.4064100086689]
  , [0.494917988777161, 2.29979991912842, -1.16322004795074]
  , [0.494917988777161, 2.29979991912842, 1.16322004795074]
  , [0.49609899520874, 0.159961000084877, -1.58293998241425]
  , [0.49609899520874, 0.159961000084877, 1.58293998241425]
  , [0.498427987098694, 1.86093997955322, -1.59037005901337]
  , [0.498427987098694, 1.86093997955322, 1.59037005901337]
  , [0.510249972343445, 2.25, -1.19924998283386]
  , [0.510249972343445, 2.25, 1.19924998283386]
  , [0.541833996772766, 2.32382988929749, -1.27348005771637]
  , [0.541833996772766, 2.32382988929749, 1.27348005771637]
  , [0.547339022159576, 0.343944996595383, -1.74644005298615]
  , [0.547339022159576, 0.343944996595383, 1.74644005298615]
  , [0.549499988555908, 2.25, -1.29149997234344]
  , [0.549499988555908, 2.25, 1.29149997234344]
  , [0.550727009773254, 2.34843993186951, -1.2943799495697]
  , [0.550727009773254, 2.34843993186951, 1.2943799495697]
  , [0.559973001480103, 2.43046998977661, -0.304711014032364]
  , [0.559973001480103, 2.43046998977661, 0.304711014032364]
  , [0.566554009914398, 1.28612995147705, -1.80774998664856]
  , [0.566554009914398, 1.28612995147705, 1.80774998664856]
  , [0.568817973136902, 2.32382988929749, -1.33689999580383]
  , [0.568817973136902, 2.32382988929749, 1.33689999580383]
  , [0.577103972434998, 2.31943011283875, -1.05979001522064]
  , [0.577103972434998, 2.31943011283875, 1.05979001522064]
  , [0.581402003765106, 0.5225830078125, -1.85511994361877]
  , [0.581402003765106, 0.5225830078125, 1.85511994361877]
  , [0.585749983787537, 2.40000009536743, -0.585749983787537]
  , [0.585749983787537, 2.40000009536743, 0.585749983787537]
  , [0.588750004768372, 0, -1.38374996185303]
  , [0.588750004768372, 0, 1.38374996185303]
  , [0.588750004768372, 2.25, -1.38374996185303]
  , [0.588750004768372, 2.25, 1.38374996185303]
  , [0.58984500169754, 0.925194978713989, -1.88206005096436]
  , [0.58984500169754, 0.925194978713989, 1.88206005096436]
  , [0.593047022819519, 0.670825004577637, -1.89227998256683]
  , [0.593047022819519, 0.670825004577637, 1.89227998256683]
  , [0.607173979282379, 2.43046998977661, -0.190548002719879]
  , [0.607173979282379, 2.43046998977661, 0.190548002719879]
  , [0.611424028873444, 2.27489995956421, -1.12281000614166]
  , [0.611424028873444, 2.27489995956421, 1.12281000614166]
  , [0.61941397190094, 0.085547000169754, -1.45581996440887]
  , [0.61941397190094, 0.085547000169754, 1.45581996440887]
  , [0.630285024642944, 2.36952996253967, -0.763400018215179]
  , [0.630285024642944, 2.36952996253967, 0.763400018215179]
  , [0.631998002529144, 2.43046998977661, -0.0648249983787537]
  , [0.631998002529144, 2.43046998977661, 0.0648249983787537]
  , [0.650390982627869, 2.33906006813049, -0.961133003234863]
  , [0.650390982627869, 2.33906006813049, 0.961133003234863]
  , [0.663837015628815, 2.28691005706787, -1.21905994415283]
  , [0.663837015628815, 2.28691005706787, 1.21905994415283]
  , [0.664583027362823, 2.33612990379333, -1.22043001651764]
  , [0.664583027362823, 2.33612990379333, 1.22043001651764]
  , [0.680997014045715, 2.33612990379333, -1.25057005882263]
  , [0.680997014045715, 2.33612990379333, 1.25057005882263]
  , [0.685781002044678, 2.40000009536743, -0.464062988758087]
  , [0.685781002044678, 2.40000009536743, 0.464062988758087]
  , [0.686874985694885, 0.234375, -1.61436998844147]
  , [0.686874985694885, 0.234375, 1.61436998844147]
  , [0.704126000404358, 2.28691005706787, -1.29305005073547]
  , [0.704126000404358, 2.28691005706787, 1.29305005073547]
  , [0.709276974201202, 2.29979991912842, -1.04814994335175]
  , [0.709276974201202, 2.29979991912842, 1.04814994335175]
  , [0.723671972751617, 1.47186994552612, -1.70086002349854]
  , [0.723671972751617, 1.47186994552612, 1.70086002349854]
  , [0.731249988079071, 2.25, -1.08062994480133]
  , [0.731249988079071, 2.25, 1.08062994480133]
  , [0.734902024269104, 0.042773000895977, -1.34957003593445]
  , [0.734902024269104, 0.042773000895977, 1.34957003593445]
  , [0.754335999488831, 0.453516006469727, -1.77293002605438]
  , [0.754335999488831, 0.453516006469727, 1.77293002605438]
  , [0.761062979698181, 2.40000009536743, -0.323812991380692]
  , [0.761062979698181, 2.40000009536743, 0.323812991380692]
  , [0.763400018215179, 2.36952996253967, -0.630285024642944]
  , [0.763400018215179, 2.36952996253967, 0.630285024642944]
  , [0.768135011196136, 1.10038995742798, -1.80535995960236]
  , [0.768135011196136, 1.10038995742798, 1.80535995960236]
  , [0.768967986106873, 2.31943011283875, -0.931373000144958]
  , [0.768967986106873, 2.31943011283875, 0.931373000144958]
  , [0.776513993740082, 2.32382988929749, -1.14751994609833]
  , [0.776513993740082, 2.32382988929749, 1.14751994609833]
  , [0.776566982269287, 0.591650009155273, -1.82518005371094]
  , [0.776566982269287, 0.591650009155273, 1.82518005371094]
  , [0.785000026226044, 0.75, -1.84500002861023]
  , [0.785000026226044, 0.75, 1.84500002861023]
  , [0.787500023841858, 2.25, -1.16375005245209]
  , [0.787500023841858, 2.25, 1.16375005245209]
  , [0.789258003234863, 2.34843993186951, -1.16635000705719]
  , [0.789258003234863, 2.34843993186951, 1.16635000705719]
  , [0.794589996337891, 0.159961000084877, -1.45916998386383]
  , [0.794589996337891, 0.159961000084877, 1.45916998386383]
  , [0.79831999540329, 1.86093997955322, -1.46601998806]
  , [0.79831999540329, 1.86093997955322, 1.46601998806]
  , [0.808499991893768, 2.40000009536743, -0.168093994259834]
  , [0.808499991893768, 2.40000009536743, 0.168093994259834]
  , [0.814697980880737, 2.27489995956421, -0.986760973930359]
  , [0.814697980880737, 2.27489995956421, 0.986760973930359]
  , [0.815186023712158, 2.32382988929749, -1.20466005802155]
  , [0.815186023712158, 2.32382988929749, 1.20466005802155]
  , [0.820937991142273, 2.33906006813049, -0.820937991142273]
  , [0.820937991142273, 2.33906006813049, 0.820937991142273]
  , [0.824999988079071, 2.40000009536743, 0]
  , [0.84375, 0, -1.24688005447388]
  , [0.84375, 0, 1.24688005447388]
  , [0.84375, 2.25, -1.24688005447388]
  , [0.84375, 2.25, 1.24688005447388]
  , [0.868654012680054, 2.36952996253967, -0.473022997379303]
  , [0.868654012680054, 2.36952996253967, 0.473022997379303]
  , [0.876659989356995, 0.343944996595383, -1.60988998413086]
  , [0.876659989356995, 0.343944996595383, 1.60988998413086]
  , [0.884536981582642, 2.28691005706787, -1.07134997844696]
  , [0.884536981582642, 2.28691005706787, 1.07134997844696]
  , [0.885531008243561, 2.33612990379333, -1.07255005836487]
  , [0.885531008243561, 2.33612990379333, 1.07255005836487]
  , [0.887695014476776, 0.085547000169754, -1.3118200302124]
  , [0.887695014476776, 0.085547000169754, 1.3118200302124]
  , [0.895265996456146, 2.29979991912842, -0.895265996456146]
  , [0.895265996456146, 2.29979991912842, 0.895265996456146]
  , [0.907401978969574, 2.33612990379333, -1.09904003143311]
  , [0.907401978969574, 2.33612990379333, 1.09904003143311]
  , [0.907437026500702, 1.28612995147705, -1.66639995574951]
  , [0.907437026500702, 1.28612995147705, 1.66639995574951]
  , [0.922999978065491, 2.25, -0.922999978065491]
  , [0.922999978065491, 2.25, 0.922999978065491]
  , [0.931218028068542, 0.5225830078125, -1.71008002758026]
  , [0.931218028068542, 0.5225830078125, 1.71008002758026]
  , [0.931373000144958, 2.31943011283875, -0.768967986106873]
  , [0.931373000144958, 2.31943011283875, 0.768967986106873]
  , [0.938220024108887, 2.28691005706787, -1.13636994361877]
  , [0.938220024108887, 2.28691005706787, 1.13636994361877]
  , [0.942332029342651, 2.36952996253967, -0.295329988002777]
  , [0.942332029342651, 2.36952996253967, 0.295329988002777]
  , [0.944741010665894, 0.925194978713989, -1.7349100112915]
  , [0.944741010665894, 0.925194978713989, 1.7349100112915]
  , [0.949871003627777, 0.670825004577637, -1.7443300485611]
  , [0.949871003627777, 0.670825004577637, 1.7443300485611]
  , [0.961133003234863, 2.33906006813049, -0.650390982627869]
  , [0.961133003234863, 2.33906006813049, 0.650390982627869]
  , [0.979228973388672, 0.042773000895977, -1.18604004383087]
  , [0.979228973388672, 0.042773000895977, 1.18604004383087]
  , [0.98013299703598, 2.32382988929749, -0.98013299703598]
  , [0.98013299703598, 2.32382988929749, 0.98013299703598]
  , [0.980718970298767, 2.36952996253967, -0.100919999182224]
  , [0.980718970298767, 2.36952996253967, 0.100919999182224]
  , [0.984375, 0.234375, -1.45468997955322]
  , [0.984375, 0.234375, 1.45468997955322]
  , [0.986760973930359, 2.27489995956421, -0.814697980880737]
  , [0.986760973930359, 2.27489995956421, 0.814697980880737]
  , [0.994000017642975, 2.25, -0.994000017642975]
  , [0.994000017642975, 2.25, 0.994000017642975]
  , [0.996218979358673, 2.34843993186951, -0.996218979358673]
  , [0.996218979358673, 2.34843993186951, 0.996218979358673]
  , [1.02893996238708, 2.32382988929749, -1.02893996238708]
  , [1.02893996238708, 2.32382988929749, 1.02893996238708]
  , [1.03710997104645, 1.47186994552612, -1.53261995315552]
  , [1.03710997104645, 1.47186994552612, 1.53261995315552]
  , [1.04814994335175, 2.29979991912842, -0.709276974201202]
  , [1.04814994335175, 2.29979991912842, 0.709276974201202]
  , [1.05876004695892, 0.159961000084877, -1.28236997127533]
  , [1.05876004695892, 0.159961000084877, 1.28236997127533]
  , [1.05979001522064, 2.31943011283875, -0.577103972434998]
  , [1.05979001522064, 2.31943011283875, 0.577103972434998]
  , [1.06373000144958, 1.86093997955322, -1.28839004039764]
  , [1.06373000144958, 1.86093997955322, 1.28839004039764]
  , [1.06500005722046, 0, -1.06500005722046]
  , [1.06500005722046, 0, 1.06500005722046]
  , [1.06500005722046, 2.25, -1.06500005722046]
  , [1.06500005722046, 2.25, 1.06500005722046]
  , [1.06664001941681, 2.33906006813049, -0.453828006982803]
  , [1.06664001941681, 2.33906006813049, 0.453828006982803]
  , [1.07134997844696, 2.28691005706787, -0.884536981582642]
  , [1.07134997844696, 2.28691005706787, 0.884536981582642]
  , [1.07255005836487, 2.33612990379333, -0.885531008243561]
  , [1.07255005836487, 2.33612990379333, 0.885531008243561]
  , [1.08062994480133, 2.25, -0.731249988079071]
  , [1.08062994480133, 2.25, 0.731249988079071]
  , [1.08106005191803, 0.453516006469727, -1.59756004810333]
  , [1.08106005191803, 0.453516006469727, 1.59756004810333]
  , [1.09904003143311, 2.33612990379333, -0.907401978969574]
  , [1.09904003143311, 2.33612990379333, 0.907401978969574]
  , [1.10082995891571, 1.10038995742798, -1.62678003311157]
  , [1.10082995891571, 1.10038995742798, 1.62678003311157]
  , [1.11292004585266, 0.591650009155273, -1.64463996887207]
  , [1.11292004585266, 0.591650009155273, 1.64463996887207]
  , [1.12047004699707, 0.085547000169754, -1.12047004699707]
  , [1.12047004699707, 0.085547000169754, 1.12047004699707]
  , [1.12281000614166, 2.27489995956421, -0.611424028873444]
  , [1.12281000614166, 2.27489995956421, 0.611424028873444]
  , [1.125, 0.75, -1.66250002384186]
  , [1.125, 0.75, 1.66250002384186]
  , [1.13311994075775, 2.33906006813049, -0.235586002469063]
  , [1.13311994075775, 2.33906006813049, 0.235586002469063]
  , [1.13636994361877, 2.28691005706787, -0.938220024108887]
  , [1.13636994361877, 2.28691005706787, 0.938220024108887]
  , [1.14751994609833, 2.32382988929749, -0.776513993740082]
  , [1.14751994609833, 2.32382988929749, 0.776513993740082]
  , [1.14968001842499, 2.31943011283875, -0.360312014818192]
  , [1.14968001842499, 2.31943011283875, 0.360312014818192]
  , [1.15625, 2.33906006813049, 0]
  , [1.16322004795074, 2.29979991912842, -0.494917988777161]
  , [1.16322004795074, 2.29979991912842, 0.494917988777161]
  , [1.16375005245209, 2.25, -0.787500023841858]
  , [1.16375005245209, 2.25, 0.787500023841858]
  , [1.16635000705719, 2.34843993186951, -0.789258003234863]
  , [1.16635000705719, 2.34843993186951, 0.789258003234863]
  , [1.16812002658844, 0.343944996595383, -1.41481995582581]
  , [1.16812002658844, 0.343944996595383, 1.41481995582581]
  , [1.18604004383087, 0.042773000895977, -0.979228973388672]
  , [1.18604004383087, 0.042773000895977, 0.979228973388672]
  , [1.19650995731354, 2.31943011283875, -0.123125001788139]
  , [1.19650995731354, 2.31943011283875, 0.123125001788139]
  , [1.19924998283386, 2.25, -0.510249972343445]
  , [1.19924998283386, 2.25, 0.510249972343445]
  , [1.20466005802155, 2.32382988929749, -0.815186023712158]
  , [1.20466005802155, 2.32382988929749, 0.815186023712158]
  , [1.20912003517151, 1.28612995147705, -1.4644900560379]
  , [1.20912003517151, 1.28612995147705, 1.4644900560379]
  , [1.21805000305176, 2.27489995956421, -0.381740003824234]
  , [1.21805000305176, 2.27489995956421, 0.381740003824234]
  , [1.21905994415283, 2.28691005706787, -0.663837015628815]
  , [1.21905994415283, 2.28691005706787, 0.663837015628815]
  , [1.22043001651764, 2.33612990379333, -0.664583027362823]
  , [1.22043001651764, 2.33612990379333, 0.664583027362823]
  , [1.235720038414, 2.29979991912842, -0.256915986537933]
  , [1.235720038414, 2.29979991912842, 0.256915986537933]
  , [1.24081003665924, 0.5225830078125, -1.50286996364594]
  , [1.24081003665924, 0.5225830078125, 1.50286996364594]
  , [1.24249994754791, 0.234375, -1.24249994754791]
  , [1.24249994754791, 0.234375, 1.24249994754791]
  , [1.24688005447388, 0, -0.84375]
  , [1.24688005447388, 0, 0.84375]
  , [1.24688005447388, 2.25, -0.84375]
  , [1.24688005447388, 2.25, 0.84375]
  , [1.25057005882263, 2.33612990379333, -0.680997014045715]
  , [1.25057005882263, 2.33612990379333, 0.680997014045715]
  , [1.25882995128632, 0.925194978713989, -1.52469003200531]
  , [1.25882995128632, 0.925194978713989, 1.52469003200531]
  , [1.26093995571136, 2.29979991912842, 0]
  , [1.26566994190216, 0.670825004577637, -1.53296995162964]
  , [1.26566994190216, 0.670825004577637, 1.53296995162964]
  , [1.26766002178192, 2.27489995956421, -0.130447998642921]
  , [1.26766002178192, 2.27489995956421, 0.130447998642921]
  , [1.27348005771637, 2.32382988929749, -0.541833996772766]
  , [1.27348005771637, 2.32382988929749, 0.541833996772766]
  , [1.27400004863739, 2.25, -0.264874994754791]
  , [1.27400004863739, 2.25, 0.264874994754791]
  , [1.28236997127533, 0.159961000084877, -1.05876004695892]
  , [1.28236997127533, 0.159961000084877, 1.05876004695892]
  , [1.28839004039764, 1.86093997955322, -1.06373000144958]
  , [1.28839004039764, 1.86093997955322, 1.06373000144958]
  , [1.29149997234344, 2.25, -0.549499988555908]
  , [1.29149997234344, 2.25, 0.549499988555908]
  , [1.29305005073547, 2.28691005706787, -0.704126000404358]
  , [1.29305005073547, 2.28691005706787, 0.704126000404358]
  , [1.2943799495697, 2.34843993186951, -0.550727009773254]
  , [1.2943799495697, 2.34843993186951, 0.550727009773254]
  , [1.29999995231628, 2.25, 0]
  , [1.30905997753143, 1.47186994552612, -1.30905997753143]
  , [1.30905997753143, 1.47186994552612, 1.30905997753143]
  , [1.3118200302124, 0.085547000169754, -0.887695014476776]
  , [1.3118200302124, 0.085547000169754, 0.887695014476776]
  , [1.32246005535126, 2.28691005706787, -0.414463996887207]
  , [1.32246005535126, 2.28691005706787, 0.414463996887207]
  , [1.32395005226135, 2.33612990379333, -0.414929002523422]
  , [1.32395005226135, 2.33612990379333, 0.414929002523422]
  , [1.33689999580383, 2.32382988929749, -0.568817973136902]
  , [1.33689999580383, 2.32382988929749, 0.568817973136902]
  , [1.34957003593445, 0.042773000895977, -0.734902024269104]
  , [1.34957003593445, 0.042773000895977, 0.734902024269104]
  , [1.35285997390747, 2.32382988929749, -0.281271010637283]
  , [1.35285997390747, 2.32382988929749, 0.281271010637283]
  , [1.35664999485016, 2.33612990379333, -0.425177007913589]
  , [1.35664999485016, 2.33612990379333, 0.425177007913589]
  , [1.36452996730804, 0.453516006469727, -1.36452996730804]
  , [1.36452996730804, 0.453516006469727, 1.36452996730804]
  , [1.37199997901917, 2.25, -0.285250008106232]
  , [1.37199997901917, 2.25, 0.285250008106232]
  , [1.37505996227264, 2.34843993186951, -0.285887002944946]
  , [1.37505996227264, 2.34843993186951, 0.285887002944946]
  , [1.37633001804352, 2.28691005706787, -0.141629993915558]
  , [1.37633001804352, 2.28691005706787, 0.141629993915558]
  , [1.37787997722626, 2.33612990379333, -0.141789004206657]
  , [1.37787997722626, 2.33612990379333, 0.141789004206657]
  , [1.38047003746033, 2.32382988929749, 0]
  , [1.38374996185303, 0, -0.588750004768372]
  , [1.38374996185303, 0, 0.588750004768372]
  , [1.38374996185303, 2.25, -0.588750004768372]
  , [1.38374996185303, 2.25, 0.588750004768372]
  , [1.38949000835419, 1.10038995742798, -1.38949000835419]
  , [1.38949000835419, 1.10038995742798, 1.38949000835419]
  , [1.39999997615814, 2.25, 0]
  , [1.40271997451782, 2.28691005706787, -0.439617991447449]
  , [1.40271997451782, 2.28691005706787, 0.439617991447449]
  , [1.40313005447388, 2.34843993186951, 0]
  , [1.40474998950958, 0.591650009155273, -1.40474998950958]
  , [1.40474998950958, 0.591650009155273, 1.40474998950958]
  , [1.41191005706787, 2.33612990379333, -0.14529100060463]
  , [1.41191005706787, 2.33612990379333, 0.14529100060463]
  , [1.41481995582581, 0.343944996595383, -1.16812002658844]
  , [1.41481995582581, 0.343944996595383, 1.16812002658844]
  , [1.41999995708466, 0.75, -1.41999995708466]
  , [1.41999995708466, 0.75, 1.41999995708466]
  , [1.42023003101349, 2.32382988929749, -0.295278012752533]
  , [1.42023003101349, 2.32382988929749, 0.295278012752533]
  , [1.4492199420929, 2.32382988929749, 0]
  , [1.45468997955322, 0.234375, -0.984375]
  , [1.45468997955322, 0.234375, 0.984375]
  , [1.45581996440887, 0.085547000169754, -0.61941397190094]
  , [1.45581996440887, 0.085547000169754, 0.61941397190094]
  , [1.45916998386383, 0.159961000084877, -0.794589996337891]
  , [1.45916998386383, 0.159961000084877, 0.794589996337891]
  , [1.45985996723175, 2.28691005706787, -0.15022599697113]
  , [1.45985996723175, 2.28691005706787, 0.15022599697113]
  , [1.46403002738953, 0.042773000895977, -0.458833009004593]
  , [1.46403002738953, 0.042773000895977, 0.458833009004593]
  , [1.4644900560379, 1.28612995147705, -1.20912003517151]
  , [1.4644900560379, 1.28612995147705, 1.20912003517151]
  , [1.46601998806, 1.86093997955322, -0.79831999540329]
  , [1.46601998806, 1.86093997955322, 0.79831999540329]
  , [1.47000002861023, 0, -0.30562499165535]
  , [1.47000002861023, 0, 0.30562499165535]
  , [1.47000002861023, 2.25, -0.30562499165535]
  , [1.47000002861023, 2.25, 0.30562499165535]
  , [1.5, 0, 0]
  , [1.5, 2.25, 0]
  , [1.50286996364594, 0.5225830078125, -1.24081003665924]
  , [1.50286996364594, 0.5225830078125, 1.24081003665924]
  , [1.52366995811462, 0.042773000895977, -0.156791999936104]
  , [1.52366995811462, 0.042773000895977, 0.156791999936104]
  , [1.52469003200531, 0.925194978713989, -1.25882995128632]
  , [1.52469003200531, 0.925194978713989, 1.25882995128632]
  , [1.53261995315552, 1.47186994552612, -1.03710997104645]
  , [1.53261995315552, 1.47186994552612, 1.03710997104645]
  , [1.53296995162964, 0.670825004577637, -1.26566994190216]
  , [1.53296995162964, 0.670825004577637, 1.26566994190216]
  , [1.54656004905701, 0.085547000169754, -0.321543008089066]
  , [1.54656004905701, 0.085547000169754, 0.321543008089066]
  , [1.57813000679016, 0.085547000169754, 0]
  , [1.58293998241425, 0.159961000084877, -0.49609899520874]
  , [1.58293998241425, 0.159961000084877, 0.49609899520874]
  , [1.59037005901337, 1.86093997955322, -0.498427987098694]
  , [1.59037005901337, 1.86093997955322, 0.498427987098694]
  , [1.59756004810333, 0.453516006469727, -1.08106005191803]
  , [1.59756004810333, 0.453516006469727, 1.08106005191803]
  , [1.60988998413086, 0.343944996595383, -0.876659989356995]
  , [1.60988998413086, 0.343944996595383, 0.876659989356995]
  , [1.61436998844147, 0.234375, -0.686874985694885]
  , [1.61436998844147, 0.234375, 0.686874985694885]
  , [1.62678003311157, 1.10038995742798, -1.10082995891571]
  , [1.62678003311157, 1.10038995742798, 1.10082995891571]
  , [1.64463996887207, 0.591650009155273, -1.11292004585266]
  , [1.64463996887207, 0.591650009155273, 1.11292004585266]
  , [1.64742004871368, 0.159961000084877, -0.169525995850563]
  , [1.64742004871368, 0.159961000084877, 0.169525995850563]
  , [1.65515995025635, 1.86093997955322, -0.170322000980377]
  , [1.65515995025635, 1.86093997955322, 0.170322000980377]
  , [1.66250002384186, 0.75, -1.125]
  , [1.66250002384186, 0.75, 1.125]
  , [1.66639995574951, 1.28612995147705, -0.907437026500702]
  , [1.66639995574951, 1.28612995147705, 0.907437026500702]
  , [1.70000004768372, 0.449999988079071, 0]
  , [1.70000004768372, 0.485448986291885, -0.216563001275063]
  , [1.70000004768372, 0.485448986291885, 0.216563001275063]
  , [1.70000004768372, 0.578905999660492, -0.371250003576279]
  , [1.70000004768372, 0.578905999660492, 0.371250003576279]
  , [1.70000004768372, 0.711035013198853, -0.464062988758087]
  , [1.70000004768372, 0.711035013198853, 0.464062988758087]
  , [1.70000004768372, 0.862500011920929, -0.495000004768372]
  , [1.70000004768372, 0.862500011920929, 0.495000004768372]
  , [1.70000004768372, 1.01397001743317, -0.464062988758087]
  , [1.70000004768372, 1.01397001743317, 0.464062988758087]
  , [1.70000004768372, 1.14609003067017, -0.371250003576279]
  , [1.70000004768372, 1.14609003067017, 0.371250003576279]
  , [1.70000004768372, 1.23954999446869, -0.216563001275063]
  , [1.70000004768372, 1.23954999446869, 0.216563001275063]
  , [1.70000004768372, 1.27499997615814, 0]
  , [1.70086002349854, 1.47186994552612, -0.723671972751617]
  , [1.70086002349854, 1.47186994552612, 0.723671972751617]
  , [1.71008002758026, 0.5225830078125, -0.931218028068542]
  , [1.71008002758026, 0.5225830078125, 0.931218028068542]
  , [1.7150000333786, 0.234375, -0.356561988592148]
  , [1.7150000333786, 0.234375, 0.356563001871109]
  , [1.7349100112915, 0.925194978713989, -0.944741010665894]
  , [1.7349100112915, 0.925194978713989, 0.944741010665894]
  , [1.7443300485611, 0.670825004577637, -0.949871003627777]
  , [1.7443300485611, 0.670825004577637, 0.949871003627777]
  , [1.74644005298615, 0.343944996595383, -0.547339022159576]
  , [1.74644005298615, 0.343944996595383, 0.547339022159576]
  , [1.75, 0.234375, 0]
  , [1.77293002605438, 0.453516006469727, -0.754335999488831]
  , [1.77293002605438, 0.453516006469727, 0.754335999488831]
  , [1.80535995960236, 1.10038995742798, -0.768135011196136]
  , [1.80535995960236, 1.10038995742798, 0.768135011196136]
  , [1.8068699836731, 1.47186994552612, -0.375663995742798]
  , [1.8068699836731, 1.47186994552612, 0.375663995742798]
  , [1.80774998664856, 1.28612995147705, -0.566554009914398]
  , [1.80774998664856, 1.28612995147705, 0.566554009914398]
  , [1.80868005752563, 0.669439971446991, -0.41533499956131]
  , [1.80868005752563, 0.669439971446991, 0.41533499956131]
  , [1.81523001194, 0.556497991085052, -0.292881011962891]
  , [1.81523001194, 0.556497991085052, 0.292881011962891]
  , [1.81757998466492, 0.343944996595383, -0.187035992741585]
  , [1.81757998466492, 0.343944996595383, 0.187035992741585]
  , [1.81850004196167, 0.493822991847992, -0.107904002070427]
  , [1.81850004196167, 0.493822991847992, 0.107904002070427]
  , [1.82518005371094, 0.591650009155273, -0.776566982269287]
  , [1.82518005371094, 0.591650009155273, 0.776566982269287]
  , [1.84375, 1.47186994552612, 0]
  , [1.84407997131348, 1.2731100320816, -0.106835998594761]
  , [1.84407997131348, 1.2731100320816, 0.106835998594761]
  , [1.84500002861023, 0.75, -0.785000026226044]
  , [1.84500002861023, 0.75, 0.785000026226044]
  , [1.8498899936676, 1.21245002746582, -0.289983987808228]
  , [1.8498899936676, 1.21245002746582, 0.289983987808228]
  , [1.85511994361877, 0.5225830078125, -0.581402003765106]
  , [1.85511994361877, 0.5225830078125, 0.581402003765106]
  , [1.86006999015808, 1.10627996921539, -0.412081986665726]
  , [1.86006999015808, 1.10627996921539, 0.412081986665726]
  , [1.87285995483398, 0.972819983959198, -0.473131000995636]
  , [1.87285995483398, 0.972819983959198, 0.473131000995636]
  , [1.88138997554779, 1.28612995147705, -0.193601995706558]
  , [1.88138997554779, 1.28612995147705, 0.193601995706558]
  , [1.88206005096436, 0.925194978713989, -0.58984500169754]
  , [1.88206005096436, 0.925194978713989, 0.58984500169754]
  , [1.8834400177002, 0.453516006469727, -0.391582012176514]
  , [1.8834400177002, 0.453516006469727, 0.391582012176514]
  , [1.88652002811432, 0.830256998538971, -0.473131000995636]
  , [1.88652002811432, 0.830256998538971, 0.473131000995636]
  , [1.89227998256683, 0.670825004577637, -0.593047022819519]
  , [1.89227998256683, 0.670825004577637, 0.593047022819519]
  , [1.90898001194, 0.762850999832153, -0.457367986440659]
  , [1.90898001194, 0.762850999832153, 0.457367986440659]
  , [1.91788995265961, 1.10038995742798, -0.398745000362396]
  , [1.91788995265961, 1.10038995742798, 0.398745000362396]
  , [1.92188000679016, 0.453516006469727, 0]
  , [1.92571997642517, 0.624967992305756, -0.368660002946854]
  , [1.92571997642517, 0.624967992305756, 0.368660002946854]
  , [1.93069005012512, 0.5225830078125, -0.198676005005836]
  , [1.93069005012512, 0.5225830078125, 0.198676005005836]
  , [1.93519997596741, 0.536666989326477, -0.215051993727684]
  , [1.93519997596741, 0.536666989326477, 0.215051993727684]
  , [1.93878996372223, 0.503174006938934, 0]
  , [1.93894994258881, 0.591650009155273, -0.403122991323471]
  , [1.93894994258881, 0.591650009155273, 0.403122991323471]
  , [1.9570300579071, 1.10038995742798, 0]
  , [1.9587299823761, 0.925194978713989, -0.201561003923416]
  , [1.9587299823761, 0.925194978713989, 0.201561003923416]
  , [1.96000003814697, 0.75, -0.407499998807907]
  , [1.96000003814697, 0.75, 0.407499998807907]
  , [1.96937000751495, 0.670825004577637, -0.202656000852585]
  , [1.96937000751495, 0.670825004577637, 0.202656000852585]
  , [1.97852003574371, 0.591650009155273, 0]
  , [1.98495995998383, 1.30458998680115, 0]
  , [1.99135994911194, 1.27330994606018, -0.210782006382942]
  , [1.99135994911194, 1.27330994606018, 0.210782006382942]
  , [2, 0.75, 0]
  , [2.00798988342285, 0.721262991428375, -0.409761011600494]
  , [2.00798988342285, 0.721262991428375, 0.409761011600494]
  , [2.00820994377136, 1.19084000587463, -0.36133998632431]
  , [2.00820994377136, 1.19084000587463, 0.36133998632431]
  , [2.02470993995667, 0.614948987960815, -0.288958013057709]
  , [2.02470993995667, 0.614948987960815, 0.288958013057709]
  , [2.03204989433289, 1.07423996925354, -0.451674997806549]
  , [2.03204989433289, 1.07423996925354, 0.451674997806549]
  , [2.03379011154175, 0.556061983108521, -0.106458000838757]
  , [2.03379011154175, 0.556061983108521, 0.106458000838757]
  , [2.05938005447388, 0.940576016902924, -0.481786996126175]
  , [2.05938005447388, 0.940576016902924, 0.481786996126175]
  , [2.08644008636475, 1.33047997951508, -0.101580999791622]
  , [2.08644008636475, 1.33047997951508, 0.101580999791622]
  , [2.08669996261597, 0.806914985179901, -0.451674997806549]
  , [2.08669996261597, 0.806914985179901, 0.451674997806549]
  , [2.10140991210938, 1.27814996242523, -0.275720000267029]
  , [2.10140991210938, 1.27814996242523, 0.275720000267029]
  , [2.11052989959717, 0.69031697511673, -0.36133998632431]
  , [2.11052989959717, 0.69031697511673, 0.36133998632431]
  , [2.12738990783691, 0.60784500837326, -0.210782006382942]
  , [2.12738990783691, 0.60784500837326, 0.210782006382942]
  , [2.1275999546051, 1.18656003475189, -0.39181199669838]
  , [2.1275999546051, 1.18656003475189, 0.39181199669838]
  , [2.13379001617432, 0.576563000679016, 0]
  , [2.16054010391235, 1.07142996788025, -0.449858993291855]
  , [2.16054010391235, 1.07142996788025, 0.449858993291855]
  , [2.16921997070312, 0.790259003639221, -0.399360001087189]
  , [2.16921997070312, 0.790259003639221, 0.399360001087189]
  , [2.17968988418579, 1.38515996932983, 0]
  , [2.1897599697113, 1.35887002944946, -0.195541992783546]
  , [2.1897599697113, 1.35887002944946, 0.195541992783546]
  , [2.19480991363525, 0.691761016845703, -0.281558990478516]
  , [2.19480991363525, 0.691761016845703, 0.281558990478516]
  , [2.19570994377136, 0.948444008827209, -0.449858993291855]
  , [2.19570994377136, 0.948444008827209, 0.449858993291855]
  , [2.20836997032166, 0.637081980705261, -0.103731997311115]
  , [2.20836997032166, 0.637081980705261, 0.103731997311115]
  , [2.21631002426147, 1.28956997394562, -0.335215002298355]
  , [2.21631002426147, 1.28956997394562, 0.335215002298355]
  , [2.2202000617981, 0.891314029693604, -0.434457004070282]
  , [2.2202000617981, 0.891314029693604, 0.434457004070282]
  , [2.24856996536255, 1.43299996852875, -0.0923840031027794]
  , [2.24856996536255, 1.43299996852875, 0.0923840031027794]
  , [2.25383996963501, 1.19159996509552, -0.419019013643265]
  , [2.25383996963501, 1.19159996509552, 0.419019013643265]
  , [2.25943994522095, 0.772489011287689, -0.349967002868652]
  , [2.25943994522095, 0.772489011287689, 0.349967002868652]
  , [2.26856994628906, 1.39015996456146, -0.250757992267609]
  , [2.26856994628906, 1.39015996456146, 0.250757992267609]
  , [2.28188991546631, 0.696393013000488, -0.20414699614048]
  , [2.28188991546631, 0.696393013000488, 0.20414699614048]
  , [2.29041004180908, 0.667528986930847, 0]
  , [2.29688000679016, 1.0793000459671, -0.446952998638153]
  , [2.29688000679016, 1.0793000459671, 0.446952998638153]
  , [2.29924988746643, 0.874952971935272, -0.384663999080658]
  , [2.29924988746643, 0.874952971935272, 0.384663999080658]
  , [2.30358004570007, 1.31519997119904, -0.356339991092682]
  , [2.30358004570007, 1.31519997119904, 0.356339991092682]
  , [2.30644011497498, 1.50440001487732, 0]
  , [2.31838011741638, 1.48355996608734, -0.17399600148201]
  , [2.31838011741638, 1.48355996608734, 0.17399600148201]
  , [2.33068990707397, 0.784406006336212, -0.271218001842499]
  , [2.33068990707397, 0.784406006336212, 0.271218001842499]
  , [2.33991003036499, 0.966988980770111, -0.419019013643265]
  , [2.33991003036499, 0.966988980770111, 0.419019013643265]
  , [2.34758996963501, 0.734270989894867, -0.0999220013618469]
  , [2.34758996963501, 0.734270989894867, 0.0999220013618469]
  , [2.34758996963501, 1.22096002101898, -0.409130990505219]
  , [2.34758996963501, 1.22096002101898, 0.409130990505219]
  , [2.34983992576599, 1.42864000797272, -0.298278987407684]
  , [2.34983992576599, 1.42864000797272, 0.298278987407684]
  , [2.35317993164062, 1.56816005706787, -0.0808229967951775]
  , [2.35317993164062, 1.56816005706787, 0.0808229967951775]
  , [2.37575006484985, 1.53531002998352, -0.219376996159554]
  , [2.37575006484985, 1.53531002998352, 0.219376996159554]
  , [2.37743997573853, 0.869018971920013, -0.335215002298355]
  , [2.37743997573853, 0.869018971920013, 0.335215002298355]
  , [2.38750004768372, 1.64999997615814, 0]
  , [2.39432001113892, 1.35098004341125, -0.372848987579346]
  , [2.39432001113892, 1.35098004341125, 0.372848987579346]
  , [2.39459991455078, 1.12030005455017, -0.409130990505219]
  , [2.39459991455078, 1.12030005455017, 0.409130990505219]
  , [2.40038990974426, 1.63469004631042, -0.149296998977661]
  , [2.40038990974426, 1.63469004631042, 0.149296998977661]
  , [2.4039900302887, 0.799722015857697, -0.195541992783546]
  , [2.4039900302887, 0.799722015857697, 0.195541992783546]
  , [2.41406011581421, 0.773437976837158, 0]
  , [2.41524004936218, 1.47781002521515, -0.311747014522552]
  , [2.41524004936218, 1.47781002521515, 0.311747014522552]
  , [2.43438005447388, 1.59433996677399, -0.255937993526459]
  , [2.43438005447388, 1.59433996677399, 0.255937993526459]
  , [2.4386100769043, 1.02605998516083, -0.356339991092682]
  , [2.4386100769043, 1.02605998516083, 0.356339991092682]
  , [2.44531011581421, 1.26196002960205, -0.397704988718033]
  , [2.44531011581421, 1.26196002960205, 0.397704988718033]
  , [2.45167994499207, 1.805340051651, -0.0630870014429092]
  , [2.45167994499207, 1.805340051651, 0.0630870014429092]
  , [2.46489000320435, 1.40551996231079, -0.357930988073349]
  , [2.46489000320435, 1.40551996231079, 0.357930988073349]
  , [2.47361993789673, 0.95109897851944, -0.250757992267609]
  , [2.47361993789673, 0.95109897851944, 0.250757992267609]
  , [2.47767996788025, 1.78638005256653, -0.171237006783485]
  , [2.47767996788025, 1.78638005256653, 0.171237006783485]
  , [2.48241996765137, 1.53727996349335, -0.319922000169754]
  , [2.48241996765137, 1.53727996349335, 0.319922000169754]
  , [2.49361991882324, 0.908263981342316, -0.0923840031027794]
  , [2.49361991882324, 0.908263981342316, 0.0923840031027794]
  , [2.49629998207092, 1.17295002937317, -0.372848987579346]
  , [2.49629998207092, 1.17295002937317, 0.372848987579346]
  , [2.50155997276306, 1.97108995914459, 0]
  , [2.5172700881958, 1.9655499458313, -0.103051997721195]
  , [2.5172700881958, 1.9655499458313, 0.103051997721195]
  , [2.51792001724243, 1.32831001281738, -0.357930988073349]
  , [2.51792001724243, 1.32831001281738, 0.357930988073349]
  , [2.52318000793457, 1.75321996212006, -0.243336006999016]
  , [2.52318000793457, 1.75321996212006, 0.243336006999016]
  , [2.53749990463257, 1.47186994552612, -0.341250002384186]
  , [2.53749990463257, 1.47186994552612, 0.341250002384186]
  , [2.54078006744385, 1.09528994560242, -0.298278987407684]
  , [2.54078006744385, 1.09528994560242, 0.298278987407684]
  , [2.5491099357605, 2.0446400642395, -0.047715999186039]
  , [2.5491099357605, 2.0446400642395, 0.047715999186039]
  , [2.55869007110596, 1.95095002651215, -0.176660001277924]
  , [2.55869007110596, 1.95095002651215, 0.176660001277924]
  , [2.56756997108459, 1.25602996349335, -0.311747014522552]
  , [2.56756997108459, 1.25602996349335, 0.311747014522552]
  , [2.57224988937378, 1.04035997390747, -0.17399600148201]
  , [2.57224988937378, 1.04035997390747, 0.17399600148201]
  , [2.57909989356995, 2.1219699382782, 0]
  , [2.58038997650146, 1.71152997016907, -0.279386013746262]
  , [2.58038997650146, 1.71152997016907, 0.279386013746262]
  , [2.58101010322571, 2.0377299785614, -0.129515007138252]
  , [2.58101010322571, 2.0377299785614, 0.129515007138252]
  , [2.58418011665344, 1.0195300579071, 0]
  , [2.59258008003235, 1.40646994113922, -0.319922000169754]
  , [2.59258008003235, 1.40646994113922, 0.319922000169754]
  , [2.59848999977112, 2.11992001533508, -0.0878119990229607]
  , [2.59848999977112, 2.11992001533508, 0.0878119990229607]
  , [2.60177993774414, 1.55472004413605, -0.304019004106522]
  , [2.60177993774414, 1.55472004413605, 0.304019004106522]
  , [2.60706996917725, 1.19852995872498, -0.219376996159554]
  , [2.60706996917725, 1.19852995872498, 0.219376996159554]
  , [2.61161994934082, 1.69128000736237, -0.287907987833023]
  , [2.61161994934082, 1.69128000736237, 0.287907987833023]
  , [2.61724996566772, 1.93031001091003, -0.220825001597404]
  , [2.61724996566772, 1.93031001091003, 0.220825001597404]
  , [2.62963008880615, 1.16568005084991, -0.0808229967951775]
  , [2.62963008880615, 1.16568005084991, 0.0808229967951775]
  , [2.6378800868988, 2.02554988861084, -0.180818006396294]
  , [2.6378800868988, 2.02554988861084, 0.180818006396294]
  , [2.64063000679016, 1.34941005706787, -0.255937993526459]
  , [2.64063000679016, 1.34941005706787, 0.255937993526459]
  , [2.6496000289917, 2.11451005935669, -0.150535002350807]
  , [2.6496000289917, 2.11451005935669, 0.150535002350807]
  , [2.65084004402161, 2.18547010421753, -0.0424610003829002]
  , [2.65084004402161, 2.18547010421753, 0.0424610003829002]
  , [2.65390992164612, 1.50419998168945, -0.264113008975983]
  , [2.65390992164612, 1.50419998168945, 0.264113008975983]
  , [2.6654200553894, 1.64925003051758, -0.266995012760162]
  , [2.6654200553894, 1.64925003051758, 0.266995012760162]
  , [2.67460989952087, 1.30905997753143, -0.149296998977661]
  , [2.67460989952087, 1.30905997753143, 0.149296998977661]
  , [2.67823004722595, 1.78253996372223, -0.252819001674652]
  , [2.67823004722595, 1.78253996372223, 0.252819001674652]
  , [2.68438005447388, 1.90664005279541, -0.235547006130219]
  , [2.68438005447388, 1.90664005279541, 0.235547006130219]
  , [2.6875, 1.29375004768372, 0]
  , [2.69190001487732, 2.18360996246338, -0.115250997245312]
  , [2.69190001487732, 2.18360996246338, 0.115250997245312]
  , [2.69644999504089, 1.46379995346069, -0.185856997966766]
  , [2.69644999504089, 1.46379995346069, 0.185856997966766]
  , [2.70000004768372, 2.25, 0]
  , [2.70808005332947, 2.01037001609802, -0.208084002137184]
  , [2.70808005332947, 2.01037001609802, 0.208084002137184]
  , [2.71703004837036, 1.61167001724243, -0.213596001267433]
  , [2.71703004837036, 1.61167001724243, 0.213596001267433]
  , [2.72076010704041, 1.44071996212006, -0.0684740021824837]
  , [2.72076010704041, 1.44071996212006, 0.0684740021824837]
  , [2.72578001022339, 2.25, -0.0820309966802597]
  , [2.72578001022339, 2.25, 0.0820309966802597]
  , [2.72599005699158, 2.10643005371094, -0.175249993801117]
  , [2.72599005699158, 2.10643005371094, 0.175249993801117]
  , [2.73600006103516, 1.75154995918274, -0.219519004225731]
  , [2.73600006103516, 1.75154995918274, 0.219519004225731]
  , [2.75021004676819, 2.26919007301331, -0.039733998477459]
  , [2.75021004676819, 2.26919007301331, 0.039733998477459]
  , [2.75149989128113, 1.8829699754715, -0.220825001597404]
  , [2.75149989128113, 1.8829699754715, 0.220825001597404]
  , [2.7535400390625, 1.58508002758026, -0.124597996473312]
  , [2.7535400390625, 1.58508002758026, 0.124597996473312]
  , [2.76737999916077, 1.57500004768372, 0]
  , [2.77555990219116, 2.28399991989136, 0]
  , [2.7809898853302, 1.9943699836731, -0.208084002137184]
  , [2.7809898853302, 1.9943699836731, 0.208084002137184]
  , [2.78303003311157, 1.72669994831085, -0.154476001858711]
  , [2.78303003311157, 1.72669994831085, 0.154476001858711]
  , [2.79375004768372, 2.25, -0.140625]
  , [2.79375004768372, 2.25, 0.140625]
  , [2.79782009124756, 2.27174997329712, -0.10784900188446]
  , [2.79782009124756, 2.27174997329712, 0.10784900188446]
  , [2.79948997497559, 2.29274988174438, -0.0769039988517761]
  , [2.79948997497559, 2.29274988174438, 0.0769039988517761]
  , [2.79999995231628, 2.25, 0]
  , [2.80468988418579, 2.09809994697571, -0.200712993741035]
  , [2.80468988418579, 2.09809994697571, 0.200712993741035]
  , [2.8099000453949, 1.71249997615814, -0.0569120012223721]
  , [2.8099000453949, 1.71249997615814, 0.0569120012223721]
  , [2.81006002426147, 1.86232995986938, -0.176660001277924]
  , [2.81006002426147, 1.86232995986938, 0.176660001277924]
  , [2.81201004981995, 2.17814993858337, -0.169843003153801]
  , [2.81201004981995, 2.17814993858337, 0.169843003153801]
  , [2.81274008750916, 2.29753994941711, -0.035631999373436]
  , [2.81274008750916, 2.29753994941711, 0.035631999373436]
  , [2.81718993186951, 2.25, -0.0492190010845661]
  , [2.81718993186951, 2.25, 0.0492190010845661]
  , [2.82500004768372, 2.30625009536743, 0]
  , [2.8301100730896, 2.27129006385803, -0.025891000404954]
  , [2.8301100730896, 2.27129006385803, 0.025891000404954]
  , [2.84063005447388, 2.29219007492065, 0]
  , [2.84478998184204, 2.29963994026184, -0.0299929995089769]
  , [2.84478998184204, 2.29963994026184, 0.0299929995089769]
  , [2.85091996192932, 2.30715990066528, -0.0656249970197678]
  , [2.85091996192932, 2.30715990066528, 0.0656249970197678]
  , [2.85118007659912, 1.97918999195099, -0.180818006396294]
  , [2.85118007659912, 1.97918999195099, 0.180818006396294]
  , [2.85148000717163, 1.84773004055023, -0.103051997721195]
  , [2.85148000717163, 1.84773004055023, 0.103051997721195]
  , [2.86048007011414, 2.30093002319336, -0.0967160016298294]
  , [2.86048007011414, 2.30093002319336, 0.0967160016298294]
  , [2.86249995231628, 2.25, -0.0843750014901161]
  , [2.86249995231628, 2.25, 0.0843750014901161]
  , [2.86262989044189, 2.29297995567322, -0.0543459989130497]
  , [2.86262989044189, 2.29297995567322, 0.0543459989130497]
  , [2.86574006080627, 2.27201008796692, -0.0702759996056557]
  , [2.86574006080627, 2.27201008796692, 0.0702759996056557]
  , [2.86718988418579, 1.84219002723694, 0]
  , [2.87227988243103, 2.29425001144409, -0.131835997104645]
  , [2.87227988243103, 2.29425001144409, 0.131835997104645]
  , [2.88338994979858, 2.08977007865906, -0.175249993801117]
  , [2.88338994979858, 2.08977007865906, 0.175249993801117]
  , [2.88836002349854, 2.30118989944458, -0.0814089998602867]
  , [2.88836002349854, 2.30118989944458, 0.0814089998602867]
  , [2.89826989173889, 2.17088007926941, -0.19438199698925]
  , [2.89826989173889, 2.17088007926941, 0.19438199698925]
  , [2.90805006027222, 1.96700000762939, -0.129515007138252]
  , [2.90805006027222, 1.96700000762939, 0.129515007138252]
  , [2.91923999786377, 2.30955004692078, -0.112499997019768]
  , [2.91923999786377, 2.30955004692078, 0.112499997019768]
  , [2.92063999176025, 2.29506993293762, -0.0931639969348907]
  , [2.92063999176025, 2.29506993293762, 0.0931639969348907]
  , [2.93279004096985, 2.13103008270264, -0.17221100628376]
  , [2.93279004096985, 2.13103008270264, 0.17221100628376]
  , [2.93980002403259, 2.27326011657715, -0.158935993909836]
  , [2.93980002403259, 2.27326011657715, 0.158935993909836]
  , [2.93996000289917, 1.96010005474091, -0.047715999186039]
  , [2.93996000289917, 1.96010005474091, 0.047715999186039]
  , [2.95977997779846, 2.08168005943298, -0.150535002350807]
  , [2.95977997779846, 2.08168005943298, 0.150535002350807]
  , [2.96994996070862, 2.27412009239197, -0.103564001619816]
  , [2.96994996070862, 2.27412009239197, 0.103564001619816]
  , [3, 2.25, -0.1875]
  , [3, 2.25, -0.112499997019768]
  , [3, 2.25, 0.112499997019768]
  , [3, 2.25, 0.1875]
  , [3.00281000137329, 2.30484008789062, -0.142528995871544]
  , [3.00281000137329, 2.30484008789062, 0.142528995871544]
  , [3.01089000701904, 2.07627010345459, -0.0878119990229607]
  , [3.01089000701904, 2.07627010345459, 0.0878119990229607]
  , [3.01577997207642, 2.30571007728577, -0.119970999658108]
  , [3.01577997207642, 2.30571007728577, 0.119970999658108]
  , [3.03027009963989, 2.0742199420929, 0]
  , [3.04150009155273, 2.12566995620728, -0.116276003420353]
  , [3.04150009155273, 2.12566995620728, 0.116276003420353]
  , [3.0432300567627, 2.2110800743103, -0.166430994868279]
  , [3.0432300567627, 2.2110800743103, 0.166430994868279]
  , [3.06841993331909, 2.17344999313354, -0.143215000629425]
  , [3.06841993331909, 2.17344999313354, 0.143215000629425]
  , [3.07928991317749, 2.12305998802185, -0.0428379997611046]
  , [3.07928991317749, 2.12305998802185, 0.0428379997611046]
  , [3.09315991401672, 2.29877996444702, -0.17578099668026]
  , [3.09315991401672, 2.29877996444702, 0.17578099668026]
  , [3.09667992591858, 2.30141997337341, -0.124219000339508]
  , [3.09667992591858, 2.30141997337341, 0.124219000339508]
  , [3.12655997276306, 2.31680011749268, -0.150000005960464]
  , [3.12655997276306, 2.31680011749268, 0.150000005960464]
  , [3.12671995162964, 2.2772901058197, -0.103564001619816]
  , [3.12671995162964, 2.2772901058197, 0.103564001619816]
  , [3.12690997123718, 2.17127990722656, -0.0835419967770576]
  , [3.12690997123718, 2.17127990722656, 0.0835419967770576]
  , [3.13750004768372, 2.25, -0.0843750014901161]
  , [3.13750004768372, 2.25, 0.0843750014901161]
  , [3.14910006523132, 2.17045998573303, 0]
  , [3.15336990356445, 2.27552008628845, -0.158935993909836]
  , [3.15336990356445, 2.27552008628845, 0.158935993909836]
  , [3.16895008087158, 2.21117997169495, -0.11235299706459]
  , [3.16895008087158, 2.21117997169495, 0.11235299706459]
  , [3.18281006813049, 2.25, -0.0492190010845661]
  , [3.18281006813049, 2.25, 0.0492190010845661]
  , [3.20000004768372, 2.25, 0]
  , [3.20624995231628, 2.25, -0.140625]
  , [3.20624995231628, 2.25, 0.140625]
  , [3.20745992660522, 2.31251001358032, -0.119970999658108]
  , [3.20745992660522, 2.31251001358032, 0.119970999658108]
  , [3.21255993843079, 2.21042990684509, -0.0413930006325245]
  , [3.21255993843079, 2.21042990684509, 0.0413930006325245]
  , [3.21691989898682, 2.31072998046875, -0.142528995871544]
  , [3.21691989898682, 2.31072998046875, 0.142528995871544]
  , [3.23094010353088, 2.27940011024475, -0.0702759996056557]
  , [3.23094010353088, 2.27940011024475, 0.0702759996056557]
  , [3.26724004745483, 2.2781400680542, -0.025891000404954]
  , [3.26724004745483, 2.2781400680542, 0.025891000404954]
  , [3.27272009849548, 2.30776000022888, -0.0931639969348907]
  , [3.27272009849548, 2.30776000022888, 0.0931639969348907]
  , [3.27421998977661, 2.25, -0.0820309966802597]
  , [3.27421998977661, 2.25, 0.0820309966802597]
  , [3.29534006118774, 2.2770299911499, -0.10784900188446]
  , [3.29534006118774, 2.2770299911499, 0.10784900188446]
  , [3.29999995231628, 2.25, 0]
  , [3.31404995918274, 2.30330991744995, -0.131835997104645]
  , [3.31404995918274, 2.30330991744995, 0.131835997104645]
  , [3.33072996139526, 2.30984997749329, -0.0543459989130497]
  , [3.33072996139526, 2.30984997749329, 0.0543459989130497]
  , [3.33388996124268, 2.324049949646, -0.112499997019768]
  , [3.33388996124268, 2.324049949646, 0.112499997019768]
  , [3.33488988876343, 2.31701993942261, -0.0814089998602867]
  , [3.33488988876343, 2.31701993942261, 0.0814089998602867]
  , [3.34236001968384, 2.2800600528717, -0.039733998477459]
  , [3.34236001968384, 2.2800600528717, 0.039733998477459]
  , [3.35542988777161, 2.30270004272461, 0]
  , [3.35925006866455, 2.31465005874634, -0.0967160016298294]
  , [3.35925006866455, 2.31465005874634, 0.0967160016298294]
  , [3.37912011146545, 2.31658005714417, -0.0299929995089769]
  , [3.37912011146545, 2.31658005714417, 0.0299929995089769]
  , [3.38684010505676, 2.30481004714966, -0.0769039988517761]
  , [3.38684010505676, 2.30481004714966, 0.0769039988517761]
  , [3.40220999717712, 2.32644009590149, -0.0656249970197678]
  , [3.40220999717712, 2.32644009590149, 0.0656249970197678]
  , [3.40638995170593, 2.31850004196167, -0.035631999373436]
  , [3.40638995170593, 2.31850004196167, 0.035631999373436]
  , [3.40838003158569, 2.31542992591858, 0]
  , [3.42811989784241, 2.32733988761902, 0] ]

voronoiCellTeapot :: [[Double]]
voronoiCellTeapot =
  [0, 1.5, 0] :
  [
    [ -0.18667466541973257
    , 0.5968790484510281
    , 0.5717139272637414
    ]
  , [ -0.15441948465435382
    , 0.7326922941987842
    , 0.7170049842821647
    ]
  , [ -1.7990621802658653e-2
    , 1.9860825589150588e-2
    , 1.7990621802654917e-2
    ]
  , [ -0.29182490099196906
    , 0.7297971305995979
    , 0.6710598534195158
    ]
  , [ -6.448622334243023e-2
    , 0.5851752797903265
    , 0.5851752797903262
    ]
  , [ -0.1624173204710072 , 0.7759039565328439 , 0.7575092969306046 ]
  , [ -2.3194160942496607e-2
    , 2.6785629054845606e-2
    , 2.3194160942459883e-2
    ]
  , [ -0.39216297275000733
    , 0.6074273900130303
    , 0.4720306023200158
    ]
  , [ -2.215103618775975e-2
    , 2.5331770412981847e-2
    , 2.215103618775975e-2
    ]
  , [ -0.4720306023200158
    , 0.6074273900130303
    , 0.39216297275000733
    ]
  , [ -0.1228050514414682 , 0.8753822495491728 , 0.8567258893994795 ]
  , [ -0.23969991808709246
    , 0.8768406254951662
    , 0.8337562524679074
    ]
  , [ 0.7805147235214109
    , 0.8624999821186053
    , 4.365935118304741e-2
    ]
  , [ 0.863072608376728 , 1.4862706190912907 , 0.14576662495052128 ]
  , [ 0.7805147235214106
    , 0.8624999821186055
    , -4.36593511830482e-2
    ]
  , [ 0.7805144198006899
    , 0.8624994903802878
    , 4.366114397309236e-2
    ]
  , [ 0.7740595934432167 , 0.862499490380288 , 9.433094929942933e-2 ]
  , [ 0.8122497597351315 , 1.3634301491185252 , 0.39698702598827496 ]
  , [ 0.7740579104368239 , 0.8624980151653314 , 9.43372496836588e-2 ]
  , [ 2.3194160942461885e-2
    , 2.6785629054845606e-2
    , 2.319416094249719e-2
    ]
  , [ 0.4720306023200158 , 0.6074273900130303 , 0.39216297275000733 ]
  , [ 2.215103618775549e-2
    , 2.5331770412978624e-2
    , 2.2151036187755824e-2
    ]
  , [ 0.39216297275000733 , 0.6074273900130303 , 0.4720306023200158 ]
  , [ -0.18893822974950683
    , 0.9120771417338428
    , 0.8765366554024406
    ]
  , [ -0.15370753430700942
    , 0.9074581172580759
    , 0.8796369493525763
    ]
  , [ -0.19282328992414305
    , 1.0886815800201057
    , 0.8962126634940768
    ]
  , [ -0.21959663365004273 , 0.90801535760842 , 0.8664476852626979 ]
  , [ 0.6553927028305585 , 0.6599214690464071 , 7.68209537002328e-2 ]
  , [ 0.6480770038782534
    , 0.6480770038782535
    , 3.783067537459344e-2
    ]
  , [ 0.6480770038782537
    , 0.6480770038782535
    , -3.7830675374592546e-2
    ]
  , [ 0.5851752797903262
    , 0.5851752797903265
    , 6.448622334243026e-2
    ]
  , [ 0.6500310589666277 , 0.6577486876432921 , 0.10873036208745468 ]
  , [ 0.7710778571614446 , 0.862498015165331 , 0.10798328513535715 ]
  , [ 0.6540866269118542 , 0.6844355992166629 , 0.20191695153447142 ]
  , [ -0.3246126189761304 , 0.9080154562527455 , 0.8335353871385288 ]
  , [ -0.289831893259673 , 0.8768408129798988 , 0.818044872661731 ]
  , [ -9.869506660256815e-2
    , 1.1894267902510014
    , 0.9173511809757207
    ]
  , [ -0.283130810168734 , 1.1814109441614336 , 0.8788805908385666 ]
  , [ -0.28347840420931564
    , 1.2593473684859822
    , 0.8800216675702618
    ]
  , [ -0.36590860085651855
    , 1.0902530151825576
    , 0.8421531448806922
    ]
  , [ 9.87971813131522e-2 , 1.257602275825742 , 0.9183913329068394 ]
  , [ 0.1916029836825906 , 1.29931244670657 , 0.8997455590151622 ]
  , [ 9.86950666025681e-2 , 1.1894267902510014 , 0.9173511809757205 ]
  , [ 0.0 , 1.3011746498142653 , 0.9190561156885906 ]
  , [ -9.879718131315218e-2
    , 1.2576022758257417
    , 0.9183913329068394
    ]
  , [ -0.1916029836825906 , 1.29931244670657 , 0.8997455590151622 ]
  , [ 0.0 , 1.0862351104914478 , 0.9157767877158491 ]
  , [ 0.0 , 1.4739386059327377 , 0.8813815581455101 ]
  , [ -3.497482895265056e-2
    , 0.9074580789365942
    , 0.8918549800719824
    ]
  , [ -6.080525584912345e-2 , 0.8753821429287596 , 0.86310582524099 ]
  , [ 7.812642760551707e-3
    , 7.812642760551707e-3
    , 7.812642760550714e-3
    ]
  , [ 0.6491833829319444 , 0.6692949179238175 , 0.1694761444119623 ]
  , [ -2.7755575615628914e-17
    , 0.7373497776866849
    , 0.7373497776866843
    ]
  , [ -7.812642760551686e-3
    , 7.812642760551715e-3
    , 7.812642760551602e-3
    ]
  , [ -0.3581881925466474 , 0.9124253481133149 , 0.8235344289369642 ]
  , [ -0.4468960748716194 , 1.1765076558719516 , 0.8091216099094026 ]
  , [ -0.4474984305598345 , 1.2598574705147236 , 0.8102438227139427 ]
  , [ -0.36541277208493106
    , 1.2970587028449376
    , 0.8457844093935878
    ]
  , [ -0.5194379814088046 , 1.2957898827631247 , 0.7622224951613341 ]
  , [ -0.518569427022487 , 1.0903982826657406 , 0.7590404339629727 ]
  , [ -0.5851752797903265
    , 0.5851752797903264
    , 6.448622334243026e-2
    ]
  , [ -7.812642760551698e-3
    , 7.81264276055331e-3
    , 7.812642760553379e-3
    ]
  , [ -0.7170049842821663 , 0.732692294198784 , 0.1544194846543815 ]
  , [ -0.7373497776866849
    , 0.7373497776866845
    , -2.7755575615628914e-17
    ]
  , [ -0.5717139272637514
    , 0.5968790484510277
    , 0.18667466541971128
    ]
  , [ -0.6710598534195159
    , 0.7297971305995982
    , 0.29182490099196917
    ]
  , [ -1.7990621802656183e-2
    , 1.9860825589150588e-2
    , 1.7990621802655427e-2
    ]
  , [ -0.7575092969305982
    , 0.7759039565328366
    , 0.16241732047101434
    ]
  , [ -0.532913153617887 , 0.6041132479674722 , 0.2962999185904398 ]
  , [ -0.6045930611639508
    , 0.7294381763744873
    , 0.41317238743168894
    ]
  , [ -0.715603493434698 , 0.780363714438081 , 0.3102929809778049 ]
  , [ 0.6491833829319443 , 0.6692949179238176 , -0.1694761444119623 ]
  , [ 0.6500310589666282 , 0.657748687643292 , -0.10873036208745479 ]
  , [ 0.6540866269118539 , 0.684435599216663 , -0.20191695153447145 ]
  , [ 0.5717139272637514
    , 0.5968790484510275
    , -0.18667466541971123
    ]
  , [ 0.78051441980069 , 0.8624994903802875 , -4.366114397309004e-2 ]
  , [ 0.7740595934432167
    , 0.8624994903802875
    , -9.43309492994287e-2
    ]
  , [ 0.6553927028305583
    , 0.6599214690464065
    , -7.682095370023323e-2
    ]
  , [ 0.8630726083767282 , 1.4862706190912907 , -0.1457666249505203 ]
  , [ 0.8651615019144336
    , 1.5020533690353601
    , -9.103018997631947e-2
    ]
  , [ 0.8545090595639963
    , 1.4798172601240018
    , -0.20522856413423346
    ]
  , [ 0.2834784042093155 , 1.2593473684859822 , 0.8800216675702618 ]
  , [ 0.3654127720849311 , 1.2970587028449378 , 0.8457844093935878 ]
  , [ 0.2831308101687339 , 1.1814109441614336 , 0.8788805908385666 ]
  , [ 0.18424327753183914 , 1.4736973001288365 , 0.8624748737615431 ]
  , [ -0.6846273996141601 , 0.8790068974537645 , 0.5388137355108445 ]
  , [ -0.6567496895904281 , 0.8790071413593897 , 0.5725795213922534 ]
  , [ -0.6464588188586459
    , 0.7823424436625966
    , 0.44119347577210544
    ]
  , [ -0.7213170770599064 , 0.908850927324427 , 0.5321478467750975 ]
  , [ -0.5181196004900488 , 0.7295797909722026 , 0.5181196004900487 ]
  , [ -0.7505675479671516 , 0.8781985739488957 , 0.4400805652422398 ]
  , [ 0.7708735521448062 , 0.8625025153160115 , 0.10873936568443998 ]
  , [ 0.6626047492685796 , 0.9655545808978969 , 0.6132901903841046 ]
  , [ 0.771074187418324 , 0.8625025153160115 , 0.108004379308 ]
  , [ 0.6419080894398502 , 0.7908779235632789 , 0.45940727414369065 ]
  , [ 0.6490953253086557 , 0.7194452653409781 , 0.31163387003160176 ]
  , [ 0.7391783489851426 , 1.1731157788543425 , 0.550164799101456 ]
  , [ -6.169852060031459e-17
    , 0.912056858034264
    , 0.8959767931433186
    ]
  , [ 0.1928232899241431 , 1.0886815800201057 , 0.8962126634940769 ]
  , [ 0.15370753430700868 , 0.907458117258076 , 0.879636949352576 ]
  , [ 0.18893822974950675 , 0.9120771417338429 , 0.8765366554024405 ]
  , [ 0.12280505144146822 , 0.8753822495491728 , 0.8567258893994794 ]
  , [ 3.4974828952650555e-2
    , 0.9074580789365942
    , 0.8918549800719824
    ]
  , [ 0.0 , 0.7712853278687586 , 0.7698054940256471 ]
  , [ 6.080525584912347e-2
    , 0.8753821429287594
    , 0.8631058252409901
    ]
  , [ 0.18667466541971134 , 0.5968790484510275 , 0.5717139272637516 ]
  , [ 0.29182490099196917 , 0.729797130599598 , 0.6710598534195159 ]
  , [ 1.7990621802653095e-2
    , 1.9860825589147996e-2
    , 1.799062180265376e-2
    ]
  , [ 0.15441948465438154 , 0.7326922941987839 , 0.7170049842821663 ]
  , [ 0.23969991808709235 , 0.8768406254951658 , 0.8337562524679074 ]
  , [ 0.289831893259673 , 0.876840812979899 , 0.818044872661731 ]
  , [ 0.21959663365004123 , 0.9080153576084199 , 0.866447685262698 ]
  , [ 0.16241732047101431 , 0.7759039565328366 , 0.757509296930598 ]
  , [ -0.49818544023253314
    , 1.4717742047093716
    , 0.7304675058910846
    ]
  , [ -0.5911999246804508 , 1.2599845756664991 , 0.7130046626566298 ]
  , [ 0.4068358188765504 , 1.74614655744181 , 0.4921333979229422 ]
  , [ 0.4787714299468666 , 1.6423746127113557 , 0.5796425375805216 ]
  , [ 0.4378291627777074 , 1.7613180604656151 , 0.43782916277770734 ]
  , [ 0.34656596334419265 , 1.7617035212112468 , 0.5125010376980281 ]
  , [ 0.35938264373453305 , 1.643431115641555 , 0.6590455307424883 ]
  , [ 0.44011909325789345 , 1.6091658630935881 , 0.6493805393140206 ]
  , [ 0.3077193099076989 , 1.6095465768220536 , 0.7210973557556136 ]
  , [ 0.30618459927572234 , 1.7463793702756347 , 0.5599378049841232 ]
  , [ 0.5851752797903262
    , 0.5851752797903265
    , -6.448622334243026e-2
    ]
  , [ 7.812642760551491e-3
    , 7.812642760551715e-3
    , -7.812642760550492e-3
    ]
  , [ -0.7622224951613341
    , 1.2957898827631245
    , -0.5194379814088045
    ]
  , [ -0.7304675058910846
    , 1.4717742047093714
    , -0.4981854402325332
    ]
  , [ -0.7130046626566298
    , 1.2599845756664991
    , -0.5911999246804508
    ]
  , [ -0.8102438227139428
    , 1.2598574705147236
    , -0.4474984305598346
    ]
  , [ 0.4206163489045973 , 1.4946619231456852 , -0.7670726439144389 ]
  , [ 0.4981854402325333 , 1.4717742047093716 , -0.7304675058910849 ]
  , [ 0.3507821217020296 , 1.4727320843657137 , -0.8105000296381791 ]
  , [ 0.4146046771732798 , 1.5166611367705873 , -0.755872347832471 ]
  , [ -0.5073894465820622 , 0.9128065261969955 , 0.7423359031749056 ]
  , [ -0.4768060384239586 , 0.9085520538404275 , 0.7584440485557873 ]
  , [ -0.5321478467750965 , 0.9088509273244271 , 0.7213170770599069 ]
  , [ -0.5903677161648284 , 1.1741465971067808 , 0.7119923208428119 ]
  , [ -0.5388137355108447 , 0.8790068974537645 , 0.6846273996141601 ]
  , [ -0.6097884029414722 , 0.9088511267237062 , 0.6572150990573598 ]
  , [ -0.29629991859043964
    , 0.6041132479674723
    , 0.5329131536178867
    ]
  , [ -2.2151036187758197e-2
    , 2.5331770412981625e-2
    , 2.215103618775873e-2
    ]
  , [ -0.41317238743168894
    , 0.7294381763744873
    , 0.6045930611639508
    ]
  , [ -0.31029298097780494 , 0.780363714438081 , 0.715603493434698 ]
  , [ -0.810500029638179
    , 1.4727320843657137
    , -0.35078212170202955
    ]
  , [ -0.7670726439144387
    , 1.4946619231456855
    , -0.42061634890459737
    ]
  , [ -0.8260665185299887
    , 1.4902762907980336
    , -0.2884082499027695
    ]
  , [ -0.8457844093935879
    , 1.2970587028449376
    , -0.3654127720849311
    ]
  , [ -0.18424327753183928
    , 1.4736973001288365
    , 0.8624748737615431
    ]
  , [ -0.3507821217020296 , 1.4727320843657137 , 0.8105000296381792 ]
  , [ -0.26432271823494774
    , 1.4970509470350615
    , 0.8320774835437866
    ]
  , [ -0.42061634890459737
    , 1.4946619231456855
    , 0.7670726439144387
    ]
  , [ -9.114226751346854e-2
    , 1.5000742593050618
    , 0.8663032960314866
    ]
  , [ -0.26089628483355987
    , 1.5173670225190703
    , 0.8208298543940763
    ]
  , [ 0.8208298543940764 , 1.5173670225190703 , 0.2608962848335599 ]
  , [ 0.8320774835437869 , 1.4970509470350615 , 0.2643227182349477 ]
  , [ 0.8228126117740736 , 1.5458407114144659 , 0.1707356424814656 ]
  , [ 0.7750736880510629 , 1.543449628093767 , 0.3300998702021285 ]
  , [ 0.8651615019144336
    , 1.5020533690353601
    , 9.103018997631947e-2
    ]
  , [ 0.8545090595639963 , 1.4798172601240018 , 0.2052285641342334 ]
  , [ 0.8562836943885954 , 1.517441568182377 , 9.01587519579244e-2 ]
  , [ 0.7673351760829068 , 1.6096835638600524 , 0.15978142628509895 ]
  , [ 0.8239288970287512 , 1.4058726252835607 , 0.35635039483568703 ]
  , [ 0.7733308622857777 , 1.287477942063759 , 0.502796814195706 ]
  , [ 0.4474984305598345 , 1.2598574705147234 , 0.8102438227139425 ]
  , [ 0.3507821217020293 , 1.4727320843657137 , 0.810500029638179 ]
  , [ 0.32461261897613036 , 0.9080154562527452 , 0.833535387138529 ]
  , [ 0.36590860085651866 , 1.0902530151825576 , 0.8421531448806923 ]
  , [ -0.6045930611639505
    , 0.7294381763744877
    , -0.41317238743168877
    ]
  , [ -0.4720306023200157
    , 0.6074273900130303
    , -0.3921629727500072
    ]
  , [ -0.646458818858646 , 0.7823424436625966 , -0.4411934757721056 ]
  , [ -0.5329131536178867
    , 0.6041132479674725
    , -0.2962999185904397
    ]
  , [ -0.6846273996141597
    , 0.8790068974537646
    , -0.5388137355108444
    ]
  , [ -0.7505675479671517
    , 0.8781985739488954
    , -0.44008056524223993
    ]
  , [ 0.29629991859043964 , 0.6041132479674723 , 0.5329131536178867 ]
  , [ 0.41317238743168894 , 0.7294381763744873 , 0.6045930611639508 ]
  , [ 2.215103618775549e-2
    , 2.533177041297828e-2
    , 2.21510361877556e-2
    ]
  , [ 0.31029298097780494 , 0.780363714438081 , 0.715603493434698 ]
  , [ -0.15441948465438154
    , 0.732692294198784
    , -0.7170049842821664
    ]
  , [ -6.448622334243025e-2
    , 0.5851752797903265
    , -0.5851752797903262
    ]
  , [ -0.16241732047101426
    , 0.7759039565328368
    , -0.7575092969305981
    ]
  , [ -0.18667466541971123
    , 0.5968790484510273
    , -0.5717139272637514
    ]
  , [ -0.5539859487435899
    , 0.7828729928425695
    , -0.5539859487435899
    ]
  , [ -0.5725795213922535
    , 0.8790071413593896
    , -0.6567496895904281
    ]
  , [ -0.5181196004900485
    , 0.7295797909722027
    , -0.5181196004900485
    ]
  , [ -0.6567496895904281
    , 0.8790071413593897
    , -0.5725795213922533
    ]
  , [ 0.8122497597351318 , 1.3634301491185252 , -0.396987025988275 ]
  , [ 0.7740579104368243
    , 0.862498015165331
    , -9.433724968365648e-2
    ]
  , [ 6.44862233424303e-2 , 0.5851752797903262 , 0.5851752797903267 ]
  , [ 7.812642760551684e-3
    , 7.812642760551713e-3
    , 7.812642760551824e-3
    ]
  , [ 0.19212615972377473 , 1.746701943306404 , 0.6080853167177859 ]
  , [ 0.2414067196592598 , 1.7624263437337935 , 0.568522694152947 ]
  , [ 0.22440550617121352 , 1.6454841596605991 , 0.7140470378267627 ]
  , [ 0.12443603223298119 , 1.763801627319081 , 0.6031237664636153 ]
  , [ -0.6254774679047082 , 1.4713967749278507 , 0.6254774679047082 ]
  , [ -0.558287861814229 , 1.4934020335516496 , 0.6747863108748675 ]
  , [ -0.6747863108748675 , 1.4934020335516496 , 0.558287861814229 ]
  , [ -0.6524829343907664 , 1.2954115625390261 , 0.6524829343907664 ]
  , [ 0.2571184603540509 , 1.8714260967581884 , 0.37885544369469604 ]
  , [ 0.28526260180018986
    , 1.8775138180920772
    , 0.34408768727411854
    ]
  , [ 0.21271144734580563 , 1.877827270100134 , 0.3923832499457651 ]
  , [ 0.18017138946943928 , 1.871613285510211 , 0.42041538751051694 ]
  , [ 0.5599378049841232 , 1.7463793702756347 , 0.30618459927572234 ]
  , [ 0.6590455307424883 , 1.643431115641555 , 0.3593826437345331 ]
  , [ 0.568522694152947 , 1.7624263437337935 , 0.2414067196592598 ]
  , [ 0.512501037698028 , 1.7617035212112466 , 0.34656596334419265 ]
  , [ 0.755872347832471 , 1.5166611367705873 , 0.4146046771732797 ]
  , [ 0.7210973557556135 , 1.6095465768220534 , 0.3077193099076989 ]
  , [ 0.7140470378267627 , 1.6454841596605991 , 0.2244055061712136 ]
  , [ 0.6493805393140206 , 1.6091658630935881 , 0.4401190932578934 ]
  , [ -0.8664476852626979
    , 0.9080153576084198
    , -0.21959663365004278
    ]
  , [ -0.8337562524679073
    , 0.8768406254951661
    , -0.2396999180870924
    ]
  , [ -0.8765366554024407
    , 0.9120771417338429
    , -0.18893822974950678
    ]
  , [ -0.833535387138529
    , 0.9080154562527452
    , -0.32461261897613036
    ]
  , [ 0.44689607487161953
    , 1.1765076558719516
    , -0.8091216099094027
    ]
  , [ 0.518569427022487 , 1.0903982826657406 , -0.7590404339629727 ]
  , [ 0.4474984305598345 , 1.2598574705147234 , -0.8102438227139426 ]
  , [ 0.36590860085651866
    , 1.0902530151825576
    , -0.8421531448806926
    ]
  , [ 0.5194379814088046 , 1.2957898827631247 , -0.7622224951613341 ]
  , [ 0.36541277208493106
    , 1.2970587028449376
    , -0.8457844093935878
    ]
  , [ 0.26432271823494774
    , 1.4970509470350615
    , -0.8320774835437867
    ]
  , [ 0.2834784042093156 , 1.2593473684859824 , -0.8800216675702618 ]
  , [ 0.283130810168734 , 1.1814109441614336 , -0.8788805908385665 ]
  , [ 0.19160298368259063 , 1.29931244670657 , -0.8997455590151622 ]
  , [ 1.7990621802653317e-2
    , 1.9860825589147875e-2
    , -1.799062180265354e-2
    ]
  , [ 0.647296121375148 , 0.6980028758147818 , -0.26667276723840716 ]
  , [ -0.86310582524099 , 0.8753821429287596 , 6.080525584912345e-2 ]
  , [ -0.7698054940256472 , 0.7712853278687583 , 0.0 ]
  , [ -0.8567258893994796
    , 0.8753822495491728
    , 0.12280505144146828
    ]
  , [ -0.891854980071982 , 0.9074580789365941 , 3.4974828952651e-2 ]
  , [ -0.8337562524679074
    , 0.8768406254951661
    , 0.23969991808709246
    ]
  , [ -0.8180448726617309 , 0.8768408129798988 , 0.289831893259673 ]
  , [ -0.8664476852626981 , 0.90801535760842 , 0.21959663365004095 ]
  , [ -0.879636949352576 , 0.907458117258076 , 0.15370753430700804 ]
  , [ -0.4400805652422398 , 0.8781985739488954 , 0.7505675479671517 ]
  , [ -0.3856172858664447 , 0.908552060323908 , 0.8081008260732252 ]
  , [ -0.3991852390173557 , 0.8781995871178615 , 0.772837999772326 ]
  , [ -0.44119347577210544
    , 0.7823424436625966
    , 0.6464588188586459
    ]
  , [ -0.5725795213922534 , 0.8790071413593897 , 0.6567496895904281 ]
  , [ -0.5539859487435899 , 0.7828729928425695 , 0.5539859487435899 ]
  , [ -0.6646325123013266 , 1.5161703861218 , 0.5499408317491791 ]
  , [ -0.7304675058910848
    , 1.4717742047093716
    , 0.49818544023253314
    ]
  , [ -0.7584440485557876 , 0.9085520538404273 , 0.4768060384239584 ]
  , [ -0.7728379997723258 , 0.8781995871178616 , 0.3991852390173557 ]
  , [ 0.8562836943885955
    , 1.5174415681823774
    , -9.015875195792439e-2
    ]
  , [ 0.8390008746485779
    , 1.547398454632929
    , 2.7755575615628914e-17
    ]
  , [ 0.16621776154382692
    , 1.9776053853655182
    , 0.16621776154382692
    ]
  , [ 0.34408768727411854
    , 1.8775138180920767
    , 0.28526260180018986
    ]
  , [ 0.15197602557430878
    , 1.9868541707659964
    , 0.15197602557430875
    ]
  , [ 0.3239549649324688 , 1.871257263189551 , 0.3239549649324688 ]
  , [ 0.3923832499457651 , 1.877827270100134 , 0.21271144734580563 ]
  , [ 0.4204153875105169 , 1.8716132855102106 , 0.18017138946943928 ]
  , [ 0.378855443694696 , 1.8714260967581884 , 0.25711846035405095 ]
  , [ 0.21242895637465672
    , 1.9790924197542168
    , 9.226906344799235e-2
    ]
  , [ 0.42437151237708004
    , 1.8784630967211673
    , 0.13411685349983243
    ]
  , [ 0.19692078525583517
    , 1.9867703444520366
    , 8.589118029151296e-2
    ]
  , [ 0.15978142628509898 , 1.6096835638600524 , 0.7673351760829068 ]
  , [ 0.17073564248146567 , 1.545840711414466 , 0.8228126117740738 ]
  , [ 7.668380969626697e-2
    , 1.6481991362041954
    , 0.7417373916617902
    ]
  , [ 0.3300998702021285 , 1.543449628093767 , 0.7750736880510631 ]
  , [ 0.4468960748716194 , 1.1765076558719516 , 0.8091216099094026 ]
  , [ 0.3581881925466474 , 0.9124253481133149 , 0.8235344289369642 ]
  , [ -2.215103618775549e-2
    , 2.533177041297907e-2
    , -2.2151036187755047e-2
    ]
  , [ -0.6710598534195159
    , 0.7297971305995978
    , -0.2918249009919692
    ]
  , [ -0.7584440485557876
    , 0.9085520538404273
    , -0.4768060384239584
    ]
  , [ -0.7728379997723258
    , 0.8781995871178616
    , -0.3991852390173557
    ]
  , [ 0.5717139272637514 , 0.5968790484510275 , 0.18667466541971128 ]
  , [ 1.7990621802653095e-2
    , 1.986082558914795e-2
    , 1.7990621802653206e-2
    ]
  , [ 0.6472961213751481 , 0.6980028758147817 , 0.26667276723840716 ]
  , [ 0.6469996697760713 , 0.7079075212097802 , 0.2926042880159187 ]
  , [ 0.6357399515521689 , 0.8593528035528301 , 0.568778121333005 ]
  , [ 0.6405215643858786 , 0.7748397465927047 , 0.4372196236016949 ]
  , [ 0.6045930611639507 , 0.7294381763744875 , 0.41317238743168894 ]
  , [ 0.5181196004900488 , 0.7295797909722026 , 0.5181196004900487 ]
  , [ 0.5321478467750966 , 0.908850927324427 , 0.7213170770599068 ]
  , [ 0.6097884029414722 , 0.9088511267237062 , 0.6572150990573598 ]
  , [ 0.5073894465820619 , 0.9128065261969955 , 0.7423359031749057 ]
  , [ 0.5388137355108445 , 0.8790068974537643 , 0.6846273996141601 ]
  , [ 0.518569427022487 , 1.0903982826657406 , 0.7590404339629727 ]
  , [ 0.4768060384239586 , 0.9085520538404275 , 0.7584440485557873 ]
  , [ 0.44008056524223993 , 0.8781985739488954 , 0.7505675479671516 ]
  , [ 0.44119347577210555 , 0.7823424436625965 , 0.646458818858646 ]
  , [ 0.3991852390173557 , 0.8781995871178615 , 0.772837999772326 ]
  , [ 0.3856172858664447 , 0.908552060323908 , 0.8081008260732252 ]
  , [ -0.6572150990573598 , 0.9088511267237064 , -0.609788402941472 ]
  , [ -0.6360954419937749 , 0.912960352086011 , -0.6360954419937749 ]
  , [ -0.7213170770599066
    , 0.9088509273244272
    , -0.5321478467750965
    ]
  , [ -0.7423359031749055
    , 0.9128065261969955
    , -0.5073894465820623
    ]
  , [ -0.3921629727500072
    , 0.6074273900130303
    , -0.4720306023200156
    ]
  , [ -2.3194160942469955e-2
    , 2.6785629054825896e-2
    , -2.3194160942469955e-2
    ]
  , [ -0.41317238743168894
    , 0.7294381763744873
    , -0.6045930611639508
    ]
  , [ -2.21510361877556e-2
    , 2.5331770412978235e-2
    , -2.215103618775549e-2
    ]
  , [ -1.7990621802653206e-2
    , 1.986082558914795e-2
    , -1.7990621802653095e-2
    ]
  , [ -0.2918249009919691
    , 0.7297971305995977
    , -0.6710598534195157
    ]
  , [ 9.869506660256812e-2
    , 1.1894267902510014
    , -0.9173511809757207
    ]
  , [ 0.192823289924143 , 1.0886815800201057 , -0.8962126634940769 ]
  , [ 9.87971813131522e-2
    , 1.2576022758257417
    , -0.9183913329068394
    ]
  , [ -2.624567824552988e-17
    , 1.0862351104914478
    , -0.9157767877158495
    ]
  , [ 6.169852060031459e-17
    , 0.912056858034264
    , -0.8959767931433186
    ]
  , [ -9.86950666025681e-2
    , 1.1894267902510014
    , -0.9173511809757204
    ]
  , [ -0.6097884029414723
    , 0.9088511267237063
    , -0.6572150990573598
    ]
  , [ -0.6502787934703502
    , 1.0902677960073714
    , -0.6502787934703501
    ]
  , [ -2.7755575615628914e-17
    , 0.7373497776866849
    , -0.7373497776866843
    ]
  , [ -7.812642760551656e-3
    , 7.812642760551715e-3
    , -7.81264276055138e-3
    ]
  , [ 0.640521564385879 , 0.7748397465927045 , -0.43721962360169503 ]
  , [ 0.6490953253086555 , 0.7194452653409784 , -0.3116338700316015 ]
  , [ 0.6419080894398494 , 0.7908779235632781 , -0.45940727414369 ]
  , [ 0.6045930611639505 , 0.7294381763744877 , -0.4131723874316888 ]
  , [ 0.7708735521448064
    , 0.8625025153160115
    , -0.10873936568444004
    ]
  , [ 0.6357399515521689 , 0.8593528035528301 , -0.568778121333005 ]
  , [ 0.7391783489851428 , 1.1731157788543423 , -0.5501647991014558 ]
  , [ 0.771074187418324 , 0.8625025153160115 , -0.10800437930799994 ]
  , [ 0.7733308622857775 , 1.287477942063759 , -0.5027968141957061 ]
  , [ 0.7271032447312008 , 1.1472482940426965 , -0.5673075249989148 ]
  , [ 0.6626047492685793 , 0.9655545808978973 , -0.6132901903841047 ]
  , [ 0.7710778571614444 , 0.862498015165331 , -0.10798328513535704 ]
  , [ 9.11422675134686e-2 , 1.5000742593050618 , 0.8663032960314866 ]
  , [ 8.706652684736651e-17
    , 1.5473984546329287
    , 0.839000874648578
    ]
  , [ 9.015875195792442e-2
    , 1.5174415681823772
    , 0.8562836943885956
    ]
  , [ 0.0 , 1.6095711978718583 , 0.7838769658257825 ]
  , [ -9.01587519579244e-2 , 1.517441568182377 , 0.8562836943885954 ]
  , [ -0.1707356424814655 , 1.5458407114144659 , 0.8228126117740736 ]
  , [ 9.3613324164243e-2 , 1.871978516403293 , 0.4470200027098403 ]
  , [ 6.613605971990155e-2
    , 1.7466998928928852
    , 0.6342820077896054
    ]
  , [ 0.13411685349983243 , 1.878463096721167 , 0.42437151237708 ]
  , [ 4.4461106873143405e-2
    , 1.8791795052256692
    , 0.4414598402161806
    ]
  , [ -7.668380969626695e-2
    , 1.6481991362041957
    , 0.7417373916617903
    ]
  , [ 0.0 , 1.764488100164783 , 0.6148766884290562 ]
  , [ -0.4146046771732797 , 1.5166611367705873 , 0.755872347832471 ]
  , [ -0.5499408317491791 , 1.5161703861218003 , 0.6646325123013266 ]
  , [ 9.226906344799235e-2
    , 1.979092419754217
    , 0.21242895637465672
    ]
  , [ 8.589118029151292e-2
    , 1.9867703444520366
    , 0.19692078525583512
    ]
  , [ 0.4921333979229422 , 1.74614655744181 , 0.40683581887655035 ]
  , [ 0.5796425375805216 , 1.6423746127113557 , 0.4787714299468666 ]
  , [ 0.2162856218653722
    , 1.986091229359059
    , 1.3877787807814457e-17
    ]
  , [ 0.4414598402161807
    , 1.8791795052256695
    , 4.4461106873143405e-2
    ]
  , [ 0.4414598402161807
    , 1.8791795052256695
    , -4.4461106873143405e-2
    ]
  , [ 0.21397126895807833 , 1.9871467005290857 , 0.0 ]
  , [ -0.7590404339629727
    , 1.0903982826657406
    , -0.5185694270224871
    ]
  , [ -0.7119923208428119
    , 1.1741465971067808
    , -0.5903677161648285
    ]
  , [ -0.8091216099094026
    , 1.1765076558719516
    , -0.4468960748716195
    ]
  , [ -0.8081008260732252
    , 0.9085520603239079
    , -0.3856172858664447
    ]
  , [ 0.3581881925466478 , 0.912425348113315 , -0.823534428936964 ]
  , [ 0.18893822974950705
    , 0.9120771417338429
    , -0.8765366554024404
    ]
  , [ 0.5911999246804508 , 1.2599845756664991 , -0.7130046626566298 ]
  , [ 0.5582878618142291 , 1.4934020335516496 , -0.6747863108748675 ]
  , [ 0.7119923208428119 , 1.1741465971067808 , -0.5903677161648284 ]
  , [ 0.6363011668042756 , 0.9129203244659402 , -0.635839187329811 ]
  , [ 0.4720306023200157 , 0.60742739001303 , -0.3921629727500073 ]
  , [ 2.3194160942469733e-2
    , 2.6785629054825906e-2
    , -2.3194160942469733e-2
    ]
  , [ 0.5181196004900488 , 0.7295797909722026 , -0.5181196004900487 ]
  , [ 0.5329131536178869 , 0.6041132479674722 , -0.2962999185904397 ]
  , [ 0.6469996697760714
    , 0.7079075212097801
    , -0.29260428801591876
    ]
  , [ 2.215103618775549e-2
    , 2.533177041297907e-2
    , -2.2151036187755047e-2
    ]
  , [ 2.2151036187755713e-2
    , 2.5331770412978624e-2
    , -2.215103618775549e-2
    ]
  , [ 0.39216297275000744 , 0.60742739001303 , -0.4720306023200158 ]
  , [ -0.8796369493525764
    , 0.9074581172580761
    , -0.15370753430700962
    ]
  , [ -0.896212663494077
    , 1.0886815800201057
    , -0.19282328992414308
    ]
  , [ -0.6572150990573599 , 0.9088511267237063 , 0.6097884029414724 ]
  , [ -0.6360954419937749 , 0.912960352086011 , 0.6360954419937749 ]
  , [ -0.512501037698028 , 1.7617035212112466 , 0.34656596334419265 ]
  , [ -0.378855443694696 , 1.8714260967581886 , 0.2571184603540509 ]
  , [ -0.4921333979229422 , 1.74614655744181 , 0.4068358188765504 ]
  , [ -0.5599378049841232
    , 1.7463793702756347
    , 0.30618459927572234
    ]
  , [ -0.6747863108748675 , 1.4934020335516496 , -0.558287861814229 ]
  , [ -0.755872347832471 , 1.5166611367705873 , -0.4146046771732797 ]
  , [ -0.8081008260732245
    , 0.9085520603239081
    , 0.38561728586644495
    ]
  , [ -0.8335353871385288
    , 0.9080154562527454
    , 0.32461261897613114
    ]
  , [ 0.7271032447312005 , 1.1472482940426967 , 0.5673075249989148 ]
  , [ 0.6363011668042756 , 0.9129203244659402 , 0.635839187329811 ]
  , [ 0.8105000296381791 , 1.4727320843657137 , 0.3507821217020296 ]
  , [ 0.7670726439144389 , 1.4946619231456855 , 0.4206163489045974 ]
  , [ 0.7838769658257825 , 1.6095711978718585 , 0.0 ]
  , [ 0.7417373916617903
    , 1.6481991362041957
    , 7.668380969626695e-2
    ]
  , [ 0.7417373916617902 , 1.6481991362041954 , -7.6683809696267e-2 ]
  , [ 0.7673351760829068
    , 1.6096835638600524
    , -0.15978142628509898
    ]
  , [ 0.6342820077896054
    , 1.7466998928928852
    , -6.613605971990152e-2
    ]
  , [ 0.8228126117740736 , 1.5458407114144659 , -0.1707356424814655 ]
  , [ 0.42061634890459737 , 1.4946619231456855 , 0.7670726439144389 ]
  , [ 0.26432271823494774 , 1.4970509470350617 , 0.8320774835437867 ]
  , [ 0.4981854402325333 , 1.4717742047093716 , 0.7304675058910848 ]
  , [ 0.41460467717327976 , 1.5166611367705878 , 0.7558723478324711 ]
  , [ 0.4731627371704971 , 1.5418687589859696 , 0.6987516862837263 ]
  , [ 0.2608962848335599 , 1.5173670225190703 , 0.8208298543940764 ]
  , [ -0.5717139272637514
    , 0.5968790484510275
    , -0.18667466541971128
    ]
  , [ -1.7990621802653317e-2
    , 1.9860825589147875e-2
    , -1.799062180265354e-2
    ]
  , [ -0.7170049842821663
    , 0.7326922941987838
    , -0.15441948465438157
    ]
  , [ -0.7575092969305981
    , 0.7759039565328365
    , -0.16241732047101434
    ]
  , [ -0.8180448726617309 , 0.876840812979899 , -0.289831893259673 ]
  , [ -0.715603493434698 , 0.7803637144380808 , -0.310292980977805 ]
  , [ -0.823534428936964 , 0.912425348113315 , -0.3581881925466473 ]
  , [ -0.8421531448806922
    , 1.0902530151825576
    , -0.36590860085651855
    ]
  , [ 0.5329131536178869 , 0.6041132479674723 , 0.2962999185904397 ]
  , [ 0.5539859487435901 , 0.7828729928425694 , 0.55398594874359 ]
  , [ 0.6360954419937748 , 0.9129603520860108 , 0.6360954419937748 ]
  , [ 0.5725795213922534 , 0.8790071413593897 , 0.656749689590428 ]
  , [ 0.6502787934703501 , 1.0902677960073712 , 0.6502787934703502 ]
  , [ 0.7558723478324709
    , 1.5166611367705873
    , -0.41460467717327976
    ]
  , [ 0.7670726439144389 , 1.4946619231456852 , -0.4206163489045974 ]
  , [ 0.7750736880510629 , 1.543449628093767 , -0.3300998702021285 ]
  , [ 0.6987516862837263
    , 1.5418687589859699
    , -0.47316273717049717
    ]
  , [ -0.19282328992414305
    , 1.0886815800201057
    , -0.896212663494077
    ]
  , [ -0.18893822974950705
    , 0.9120771417338428
    , -0.8765366554024404
    ]
  , [ -0.283130810168734 , 1.1814109441614336 , -0.8788805908385667 ]
  , [ -0.28347840420931564
    , 1.2593473684859822
    , -0.8800216675702618
    ]
  , [ -0.36590860085651855
    , 1.0902530151825576
    , -0.8421531448806925
    ]
  , [ -9.879718131315204e-2
    , 1.2576022758257417
    , -0.9183913329068394
    ]
  , [ -0.5388137355108445
    , 0.8790068974537645
    , -0.6846273996141601
    ]
  , [ -0.5321478467750966 , 0.908850927324427 , -0.7213170770599068 ]
  , [ -0.358188192546648 , 0.912425348113315 , -0.8235344289369639 ]
  , [ -0.32461261897613036
    , 0.9080154562527454
    , -0.8335353871385288
    ]
  , [ -0.3856172858664454
    , 0.9085520603239081
    , -0.8081008260732248
    ]
  , [ -0.39918523901735564
    , 0.8781995871178618
    , -0.7728379997723256
    ]
  , [ -0.47680603842395847
    , 0.9085520538404271
    , -0.7584440485557874
    ]
  , [ -0.4468960748716195
    , 1.1765076558719516
    , -0.8091216099094026
    ]
  , [ -0.44749843055983474
    , 1.2598574705147236
    , -0.8102438227139428
    ]
  , [ -0.518569427022487 , 1.0903982826657406 , -0.7590404339629725 ]
  , [ -0.5073894465820619
    , 0.9128065261969955
    , -0.7423359031749057
    ]
  , [ -0.5903677161648284
    , 1.1741465971067808
    , -0.7119923208428119
    ]
  , [ -0.2962999185904398 , 0.6041132479674721 , -0.532913153617887 ]
  , [ -0.44119347577210544
    , 0.7823424436625968
    , -0.6464588188586459
    ]
  , [ -0.31029298097780494
    , 0.7803637144380812
    , -0.7156034934346979
    ]
  , [ -0.44008056524224 , 0.8781985739488956 , -0.7505675479671517 ]
  , [ -0.28983189325967296
    , 0.8768408129798992
    , -0.8180448726617306
    ]
  , [ -0.23969991808709237
    , 0.8768406254951662
    , -0.8337562524679074
    ]
  , [ -0.21959663365004273
    , 0.9080153576084198
    , -0.8664476852626977
    ]
  , [ 0.18424327753183933
    , 1.4736973001288365
    , -0.8624748737615431
    ]
  , [ -2.7755575615628914e-17
    , 1.3011746498142651
    , -0.9190561156885906
    ]
  , [ 6.448622334243023e-2
    , 0.5851752797903262
    , -0.5851752797903267
    ]
  , [ 0.0 , 0.7712853278687584 , -0.7698054940256471 ]
  , [ 0.6646325123013265 , 1.5161703861218 , -0.549940831749179 ]
  , [ 0.6493805393140206 , 1.6091658630935881 , -0.4401190932578934 ]
  , [ 0.6590455307424883 , 1.643431115641555 , -0.3593826437345331 ]
  , [ 0.5796425375805215
    , 1.6423746127113557
    , -0.47877142994686667
    ]
  , [ 0.0 , 1.9860912293590587 , 0.21628562186537217 ]
  , [ 0.0 , 1.8719852675758741 , 0.4566121245194575 ]
  , [ 0.0 , 2.0282480595325483 , 8.294756480297713e-2 ]
  , [ 1.6023498781971006e-2
    , 2.0298344402390818
    , 7.461906659600522e-2
    ]
  , [ -4.163336342344337e-17
    , 1.9871467005290855
    , 0.21397126895807828
    ]
  , [ -1.6023498781971006e-2
    , 2.0298344402390818
    , 7.461906659600522e-2
    ]
  , [ 0.15197602557430875
    , 1.9868541707659964
    , 0.15197602557430875
    ]
  , [ 8.352331765507048e-2
    , 1.9874788959388558
    , 0.19585993121305542
    ]
  , [ 3.459146906505174e-2
    , 2.028192844530145
    , 7.606128724075058e-2
    ]
  , [ 1.3877787807814457e-17
    , 1.9871467005290855
    , 0.2139712689580783
    ]
  , [ -0.5851752797903265
    , 0.5851752797903264
    , -6.448622334243026e-2
    ]
  , [ -7.812642760551698e-3
    , 7.81264276055331e-3
    , -7.812642760553379e-3
    ]
  , [ -0.8631058252409901
    , 0.8753821429287594
    , -6.080525584912344e-2
    ]
  , [ -0.8567258893994794
    , 0.8753822495491729
    , -0.12280505144146825
    ]
  , [ -0.4731627371704971 , 1.5418687589859696 , 0.6987516862837264 ]
  , [ -0.5970610762747673 , 1.5413389851975459 , 0.5970610762747673 ]
  , [ -0.6254774679047082
    , 1.4713967749278507
    , -0.6254774679047082
    ]
  , [ -0.558287861814229 , 1.4934020335516496 , -0.6747863108748675 ]
  , [ -0.6524829343907664
    , 1.2954115625390261
    , -0.6524829343907664
    ]
  , [ -0.6646325123013265 , 1.5161703861218 , -0.549940831749179 ]
  , [ 0.6342820077896054
    , 1.7466998928928852
    , 6.613605971990152e-2
    ]
  , [ 0.6080853167177859 , 1.7467019433064042 , 0.19212615972377467 ]
  , [ 0.5548468717518389 , 1.6090000296403644 , 0.5548468717518389 ]
  , [ 0.6987516862837264 , 1.5418687589859699 , 0.47316273717049717 ]
  , [ 0.7304675058910848 , 1.4717742047093716 , 0.4981854402325333 ]
  , [ 0.6646325123013265 , 1.5161703861218 , 0.5499408317491791 ]
  , [ 0.6747863108748674 , 1.4934020335516496 , 0.5582878618142292 ]
  , [ 0.5970610762747673 , 1.5413389851975459 , 0.5970610762747673 ]
  , [ 0.6524829343907664 , 1.2954115625390261 , -0.6524829343907664 ]
  , [ 0.5903677161648287 , 1.1741465971067808 , -0.7119923208428119 ]
  , [ 0.7130046626566298 , 1.2599845756664991 , -0.5911999246804508 ]
  , [ 0.6254774679047082 , 1.4713967749278507 , -0.6254774679047082 ]
  , [ 0.7622224951613341 , 1.2957898827631247 , -0.5194379814088045 ]
  , [ 0.6502787934703502 , 1.0902677960073714 , -0.6502787934703502 ]
  , [ 0.5499408317491791 , 1.5161703861218003 , 0.6646325123013266 ]
  , [ 0.5582878618142292 , 1.4934020335516496 , 0.6747863108748676 ]
  , [ -0.8306211796074129 , 1.5568496825908318 , 0.0 ]
  , [ -0.7838769658257825 , 1.6095711978718583 , 0.0 ]
  , [ -0.8338378098771851
    , 1.5431253932352957
    , 8.699039391932935e-2
    ]
  , [ -0.8338378098771851
    , 1.5431253932352957
    , -8.699039391932935e-2
    ]
  , [ -0.8788805908385666
    , 1.1814109441614336
    , -0.28313081016873404
    ]
  , [ -0.9170605544812714
    , 1.1880416776333664
    , -9.998920445288839e-2
    ]
  , [ -0.9058201893253578
    , 1.2360005696799088
    , -0.15764435612292219
    ]
  , [ -0.9173135774468576
    , 1.1869621129641121
    , -9.633778848140394e-2
    ]
  , [ -0.7130046626566299 , 1.2599845756664991 , 0.5911999246804511 ]
  , [ -0.6502787934703503 , 1.0902677960073714 , 0.6502787934703503 ]
  , [ -0.7417373916617903
    , 1.6481991362041957
    , 7.668380969626695e-2
    ]
  , [ -0.7417373916617903
    , 1.6481991362041957
    , -7.668380969626695e-2
    ]
  , [ -0.6342820077896054
    , 1.7466998928928854
    , 6.613605971990152e-2
    ]
  , [ -0.7673351760829068
    , 1.6096835638600524
    , 0.15978142628509898
    ]
  , [ -0.6031237664636153 , 1.763801627319081 , 0.12443603223298116 ]
  , [ -0.6148766884290562 , 1.764488100164783 , 0.0 ]
  , [ -0.4566121245194575
    , 1.871985267575874
    , -1.830024316166545e-17
    ]
  , [ -0.6342820077896054
    , 1.7466998928928852
    , -6.613605971990157e-2
    ]
  , [ -0.34408768727411854
    , 1.877513818092077
    , 0.28526260180018986
    ]
  , [ -0.3923832499457651 , 1.877827270100134 , 0.2127114473458056 ]
  , [ -0.19692078525583515
    , 1.9867703444520366
    , 8.589118029151296e-2
    ]
  , [ -0.15197602557430873
    , 1.9868541707659966
    , 0.15197602557430875
    ]
  , [ -0.19585993121305542
    , 1.9874788959388558
    , 8.35233176550705e-2
    ]
  , [ -0.21242895637465672
    , 1.979092419754217
    , 9.226906344799235e-2
    ]
  , [ -0.7140470378267627 , 1.645484159660599 , 0.22440550617121358 ]
  , [ -0.8228126117740738 , 1.545840711414466 , 0.17073564248146567 ]
  , [ -0.7558723478324709
    , 1.5166611367705873
    , 0.41460467717327976
    ]
  , [ -0.6987516862837263
    , 1.5418687589859699
    , 0.47316273717049717
    ]
  , [ -0.7750736880510629 , 1.543449628093767 , 0.3300998702021285 ]
  , [ -0.7670726439144389 , 1.4946619231456852 , 0.4206163489045974 ]
  , [ -0.7210973557556136
    , 1.6095465768220536
    , 0.30771930990769886
    ]
  , [ -0.8208298543940764 , 1.5173670225190703 , 0.2608962848335599 ]
  , [ -0.5911999246804508
    , 1.2599845756664991
    , -0.7130046626566298
    ]
  , [ -0.5194379814088046
    , 1.2957898827631247
    , -0.7622224951613342
    ]
  , [ 0.6254774679047082 , 1.4713967749278507 , 0.625477467904708 ]
  , [ 0.7622224951613341 , 1.2957898827631245 , 0.5194379814088045 ]
  , [ 0.5599378049841232
    , 1.7463793702756347
    , -0.30618459927572234
    ]
  , [ 0.568522694152947 , 1.7624263437337935 , -0.2414067196592598 ]
  , [ 0.512501037698028 , 1.7617035212112468 , -0.34656596334419265 ]
  , [ 0.7210973557556135 , 1.6095465768220536 , -0.3077193099076989 ]
  , [ 0.4204153875105169
    , 1.8716132855102106
    , -0.18017138946943928
    ]
  , [ 0.42437151237708004
    , 1.8784630967211673
    , -0.13411685349983243
    ]
  , [ 0.3923832499457651 , 1.877827270100134 , -0.2127114473458056 ]
  , [ 0.4470200027098403 , 1.871978516403293 , -9.3613324164243e-2 ]
  , [ 0.21242895637465675
    , 1.979092419754217
    , -9.22690634479924e-2
    ]
  , [ 0.6080853167177859 , 1.7467019433064044 , -0.1921261597237747 ]
  , [ 0.7140470378267627
    , 1.6454841596605991
    , -0.22440550617121363
    ]
  , [ 0.6031237664636153
    , 1.7638016273190815
    , -0.12443603223298119
    ]
  , [ 0.6524829343907664 , 1.2954115625390261 , 0.6524829343907664 ]
  , [ 0.5194379814088046 , 1.2957898827631245 , 0.7622224951613343 ]
  , [ -0.7622224951613341 , 1.2957898827631245 , 0.5194379814088045 ]
  , [ -0.8105000296381791 , 1.4727320843657137 , 0.3507821217020296 ]
  , [ -0.1228050514414682
    , 0.8753822495491729
    , -0.8567258893994794
    ]
  , [ -6.080525584912345e-2
    , 0.8753821429287596
    , -0.86310582524099
    ]
  , [ -0.15370753430700937
    , 0.907458117258076
    , -0.8796369493525762
    ]
  , [ -3.497482895265056e-2
    , 0.9074580789365942
    , -0.8918549800719824
    ]
  , [ 9.114226751346854e-2
    , 1.5000742593050618
    , -0.8663032960314866
    ]
  , [ 0.2608962848335599 , 1.5173670225190703 , -0.8208298543940764 ]
  , [ 0.3300998702021285 , 1.5434496280937666 , -0.7750736880510629 ]
  , [ 0.17073564248146547 , 1.545840711414466 , -0.8228126117740737 ]
  , [ -0.1916029836825906 , 1.29931244670657 , -0.8997455590151622 ]
  , [ 5.276171965179255e-17
    , 1.4739386059327377
    , -0.8813815581455101
    ]
  , [ 6.080525584912359e-2
    , 0.8753821429287593
    , -0.8631058252409899
    ]
  , [ 3.497482895265053e-2
    , 0.9074580789365946
    , -0.8918549800719818
    ]
  , [ 0.15441948465438154 , 0.732692294198784 , -0.7170049842821665 ]
  , [ 7.812642760551658e-3
    , 7.8126427605515e-3
    , -7.812642760551602e-3
    ]
  , [ 0.4131723874316888 , 0.7294381763744876 , -0.6045930611639508 ]
  , [ 0.5539859487435899 , 0.7828729928425695 , -0.5539859487435899 ]
  , [ 0.44119347577210555
    , 0.7823424436625966
    , -0.6464588188586459
    ]
  , [ 0.2962999185904397 , 0.6041132479674723 , -0.5329131536178867 ]
  , [ 0.7304675058910848
    , 1.4717742047093716
    , -0.49818544023253314
    ]
  , [ 0.8239288970287512
    , 1.4058726252835607
    , -0.35635039483568703
    ]
  , [ 0.6747863108748675 , 1.4934020335516496 , -0.558287861814229 ]
  , [ 0.8105000296381791 , 1.4727320843657137 , -0.3507821217020296 ]
  , [ -0.8228126117740736 , 1.545840711414466 , -0.1707356424814655 ]
  , [ -0.7673351760829068
    , 1.6096835638600524
    , -0.15978142628509898
    ]
  , [ -0.8208298543940763 , 1.5173670225190703 , -0.26089628483356 ]
  , [ -0.8254897540595187
    , 1.5435692417971658
    , -0.16429080162183143
    ]
  , [ -0.7750736880510629 , 1.543449628093767 , -0.3300998702021285 ]
  , [ -0.8228724369633584
    , 1.5136775995208986
    , -0.26151852913476525
    ]
  , [ -0.2414067196592598 , 1.7624263437337935 , 0.568522694152947 ]
  , [ -0.18017138946943925
    , 1.8716132855102106
    , 0.4204153875105169
    ]
  , [ -0.19212615972377473 , 1.746701943306404 , 0.6080853167177859 ]
  , [ -0.3061845992757223 , 1.7463793702756347 , 0.5599378049841232 ]
  , [ -0.13411685349983243 , 1.878463096721167 , 0.42437151237708 ]
  , [ -0.2127114473458056 , 1.877827270100134 , 0.3923832499457651 ]
  , [ -0.12443603223298119 , 1.763801627319081 , 0.6031237664636153 ]
  , [ -0.22440550617121352
    , 1.6454841596605991
    , 0.7140470378267627
    ]
  , [ -6.613605971990154e-2
    , 1.7466998928928852
    , 0.6342820077896054
    ]
  , [ -0.15978142628509898
    , 1.6096835638600524
    , 0.7673351760829068
    ]
  , [ -4.4461106873143405e-2
    , 1.8791795052256695
    , 0.4414598402161806
    ]
  , [ -9.361332416424303e-2
    , 1.871978516403293
    , 0.4470200027098403
    ]
  , [ -5.957385788325474e-2
    , 2.0280904764199947
    , 5.957385788325473e-2
    ]
  , [ -4.433393286244257e-2
    , 2.0294496432112377
    , 6.476360671202533e-2
    ]
  , [ -6.476360671202533e-2
    , 2.0294496432112377
    , 4.433393286244257e-2
    ]
  , [ -0.15197602557430875
    , 1.9868541707659964
    , 0.15197602557430875
    ]
  , [ -8.352331765507041e-2
    , 1.9874788959388558
    , 0.19585993121305537
    ]
  , [ -3.459146906505173e-2
    , 2.028192844530145
    , 7.606128724075058e-2
    ]
  , [ -8.589118029151296e-2
    , 1.9867703444520366
    , 0.19692078525583517
    ]
  , [ -9.226906344799235e-2
    , 1.9790924197542166
    , 0.21242895637465672
    ]
  , [ 0.19585993121305542
    , 1.9874788959388558
    , 8.352331765507048e-2
    ]
  , [ 7.606128724075059e-2
    , 2.028192844530145
    , 3.459146906505173e-2
    ]
  , [ 1.607119023392417e-3
    , 2.0437414787107526
    , 1.6071190233924448e-3
    ]
  , [ 4.433393286244257e-2
    , 2.0294496432112377
    , 6.476360671202529e-2
    ]
  , [ -0.43782916277770734 , 1.761318060465615 , 0.4378291627777074 ]
  , [ -0.3239549649324688 , 1.871257263189551 , 0.3239549649324688 ]
  , [ -0.4068358188765504 , 1.74614655744181 , 0.4921333979229422 ]
  , [ -0.5796425375805216 , 1.6423746127113557 , 0.4787714299468666 ]
  , [ -0.3300998702021285 , 1.543449628093767 , 0.7750736880510629 ]
  , [ -0.44011909325789345
    , 1.6091658630935883
    , 0.6493805393140207
    ]
  , [ -0.6493805393140206 , 1.6091658630935881 , 0.4401190932578934 ]
  , [ -0.5548468717518389 , 1.6090000296403644 , 0.5548468717518389 ]
  , [ -0.6590455307424883 , 1.643431115641555 , 0.3593826437345331 ]
  , [ -0.47877142994686667
    , 1.6423746127113557
    , 0.5796425375805216
    ]
  , [ -0.5499408317491791
    , 1.5161703861218003
    , -0.6646325123013266
    ]
  , [ -0.49818544023253325
    , 1.4717742047093716
    , -0.7304675058910849
    ]
  , [ -0.36541277208493106
    , 1.2970587028449376
    , -0.8457844093935878
    ]
  , [ -0.4206163489045974
    , 1.4946619231456855
    , -0.7670726439144389
    ]
  , [ 0.6031237664636153 , 1.7638016273190815 , 0.12443603223298119 ]
  , [ 0.4470200027098403 , 1.871978516403293 , 9.3613324164243e-2 ]
  , [ 0.1958599312130554
    , 1.9874788959388558
    , -8.352331765507048e-2
    ]
  , [ 0.2139712689580783 , 1.9871467005290855 , 0.0 ]
  , [ 0.19692078525583515
    , 1.9867703444520366
    , -8.589118029151296e-2
    ]
  , [ 7.606128724075059e-2
    , 2.028192844530145
    , -3.459146906505173e-2
    ]
  , [ 7.46190665960052e-2
    , 2.0298344402390818
    , -1.6023498781971e-2
    ]
  , [ 6.47636067120253e-2
    , 2.0294496432112377
    , -4.433393286244257e-2
    ]
  , [ 0.5548468717518389 , 1.6090000296403646 , -0.5548468717518389 ]
  , [ 0.5970610762747675 , 1.5413389851975459 , -0.5970610762747675 ]
  , [ 0.47877142994686667
    , 1.6423746127113557
    , -0.5796425375805215
    ]
  , [ 0.4921333979229422 , 1.74614655744181 , -0.40683581887655035 ]
  , [ 0.44011909325789345
    , 1.6091658630935881
    , -0.6493805393140206
    ]
  , [ 0.40683581887655035 , 1.74614655744181 , -0.4921333979229422 ]
  , [ -0.41460467717327976
    , 1.5166611367705878
    , -0.7558723478324711
    ]
  , [ -0.3300998702021285 , 1.543449628093767 , -0.7750736880510629 ]
  , [ -0.4731627371704971
    , 1.5418687589859696
    , -0.6987516862837263
    ]
  , [ -0.3507821217020296
    , 1.4727320843657137
    , -0.8105000296381792
    ]
  , [ 0.2244055061712136 , 1.6454841596605991 , -0.7140470378267627 ]
  , [ 0.3077193099076989 , 1.6095465768220536 , -0.7210973557556135 ]
  , [ 0.19212615972377467 , 1.7467019433064042 , -0.608085316717786 ]
  , [ 0.15978142628509895
    , 1.6096835638600524
    , -0.7673351760829068
    ]
  , [ 0.4731627371704971 , 1.5418687589859696 , -0.6987516862837264 ]
  , [ 0.549940831749179 , 1.5161703861218 , -0.6646325123013266 ]
  , [ 0.35938264373453305 , 1.643431115641555 , -0.6590455307424883 ]
  , [ 0.30618459927572234
    , 1.7463793702756347
    , -0.5599378049841232
    ]
  , [ -0.886198300038301
    , 1.2718626269690558
    , -0.25470716973839896
    ]
  , [ -0.880021667570262
    , 1.2593473684859824
    , -0.28347840420931564
    ]
  , [ -0.7140470378267627
    , 1.6454841596605991
    , -0.2244055061712136
    ]
  , [ -0.6080853167177859
    , 1.7467019433064042
    , -0.19212615972377467
    ]
  , [ -0.7210973557556135
    , 1.6095465768220534
    , -0.3077193099076989
    ]
  , [ -0.6031237664636153
    , 1.7638016273190813
    , -0.12443603223298118
    ]
  , [ -0.25711846035405095
    , 1.8714260967581882
    , 0.3788554436946959
    ]
  , [ -0.28526260180018986
    , 1.8775138180920772
    , 0.34408768727411854
    ]
  , [ -0.34656596334419265
    , 1.7617035212112468
    , 0.5125010376980281
    ]
  , [ -0.4204153875105169
    , 1.8716132855102106
    , 0.18017138946943928
    ]
  , [ -0.42437151237708004
    , 1.8784630967211673
    , 0.13411685349983243
    ]
  , [ -0.6080853167177859 , 1.7467019433064044 , 0.1921261597237747 ]
  , [ -0.568522694152947 , 1.7624263437337935 , 0.2414067196592598 ]
  , [ 0.7130046626566298 , 1.2599845756664991 , 0.5911999246804508 ]
  , [ 0.7119923208428119 , 1.1741465971067808 , 0.5903677161648285 ]
  , [ 0.45661212451945754 , 1.8719852675758741 , 0.0 ]
  , [ 0.6148766884290562 , 1.764488100164783 , 0.0 ]
  , [ 0.5911999246804508 , 1.2599845756664991 , 0.7130046626566298 ]
  , [ 0.5903677161648285 , 1.1741465971067808 , 0.7119923208428119 ]
  , [ -0.9058201893253579
    , 1.2360005696799088
    , 0.15764435612292219
    ]
  , [ -0.8861983000383011
    , 1.2718626269690558
    , 0.25470716973839896
    ]
  , [ -0.9170605544812716
    , 1.1880416776333662
    , 9.998920445288842e-2
    ]
  , [ -0.8254897540595187
    , 1.5435692417971658
    , 0.16429080162183185
    ]
  , [ -0.9173135774468574
    , 1.1869621129641121
    , 9.633778848140408e-2
    ]
  , [ -0.9157767877158494 , 1.0862351104914478 , 0.0 ]
  , [ -0.8765366554024407
    , 0.9120771417338429
    , 0.18893822974950678
    ]
  , [ -0.8235344289369638 , 0.9124253481133151 , 0.358188192546648 ]
  , [ -0.3077193099076989 , 1.6095465768220536 , 0.7210973557556136 ]
  , [ -0.35938264373453305 , 1.643431115641555 , 0.6590455307424883 ]
  , [ 0.8208298543940763
    , 1.5173670225190703
    , -0.26089628483355987
    ]
  , [ 0.8320774835437867
    , 1.4970509470350615
    , -0.26432271823494774
    ]
  , [ 0.29182490099196934
    , 0.7297971305995975
    , -0.6710598534195162
    ]
  , [ 0.47680603842395847
    , 0.9085520538404271
    , -0.7584440485557874
    ]
  , [ 0.5073894465820622 , 0.9128065261969955 , -0.7423359031749056 ]
  , [ 0.3856172858664447 , 0.908552060323908 , -0.8081008260732252 ]
  , [ 0.44008056524223993
    , 0.8781985739488957
    , -0.7505675479671516
    ]
  , [ 0.3991852390173557 , 0.8781995871178615 , -0.772837999772326 ]
  , [ 0.5388137355108445 , 0.8790068974537643 , -0.6846273996141601 ]
  , [ 9.01587519579244e-2 , 1.517441568182377 , -0.8562836943885954 ]
  , [ 7.668380969626695e-2
    , 1.6481991362041957
    , -0.7417373916617903
    ]
  , [ 0.6360954419937748 , 0.9129603520860109 , -0.636095441993775 ]
  , [ 0.5321478467750965 , 0.9088509273244271 , -0.7213170770599069 ]
  , [ 5.957385788325474e-2
    , 2.0280904764199947
    , 5.957385788325473e-2
    ]
  , [ 6.476360671202533e-2
    , 2.0294496432112377
    , 4.433393286244257e-2
    ]
  , [ 2.988199908178335e-3
    , 2.0433955508208292
    , 2.988199908178321e-3
    ]
  , [ -0.8918549800719823
    , 0.9074580789365944
    , -3.497482895265047e-2
    ]
  , [ -0.8959767931433185
    , 0.912056858034264
    , -2.7755575615628914e-17
    ]
  , [ -0.4470200027098403
    , 1.871978516403293
    , -9.361332416424299e-2
    ]
  , [ -0.42437151237708 , 1.878463096721167 , -0.13411685349983246 ]
  , [ -0.4414598402161806
    , 1.8791795052256695
    , -4.4461106873143384e-2
    ]
  , [ -0.568522694152947 , 1.7624263437337935 , -0.2414067196592598 ]
  , [ -0.18424327753183928
    , 1.4736973001288365
    , -0.8624748737615431
    ]
  , [ -0.26432271823494774
    , 1.4970509470350615
    , -0.8320774835437866
    ]
  , [ -9.114226751346857e-2
    , 1.5000742593050618
    , -0.8663032960314865
    ]
  , [ -0.26089628483355987
    , 1.5173670225190703
    , -0.8208298543940763
    ]
  , [ 0.34656596334419265 , 1.7617035212112468 , -0.512501037698028 ]
  , [ 0.2414067196592598 , 1.7624263437337935 , -0.568522694152947 ]
  , [ 0.15197602557430875
    , 1.9868541707659966
    , -0.15197602557430875
    ]
  , [ 0.378855443694696 , 1.8714260967581884 , -0.25711846035405095 ]
  , [ -0.3077193099076989
    , 1.6095465768220534
    , -0.7210973557556135
    ]
  , [ -0.17073564248146547
    , 1.545840711414466
    , -0.8228126117740737
    ]
  , [ -0.2244055061712136
    , 1.6454841596605991
    , -0.7140470378267627
    ]
  , [ -0.35938264373453305
    , 1.643431115641555
    , -0.6590455307424883
    ]
  , [ 4.446110687314339e-2
    , 1.8791795052256695
    , -0.4414598402161806
    ]
  , [ 9.361332416424298e-2
    , 1.871978516403293
    , -0.4470200027098403
    ]
  , [ 1.3877787807814457e-17
    , 1.986091229359059
    , -0.2162856218653722
    ]
  , [ 0.0 , 1.8719852675758741 , -0.45661212451945754 ]
  , [ -0.16621776154382692
    , 1.9776053853655182
    , 0.16621776154382692
    ]
  , [ -0.4470200027098403
    , 1.871978516403293
    , 9.361332416424298e-2
    ]
  , [ -0.4414598402161806
    , 1.8791795052256695
    , 4.446110687314339e-2
    ]
  , [ 8.29475648029771e-2 , 2.0282480595325483 , 0.0 ]
  , [ 7.461906659600517e-2
    , 2.0298344402390813
    , 1.6023498781971006e-2
    ]
  , [ 1.6071190233923754e-3
    , 2.0437414787107526
    , -1.607119023392417e-3
    ]
  , [ -2.988199908178335e-3
    , 2.0433955508208292
    , 2.988199908178321e-3
    ]
  , [ -1.607119023392417e-3
    , 2.0437414787107526
    , 1.6071190233924448e-3
    ]
  , [ -0.822872436963358 , 1.5136775995208986 , 0.26151852913476537 ]
  , [ -0.8260665185299888 , 1.4902762907980336 , 0.2884082499027695 ]
  , [ -0.8962126634940768 , 1.0886815800201057 , 0.192823289924143 ]
  , [ -0.8788805908385666 , 1.1814109441614336 , 0.2831308101687341 ]
  , [ -0.8800216675702618 , 1.2593473684859822 , 0.2834784042093155 ]
  , [ -0.8421531448806925
    , 1.0902530151825576
    , 0.36590860085651855
    ]
  , [ 2.7755575615628914e-17
    , 1.547398454632929
    , -0.8390008746485779
    ]
  , [ -9.015875195792439e-2
    , 1.5174415681823774
    , -0.8562836943885955
    ]
  , [ 0.310292980977805 , 0.7803637144380807 , -0.7156034934346982 ]
  , [ 0.3246126189761304 , 0.9080154562527455 , -0.8335353871385288 ]
  , [ 0.2898318932596732 , 0.8768408129798986 , -0.8180448726617308 ]
  , [ 0.21959663365004273
    , 0.9080153576084198
    , -0.8664476852626977
    ]
  , [ 0.15370753430700937 , 0.907458117258076 , -0.8796369493525762 ]
  , [ 0.1228050514414682 , 0.8753822495491729 , -0.8567258893994794 ]
  , [ 0.5725795213922535 , 0.8790071413593896 , -0.6567496895904281 ]
  , [ 0.6097884029414725 , 0.9088511267237063 , -0.65721509905736 ]
  , [ -0.4401190932578934
    , 1.6091658630935881
    , -0.6493805393140207
    ]
  , [ -0.5970610762747673
    , 1.5413389851975459
    , -0.5970610762747673
    ]
  , [ -0.30618459927572234
    , 1.7463793702756347
    , -0.5599378049841232
    ]
  , [ -0.47877142994686667
    , 1.6423746127113557
    , -0.5796425375805215
    ]
  , [ -0.40683581887655035 , 1.74614655744181 , -0.4921333979229422 ]
  , [ -0.5548468717518389
    , 1.6090000296403644
    , -0.5548468717518389
    ]
  , [ -0.34656596334419265
    , 1.7617035212112468
    , -0.512501037698028
    ]
  , [ -0.4378291627777074
    , 1.7613180604656151
    , -0.43782916277770734
    ]
  , [ -0.21242895637465672
    , 1.9790924197542168
    , -9.226906344799235e-2
    ]
  , [ -0.39238324994576507
    , 1.8778272701001337
    , -0.21271144734580563
    ]
  , [ -0.19692078525583517
    , 1.9867703444520366
    , -8.589118029151296e-2
    ]
  , [ -0.4204153875105169
    , 1.8716132855102106
    , -0.18017138946943928
    ]
  , [ 0.12443603223298116 , 1.763801627319081 , -0.6031237664636153 ]
  , [ 0.18017138946943925
    , 1.8716132855102106
    , -0.42041538751051694
    ]
  , [ 6.613605971990155e-2
    , 1.7466998928928852
    , -0.6342820077896054
    ]
  , [ 0.13411685349983243 , 1.878463096721167 , -0.42437151237708 ]
  , [ 0.0 , 1.6095711978718583 , -0.7838769658257825 ]
  , [ -0.15978142628509895
    , 1.6096835638600524
    , -0.7673351760829068
    ]
  , [ 0.0 , 1.764488100164783 , -0.6148766884290562 ]
  , [ -4.4461106873143405e-2
    , 1.8791795052256695
    , -0.4414598402161806
    ]
  , [ -7.606128724075058e-2
    , 2.028192844530145
    , 3.459146906505172e-2
    ]
  , [ -0.21397126895807833 , 1.9871467005290857 , 0.0 ]
  , [ -8.29475648029771e-2 , 2.0282480595325483 , 0.0 ]
  , [ -7.461906659600517e-2
    , 2.0298344402390813
    , 1.6023498781971006e-2
    ]
  , [ -7.461906659600517e-2
    , 2.0298344402390813
    , -1.6023498781971006e-2
    ]
  , [ -0.21397126895807833 , 1.987146700529086 , 0.0 ]
  , [ -0.2162856218653722
    , 1.9860912293590587
    , -4.293150814092422e-18
    ]
  , [ -1.6023498781971048e-2
    , 2.0298344402390818
    , -7.461906659600523e-2
    ]
  , [ 0.0 , 2.0282480595325483 , -8.294756480297713e-2 ]
  , [ -1.607119023392417e-3
    , 2.0437414787107526
    , -1.6071190233923754e-3
    ]
  , [ -3.459146906505177e-2
    , 2.028192844530145
    , -7.606128724075062e-2
    ]
  , [ -8.352331765507041e-2
    , 1.9874788959388558
    , -0.19585993121305537
    ]
  , [ -4.433393286244257e-2
    , 2.0294496432112377
    , -6.47636067120253e-2
    ]
  , [ 1.6071190233924448e-3
    , 2.0437414787107526
    , 1.6071190233924448e-3
    ]
  , [ 2.988199908178335e-3
    , 2.0433955508208292
    , -2.988199908178363e-3
    ]
  , [ 4.4333932862442554e-2
    , 2.029449643211237
    , -6.476360671202536e-2
    ]
  , [ 5.957385788325471e-2
    , 2.0280904764199947
    , -5.95738578832547e-2
    ]
  , [ -0.8457844093935878
    , 1.2970587028449376
    , 0.36541277208493106
    ]
  , [ -0.8102438227139426 , 1.2598574705147234 , 0.4474984305598345 ]
  , [ -0.8091216099094026 , 1.1765076558719514 , 0.4468960748716195 ]
  , [ -0.7119923208428118 , 1.1741465971067808 , 0.5903677161648285 ]
  , [ 0.18667466541971134
    , 0.5968790484510273
    , -0.5717139272637516
    ]
  , [ 1.7990621802653206e-2
    , 1.986082558914795e-2
    , -1.7990621802653095e-2
    ]
  , [ 0.2396999180870924 , 0.8768406254951663 , -0.8337562524679073 ]
  , [ 0.16241732047101426
    , 0.7759039565328367
    , -0.7575092969305981
    ]
  , [ -0.6987516862837264
    , 1.5418687589859696
    , -0.4731627371704971
    ]
  , [ -0.5796425375805216
    , 1.6423746127113557
    , -0.4787714299468666
    ]
  , [ -0.4921333979229422 , 1.74614655744181 , -0.40683581887655035 ]
  , [ -0.6493805393140207
    , 1.6091658630935883
    , -0.44011909325789345
    ]
  , [ -0.16621776154382692
    , 1.9776053853655182
    , -0.16621776154382692
    ]
  , [ -0.28526260180018986
    , 1.877513818092077
    , -0.34408768727411854
    ]
  , [ -0.15197602557430878
    , 1.9868541707659964
    , -0.15197602557430875
    ]
  , [ -0.34408768727411854
    , 1.8775138180920767
    , -0.28526260180018986
    ]
  , [ -0.2571184603540509
    , 1.8714260967581884
    , -0.3788554436946959
    ]
  , [ -0.3239549649324688 , 1.871257263189551 , -0.3239549649324688 ]
  , [ -0.378855443694696
    , 1.8714260967581884
    , -0.25711846035405095
    ]
  , [ -0.5599378049841232
    , 1.746379370275635
    , -0.30618459927572234
    ]
  , [ -0.512501037698028
    , 1.7617035212112468
    , -0.34656596334419265
    ]
  , [ -0.6590455307424883
    , 1.643431115641555
    , -0.35938264373453305
    ]
  , [ -0.18017138946943928
    , 1.8716132855102106
    , -0.4204153875105169
    ]
  , [ -0.13411685349983246 , 1.878463096721167 , -0.42437151237708 ]
  , [ -0.2127114473458056
    , 1.8778272701001337
    , -0.39238324994576507
    ]
  , [ -0.2414067196592598 , 1.7624263437337935 , -0.568522694152947 ]
  , [ -0.1921261597237747
    , 1.7467019433064042
    , -0.6080853167177859
    ]
  , [ -9.226906344799232e-2
    , 1.979092419754217
    , -0.21242895637465659
    ]
  , [ -0.19585993121305542
    , 1.9874788959388558
    , -8.352331765507048e-2
    ]
  , [ -7.606128724075059e-2
    , 2.028192844530145
    , -3.459146906505173e-2
    ]
  , [ 9.226906344799235e-2
    , 1.979092419754217
    , -0.21242895637465672
    ]
  , [ 0.2127114473458056 , 1.877827270100134 , -0.3923832499457651 ]
  , [ 8.589118029151292e-2
    , 1.9867703444520366
    , -0.19692078525583512
    ]
  , [ 0.25711846035405095 , 1.8714260967581884 , -0.378855443694696 ]
  , [ 0.15197602557430875
    , 1.9868541707659966
    , -0.15197602557430878
    ]
  , [ 8.35233176550705e-2
    , 1.9874788959388558
    , -0.19585993121305542
    ]
  , [ 3.459146906505177e-2
    , 2.028192844530145
    , -7.606128724075062e-2
    ]
  , [ 0.34408768727411854
    , 1.877513818092077
    , -0.28526260180018986
    ]
  , [ 0.43782916277770734
    , 1.7613180604656151
    , -0.4378291627777074
    ]
  , [ -7.668380969626697e-2
    , 1.6481991362041954
    , -0.7417373916617902
    ]
  , [ -0.1244360322329812 , 1.763801627319081 , -0.6031237664636153 ]
  , [ -6.613605971990157e-2
    , 1.746699892892885
    , -0.6342820077896053
    ]
  , [ -9.361332416424303e-2
    , 1.871978516403293
    , -0.4470200027098403
    ]
  , [ -1.6071190233924448e-3
    , 2.0437414787107526
    , 1.6071190233924448e-3
    ]
  , [ 1.602349878197104e-2
    , 2.0298344402390813
    , -7.461906659600523e-2
    ]
  , [ 1.6071190233924448e-3
    , 2.0437414787107526
    , -1.6071190233924448e-3
    ]
  , [ 4.163336342344337e-17
    , 1.9871467005290857
    , -0.2139712689580784
    ]
  , [ 1.3877787807814457e-17
    , 1.9871467005290855
    , -0.2139712689580783
    ]
  , [ -8.589118029151295e-2
    , 1.9867703444520366
    , -0.19692078525583515
    ]
  , [ -0.15197602557430875
    , 1.9868541707659964
    , -0.15197602557430875
    ]
  , [ -0.7590404339629725 , 1.0903982826657406 , 0.518569427022487 ]
  , [ -0.7423359031749055 , 0.9128065261969956 , 0.5073894465820623 ]
  , [ -5.957385788325474e-2
    , 2.0280904764199947
    , -5.957385788325473e-2
    ]
  , [ -6.476360671202533e-2
    , 2.0294496432112377
    , -4.433393286244257e-2
    ]
  , [ -2.988199908178335e-3
    , 2.0433955508208292
    , -2.988199908178321e-3
    ]
  , [ -1.6071190233924448e-3
    , 2.0437414787107526
    , -1.6071190233924448e-3
    ]
  , [ 0.16621776154382692
    , 1.9776053853655182
    , -0.16621776154382692
    ]
  , [ 0.2852626018001899 , 1.8775138180920772 , -0.3440876872741186 ]
  , [ 0.3239549649324688 , 1.871257263189551 , -0.3239549649324688 ]
  , [ 0.0 , 0.0 , 0.0 ]
  , [ 2.7755575615628914e-17 , 2.0440475966384333 , 0.0 ] ]
