# qhull

Delaunay triangulation, Voronoi diagrams and convex hulls.
Based on the `qhull` C library.

## Delaunay tesselation

Consider this list of vertices (actually these are the vertices of a
polyhedron):

```haskell
vertices = [
            [ -5, -5,  16 ]  -- 0
          , [ -5,  8,   3 ]  -- 1
          , [  4, -1,   3 ]  -- 2
          , [  4, -5,   7 ]  -- 3
          , [  4, -1, -10 ]  -- 4
          , [  4, -5, -10 ]  -- 5
          , [ -5,  8, -10 ]  -- 6
          , [ -5, -5, -10 ]  -- 7
                           ]
```

The `delaunay` function splits the polyhedron into simplices, the tiles of the
tesselation:

```haskell
> d <- delaunay vertices False
> _tiles d
fromList
  [ ( 0
    , Tile
        { _simplex =
            Simplex
              { _points =
                  fromList
                    [ ( 2 , [ 4.0 , -1.0 , 3.0 ] )
                    , ( 4 , [ 4.0 , -1.0 , -10.0 ] )
                    , ( 5 , [ 4.0 , -5.0 , -10.0 ] )
                    , ( 7 , [ -5.0 , -5.0 , -10.0 ] )
                    ]
              , _circumcenter =
                  [ -0.5000000000000009 , -3.0 , -3.499999999999999 ]
              , _circumradius = 8.154753215150047
              , _normal =
                  [ -7.03638769136053e-2
                  , -0.4221832614816321
                  , -0.4925471383952373
                  , -0.7577648283003651
                  ]
              , _offset = 1.3369136613585004
              , _volume = 78.0
              }
        , _neighborsIds = fromList [ 1 , 3 ]
        , _facetsIds = fromList [ 0 , 1 , 2 , 3 ]
        , _family = Nothing
        , _toporiented = False
        }
    )
  , ( 1
    , Tile
        { _simplex =
  ......
```

The field `_tiles` is a map of `Tile` objects. The keys of the map are
the tiles identifiers. A `Tile` object has five fields:

-   `_simplex`, a `Simplex` object;

-   `_neighborsIds`, a set of tiles identifiers, the neighbors of the tile;

-   `facetsIds`, a set of facets identifiers, the facets of the tile;

-   `family`, xxxxx

-   `toporiented`, the orientation of the tile.

A `Simplex` object has six fields:

-   `_points`, the vertices of the simplex, actually a map of the vertices
identifiers to their coordinates

-   `_circumcenter`, the coordinates of the circumcenter of the simplex;

-   `_circumradius`, the circumradius;

-   `_normal`, the coordinates of the normal of the simplex;

-   `_offset`, the offset of the simplex;

-   `_volume`, the volume of the simplex (the area in dimension 2, the
  length in dimension 1).

Another field of the output of `delaunay` is `_tilefacets`:

```haskell
> _tilefacets d
fromList
  [ ( 0
    , TileFacet
        { _subsimplex =
            Simplex
              { _points =
                  fromList
                    [ ( 4 , [ 4.0 , -1.0 , -10.0 ] )
                    , ( 5 , [ 4.0 , -5.0 , -10.0 ] )
                    , ( 7 , [ -5.0 , -5.0 , -10.0 ] )
                    ]
              , _circumcenter = [ -0.5000000000000009 , -3.0 , -10.0 ]
              , _circumradius = 4.924428900898053
              , _normal = [ 0.0 , 0.0 , -1.0 ]
              , _offset = -10.0
              , _volume = 36.0
              }
        , _facetOf = fromList [ 0 ]
        }
    )
  , ( 1
    , TileFacet
        { _subsimplex =
  ......
```

This is a map of `TileFacet` objects. A tile facet is a subsimplex. The keys of
the map are the identifiers of the facets.
A `TileFacet` object has two fields: `_subsimplex`, a `Simplex` object, and
`_facetOf`, the identifiers of the tiles this facet belongs to (a set of one
or two integers).

Finally, the output of `delaunay` has a `_sites` field, the vertices with
additional information:

```haskell
> _sites d
fromList
  [ ( 0
    , Site
        { _point = [ -5.0 , -5.0 , 16.0 ]
        , _neighsitesIds = fromList [ 1 , 3 , 7 ]
        , _neighfacetsIds = fromList [ 15 , 16 , 17 ]
        , _neightilesIds = fromList [ 5 ]
        }
    )
  , ( 1
    , Site
  ......
```

This is a map of `Site` objects. The keys of the map are the identifiers of
the vertices. A `Site` object has four fields:

-   `_point`, the coordinates of the vertex;

-   `_neighsitesIds`, the identifiers of the connected vertices;

-   `_neighfacetsIds`, a set of integers, the identifiers of the facets the
vertex belongs to;

-   `_neightilesIds`, the set of the identifiers of the tiles the vertex belongs
to.

[![rgg.gif](https://s13.postimg.org/6h1r72k3r/rgg.gif)](https://postimg.org/image/ojutyafyb/)


## Voronoi diagrams

The library allows to get the Voronoi diagram of a list of sites (vertices)
from the Delaunay tesselation. Here is a 3D example.

```haskell
centricCuboctahedron :: [[Double]]
centricCuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
                       [[i,0,j] | i <- [-1,1], j <- [-1,1]] ++
                       [[0,i,j] | i <- [-1,1], j <- [-1,1]] ++
                       [[0,0,0]]
import Delaunay
import Voronoi3D
d <- delaunay centricCuboctahedron False
v = voronoi3 d
```

The output of `voronoi3` is a list of Voronoi cells given as pairs, each pair
consisting of a site and a list of edges.
This is the cell of the center `[0,0,0]`:

```haskell
> last v
( [ 0.0 , 0.0 , 0.0 ]
, [ Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , 0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , 0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , 0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , 0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , 0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , 0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , -0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , -0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , -0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , -0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , -0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , -0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  ]
)
```

This is a bounded cell: it has finite edges only. The other ones are not
bounded, they have infinite edges:

```haskell
> head v
( [ -1.0 , -1.0 , 0.0 ]
, [ Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , IEdge3
      ( ( -0.5 , -0.5 , 0.5 )
      , ( -0.5773502691896258
        , -0.5773502691896258
        , 0.5773502691896258
        )
      )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , IEdge3
      ( ( -0.5 , -0.5 , -0.5 )
      , ( -0.5773502691896258
        , -0.5773502691896258
        , -0.5773502691896258
        )
      )
  , IEdge3 ( ( -1.0 , 0.0 , 0.0 ) , ( 1.0 , 0.0 , 0.0 ) )
  , IEdge3 ( ( 0.0 , -1.0 , 0.0 ) , ( 0.0 , -1.0 , 0.0 ) )
  ]
)
```

[![voronoi_cuboctahedron.gif](https://s13.postimg.org/6e0vngu1j/voronoi_cuboctahedron.gif)](https://postimg.org/image/ceykkjgnn/)


## Convex hull

```haskell
import ConvexHull
```

[![convexhull02.gif](https://s17.postimg.org/zbl78b1bz/convexhull02.gif)](https://postimg.org/image/80zw0dyez/)
